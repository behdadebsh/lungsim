"""Python-binding smoke checks on a synthetic tree; no physiological validation.

Run with the built aether package on PYTHONPATH. All outputs use a temporary
directory. Each child process starts with clean Fortran geometry/state.
"""
import argparse
import csv
import math
import os
from pathlib import Path
import subprocess
import sys
import tempfile


def run_model(mode, output):
    from aether.diagnostics import set_diagnostics_on
    from aether.geometry import (
        append_units, define_1d_elements, define_node_geometry, define_rad_from_geom,
    )
    from aether.indices import ventilation_indices
    from aether.parameter_types import update_lung, update_solve_v, update_ventilation
    from aether.ventilation import evaluate_vent

    set_diagnostics_on(False)
    ventilation_indices()
    geometry = str(Path(__file__).resolve().parent / "fixtures" / "coupled_tree")
    define_node_geometry(geometry)
    define_1d_elements(geometry)
    define_rad_from_geom("horsf", 1.16, "inlet", 1.0, "all", "")
    append_units()
    update_lung("FRC", 200.0)
    update_lung("cov", 0.0)
    update_lung("rmax", 1.0)
    update_lung("rmin", 1.0)
    update_lung("chest_wall_compliance", 0.136)
    update_ventilation("tidal_volume", 40.0)
    update_solve_v("dt", 0.01)
    update_solve_v("number_of_breaths", 2)
    if mode == "ordinary":
        evaluate_vent(str(output / mode))
        return

    from aether.parameter_types import update_coupled_lymphatics, update_surfactant
    from aether.ventilation import evaluate_vent_coupled

    update_surfactant("alveoli_per_unit", 37700.0)
    update_coupled_lymphatics("lung_mass_g", 0.02)  # synthetic scale, not a species preset
    update_coupled_lymphatics("pressure_multiplier", 1.0)
    capillary = output / "mapped_capillary.txt"
    contents = "# shuffled rows\n3 1500 100 0.50\n2 2100 200 0.75\n"
    if mode == "duplicate":
        contents = "2 2100 200 0.75\n2 1500 100 0.50\n"
    elif mode == "missing":
        contents = "2 2100 200 0.75\n"
    elif mode == "nan":
        contents = "2 NaN 200 0.75\n3 1500 100 0.50\n"
    elif mode == "nonterminal":
        contents = "1 2100 200 0.75\n3 1500 100 0.50\n"
    capillary.write_text(contents)
    evaluate_vent_coupled(str(output / mode), "" if mode == "surface" else str(capillary))
    if mode == "reset":
        # Exercise deallocation and surfactant-only reinitialisation in-process.
        evaluate_vent_coupled(str(output / "after_reset"), "")
        evaluate_vent(str(output / "ordinary_after_reset"))


def read_result(path):
    with path.open(newline="") as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 2
    assert all(math.isfinite(float(value)) for row in rows for value in row.values())
    return rows


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--mode")
    parser.add_argument("--output", type=Path)
    parser.add_argument("--baseline-package", type=Path, help="Optional pre-integration aether package parent")
    args = parser.parse_args()
    if args.mode:
        args.output.mkdir(parents=True, exist_ok=True)
        run_model(args.mode, args.output)
        return

    with tempfile.TemporaryDirectory(prefix="lungsim-coupled-") as directory:
        root = Path(directory)

        def child(mode, output, env=None):
            return subprocess.run(
                [sys.executable, str(Path(__file__).resolve()), "--mode", mode, "--output", str(output)],
                text=True, capture_output=True, env=env, timeout=60,
            )

        for mode in ("ordinary", "surface", "coupled", "reset"):
            result = child(mode, root / mode)
            assert result.returncode == 0, result.stdout + result.stderr
            assert "unknown parameter" not in result.stdout.lower(), result.stdout[:1000]
        surface = read_result(root / "surface/surface.coupled.csv")
        coupled = read_result(root / "coupled/coupled.coupled.csv")
        assert [int(row["terminal_element"]) for row in coupled] == [2, 3]
        assert [float(row["input_cap_pressure_Pa"]) for row in coupled] == [2100.0, 1500.0]
        assert [float(row["effective_cap_pressure_Pa"]) for row in coupled] == [2100.0, 1500.0]
        assert all(abs(float(row["elapsed_fluid_s"]) - 8.0) < 1e-9 for row in coupled)
        assert all(float(row["mean_lymph_mm3_s"]) >= 0 for row in coupled)
        assert all(float(row["elapsed_fluid_s"]) == 0 for row in surface)
        reset = read_result(root / "reset/after_reset.coupled.csv")
        assert all(float(row["alveolar_fluid_mm3"]) == 0 for row in reset)
        assert not (root / "ordinary/ordinary.coupled.csv").exists()
        assert not (root / "reset/ordinary_after_reset.coupled.csv").exists()

        result = child("coupled", root / "repeat")
        assert result.returncode == 0, result.stdout + result.stderr
        assert (root / "repeat/coupled.coupled.csv").read_bytes() == (root / "coupled/coupled.coupled.csv").read_bytes()
        for mode, message in (
            ("duplicate", "Duplicate airway terminal"),
            ("missing", "every airway terminal"),
            ("nan", "Non-finite capillary input"),
            ("nonterminal", "not an airway terminal"),
        ):
            result = child(mode, root / mode)
            assert result.returncode != 0 and message in result.stdout + result.stderr, result.stdout + result.stderr

        if args.baseline_package:
            env = dict(os.environ, PYTHONPATH=str(args.baseline_package.resolve()))
            result = child("ordinary", root / "baseline", env)
            assert result.returncode == 0, result.stdout + result.stderr
            # The legacy initial output row contains pre-existing uninitialised
            # values. Compare every time-advanced result; do not bless that row.
            before = (root / "baseline/ordinary.exflow").read_text().splitlines()[3:]
            after = (root / "ordinary/ordinary.exflow").read_text().splitlines()[3:]
            assert before == after, "Ordinary ventilation differs from baseline"
        print("PASS: Python bindings, surfactant/coupled runs, repeated results, state reset, input mapping and rejection")


if __name__ == "__main__":
    main()
