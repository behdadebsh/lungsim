Experimental coupled surfactant model
====================================

Status and provenance
---------------------

This opt-in implementation adapts the constitutive equations from
`Ruobing-rl/lungsim, Lym_surf, commit 143d819
<https://github.com/Ruobing-rl/lungsim/tree/143d8192def1938e6eb781c451d411ba825bb0a6>`_.
That commit is also the tip of the previously imported ``lym_latest`` snapshot.
It combines surfactant adsorption/desorption, surface tension, alveolar recoil,
and two-compartment lymphatic fluid/protein transport. It is an experimental
model, not the published Ashworth solver and not a validated species-specific
calibration. Successful software tests do not establish physiological validity.

Existing ``evaluate_vent`` and ``lymphatic_transport`` calls retain their existing
behaviour and defaults. Perfusion, capillary calculations, shared field indices,
and existing exnode exports are not changed. New state lives outside
``unit_field`` so terminal pressure and mean capillary pressure cannot alias.

Python entry points
-------------------

Set up an airway tree using ``ventilation_indices()``, geometry imports, radii,
and ``append_units()`` as for ordinary ventilation. Then choose one of::

    from aether.ventilation import evaluate_vent_coupled
    from aether.parameter_types import (
        update_surfactant, update_coupled_lymphatics, update_solve_v,
        update_ventilation,
    )

    update_solve_v('number_of_breaths', 10)
    update_solve_v('dt', 0.01)
    update_ventilation('t_interval', 4.0)

    # Surfactant mechanics only: no capillary input or fluid transport.
    evaluate_vent_coupled('output/surface_only', '')

    # Or ventilation + surfactant + fluid/protein transport:
    evaluate_vent_coupled('output/coupled', 'inputs/mapped_capillary.txt')

The output directory must already exist. Each call starts fresh coupled state
and releases it on completion. It does not resume a previous fluid solution.
The number of breaths is a duration, not an equilibrium criterion: the coupled
run does not stop early when tidal volume converges. Check duration and timestep
convergence for the application; ten breaths need not equilibrate lymphatics.
Breathing phase uses ``T_interval``; the standalone lymphatic
``breathing_rate_bpm`` does not control this mode.

Input mapping
-------------

``mapped_capillary.txt`` has four whitespace-separated columns, no header
(blank lines and lines starting with ``#`` are allowed)::

    # local_airway_terminal_element  mean_capillary_pressure_Pa  sheet_area_mm2  transit_time_s
    2  2100.0  200.0  0.75
    3  1500.0  100.0  0.50

These illustrative numbers describe the three-element software-test tree, not
a physiological reference case. Provide exactly one record per airway terminal.
Records can be in any order. The first column is the **local airway terminal
element number**, not node number, capillary element number, global element label
or row position. Duplicate, missing, nonterminal and out-of-range records are
rejected, as are non-finite values or negative areas/transit times. Zero area is
allowed for a non-exchanging unit; signed pressure is a gauge pressure.

Map perfusion capillary results to airway terminals explicitly. Use the mean
capillary pressure, not the terminal artery pressure. Do not pass
``micro_flow_unit.out`` or a lymphatic exnode directly: their formats and element
numbering differ. This interface intentionally does not guess a correspondence
between independently personalised airway and vascular trees. Transit time is
retained for reporting; it does not control the new fixed-duration integration.

Parameters
----------

All new parameters are declared in ``src/lib/parameter_types.f90`` and updated
through the same string/value Python convention as ``update_lung``. Set them
before starting a run. Unknown names are rejected; invalid/non-finite values
are checked when initialising the coupled solver.

``update_surfactant(name, value)`` accepts:

.. list-table:: Source surfactant defaults
   :header-rows: 1

   * - Name
     - Default
     - Units / meaning
   * - gamma_star
     - 3e-7
     - g/cm², concentration at the tension-law breakpoint
   * - tension_clean / tension_hat / tension_min
     - 70 / 22 / 1
     - dyn/cm
   * - m2
     - 140
     - dyn/cm, slope against normalised concentration
   * - adsorption_rate
     - 1667
     - mL/(g s)
   * - desorption_rate
     - 0.01667
     - 1/s; independent of adsorption_rate when updated
   * - bulk_normal / bulk_flooded
     - 0.01 / 0.0001
     - g/mL
   * - initial_gamma_ratio
     - 0.5
     - fraction of gamma_star
   * - alveoli_per_unit
     - 37700
     - alveoli represented by one terminal unit

``gamma_max`` is derived as
``gamma_star * (1 + (tension_hat - tension_min) / m2)``. Alveoli are hemispheres;
the source's volume divisor 37700000 becomes the explicit product of 1000
mm³/cm³ and ``alveoli_per_unit``. Any positive accumulated alveolar fluid selects
``bulk_flooded``. No continuous dilution or alveolar fluid clearance is added.

``update_coupled_lymphatics(name, value)`` accepts:

.. list-table:: Source experimental fluid/protein defaults
   :header-rows: 1

   * - Name
     - Default
     - Units / meaning
   * - lung_mass_g
     - 840
     - g; source's male preset, independent of standalone default 639 g
   * - pressure_multiplier / conductivity_multiplier
     - 2 / 2
     - Source experimental scaling; **not normal-human reference values**
   * - protein_reflection
     - 0.24
     - Source expression 0.6 × 0.4
   * - protein_permeability
     - 4.5e-7
     - mm/s inferred from the implemented equation; calibration unverified
   * - protein_convection_fraction
     - 0.01555
     - Source convective protein fraction
   * - plasma_protein / initial_interstitial_protein
     - 70 / 45
     - mg/mL
   * - oncotic_linear / oncotic_quadratic
     - 0.157 / 0.0032
     - Coefficients of pressure (mmHg) versus concentration (mg/mL)
   * - initial_a_fraction
     - 0.000005
     - Initial A volume / total interstitial capacity
   * - exchange_resistance
     - 200
     - Empirical saturation-driven exchange divisor; inferred s/mm³
   * - minimum_volume
     - 1e-10
     - mm³; donor-volume floor
   * - fluid_substeps
     - 2
     - Fluid steps per ventilation step, rounded to an integer

For an experiment without the source's doubled loading, explicitly use::

    update_coupled_lymphatics('pressure_multiplier', 1.0)
    update_coupled_lymphatics('conductivity_multiplier', 1.0)

The coupled solver also reads these existing ``update_lymphatics`` parameters:
capillary hydraulic conductivity, interstitial capacity per 100 g, compartment A
fraction, initial interstitial saturation (sets B volume / total capacity), both
interstitial and lymphatic pressure limits, lymphatic density/surface-area ratio,
the conductivity threshold/baseline/six polynomial coefficients, and phase offset.
It does not use the standalone mass, integration_steps_per_transit, convergence
tolerance, test_time, integrity or reflection_coefficient. Use the new
``protein_reflection`` for the active protein/oncotic model.

Outputs
-------

``evaluate_vent_coupled('output/name', ...)`` writes the usual ``name.exflow``
and a separate ``name.coupled.csv``. Existing ventilation exports remain usable.
The CSV contains one row per terminal with explicit unit-labelled columns:
surfactant concentration, tension, surface recoil pressure/compliance, accumulated
alveolar fluid, interstitial saturation (fraction, not percent), elapsed fluid
time, mean capillary filtration and mean lymph drainage, original and scaled
capillary pressures, sheet area/transit time, and retained/drained protein amounts.
Fluid volumes are mm³, flows mm³/s and protein amounts mg. Surfactant-only runs
report zero fluid fields. Filtration and drainage are cumulative volumes divided
by elapsed fluid time, not the final instantaneous rates.

Differences from the source experiment
--------------------------------------

This is a selective integration, **not a bit-for-bit reproduction** of Lym_surf:

* Its hard-coded staged schedule (ventilation, fluid-only settling, five
  surfactant breaths, then a half-breath) is replaced by an explicit fixed-duration
  coupled run. Surface state is initialised consistently before the first
  compliance evaluation. Fluid advances after ventilation; its flooding state
  affects surfactant in the following step. Recoil extrema span the run, as in
  the source; they are not reset every breath.
* The original hard-coded 0.25 Hz oscillation now follows ``T_interval``.
* Surface adsorption and area changes are internally substepped and concentration
  is bounded between zero and gamma_max. This changes large-step Euler results.
* Initialisation, repeat-call cleanup, uninitialised overflow variables, and
  shadowed accumulator arrays are corrected. No unsafe assumed-shape array is
  passed through the C/Python interface.
* Donor-limited fluid/protein transfers preserve mass. Overflow from either
  interstitial compartment's capillary loading is accounted for; protein follows
  donor concentration. Protein is stored in mg using explicit mm³/mL conversion.
  These repairs can change flooding results compared with the source's clipping
  and post-transfer concentration calculations.
* Plasma concentration remains a fixed reservoir, as in the active source
  equations. No dynamic plasma depletion model is claimed.
* The source's additive recoil and reciprocal-compliance rule is retained. Its
  ``3V/P`` surface compliance is a modelling prescription, not the complete
  derivative of a dynamic surfactant law. Coupling it to the tissue law may need
  recalibration to avoid double-counting baseline surface effects.
* No subject-specific output paths, unit 19272 diagnostics, unrelated wave/
  gas-exchange changes, or unfinished ``evaluate_surf`` stub are imported.

Before quantitative use, compare with Ruobing's original inputs, staged protocol
and expected outputs, audit the protein/oncotic calibration and units, and test
timestep/duration sensitivity. Rat use additionally requires an appropriate lung
mass, alveoli-per-terminal interpretation, breathing period and mapped capillary
inputs; the defaults do not establish rat validity.

Software tests
--------------

The native ``Aether/surfactant_test`` checks constitutive-law breakpoints,
pressure/compliance units, flooding response, deterministic stepping, fluid and
protein mass balance, mapped pressure identity, state reset and parameter updates.
``tests/smoke_coupled.py`` exercises the installed Python bindings on a synthetic
three-element tree. It is registered as ``Aether/coupled_python_test`` when
bindings and native tests are enabled, and also runs in the build matrix after
package installation. These are software checks, not physiological reference results.
