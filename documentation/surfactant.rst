Experimental surfactant and lymphatic coupling
==============================================

Status and provenance
---------------------

This opt-in implementation adapts equations and run protocols from Ruobing's
``ven_surf`` commit ``a681432`` (surfactant-only) and ``Lym_surf`` commit
``143d819`` (lymphatic/surfactant). The latter is identical to the previously
imported ``lym_latest`` snapshot.

It is experimental, is not the published Ashworth standalone lymphatic solver,
and is not a validated human or rat calibration. Software tests do not establish
physiological validity.

API and repository separation
-----------------------------

Existing ``evaluate_vent`` and ``lymphatic_transport`` calls are unchanged. The
new entry point is ``evaluate_vent_coupled(output_stem, model)``, where ``model``
must be ``surfactant`` or ``lymphatic_surfactant``. For the combined mode, call
``import_coupled_capillary(filename)`` first. After solving, use
``export_terminal_coupled`` for standard exnode output and optionally
``export_coupled_csv`` for analysis.

LungSim contains equations, parameters, import/export routines and bindings.
The runnable Python workflow, documentation and Python integration tests live in
``lung-group-examples/surfactant_Ruobing2024``. Native equation and stopping-rule
tests remain in LungSim.

All Ruobing-specific surface, fluid/protein and coupling routines are contained
in ``src/lib/surfactant.f90``. The file is divided internally into surface
constitutive equations, fluid/protein transport, and coupling orchestration.
The published standalone model remains separately implemented in
``src/lib/lymphatics.f90``.

Explicit protocols
------------------

``surfactant`` runs dynamic ventilation with surface concentration, tension,
recoil and compliance active, then performs the final inspiration used by
``ven_surf``. It performs no fluid/protein work. Fluid outputs are zero and
``stop_status`` is -1.

``lymphatic_surfactant`` restores the staged ``Lym_surf`` structure:

#. run ventilation, surfactant and fluid/protein transport together;
#. freeze ventilation and settle fluid using the established recoil range;
#. stop terminals after saturation convergence plus minimum active time, or at
   the maximum transit-time limit;
#. stop the stage at its global duration limit if necessary;
#. reset surface state, freeze fluid, and run five surfactant breaths by default;
#. run a final inspiration; only still-eligible fluid units may advance.

The source allows 10000 breathing periods for settling and 5000 transit times
per terminal, with two fluid substeps per ventilation step. A full tree therefore
requires billions of terminal updates, explaining the reported 2.5--3 hour
runtime. Surfactant-only performs none of this work.

Capillary mapping
-----------------

Combined input has four whitespace-separated columns; blank/comment lines are
allowed::

   # airway_terminal_element  mean_capillary_pressure_Pa  sheet_area_mm2  transit_time_s
   2  2100.0  200.0  0.75

Provide every airway terminal exactly once. Rows can be shuffled. Duplicate,
missing, nonterminal, non-finite or negative values are rejected. Positive area
requires positive transit time; zero area/transit marks an excluded terminal.

The importer does not guess correspondence between personalised airway and
vascular trees. Use mean capillary pressure, not terminal arterial pressure.
``micro_flow_unit.out`` and exnode files are not this mapping format.

Parameters
----------

``update_surfactant`` exposes source surface-law inputs: ``gamma_star`` (3e-7
g/cm2), clean/hat/minimum tension (70/22/1 dyn/cm), ``m2`` (140 dyn/cm),
adsorption/desorption (1667 mL/(g s), 0.01667 1/s), normal/flooded bulk
concentration (0.01/0.0001 g/mL), initial ratio (0.5), and
``alveoli_per_unit`` (37700).

Important ``update_coupled_lymphatics`` defaults are:

.. list-table::
   :header-rows: 1

   * - Parameter
     - Default
     - Meaning
   * - lung_mass_g
     - 840
     - Source male preset, independent of standalone default
   * - pressure_multiplier / conductivity_multiplier
     - 2 / 2
     - Source experimental scaling, not normal-human reference values
   * - minimum_transit_times / maximum_transit_times
     - 200 / 5000
     - Per-terminal active-time rules
   * - saturation_tolerance
     - 1e-5
     - Current saturation versus five-sample mean
   * - convergence_check_steps
     - 1333
     - Fluid calls between samples, matching the source
   * - maximum_settling_breaths
     - 10000
     - Global settling limit in breathing periods
   * - surfactant_equilibration_breaths
     - 5
     - Frozen-fluid breaths after settling
   * - fluid_substeps
     - 2
     - Fluid steps per ventilation step

Protein/oncotic defaults are reflection 0.24, permeability 4.5e-7 (inferred
mm/s; calibration unverified), convection fraction 0.01555, plasma/interstitial
protein 70/45 mg/mL, and oncotic coefficients 0.157/0.0032. The empirical
intercompartment divisor is 200 and initial A-volume fraction is 0.000005.

Relevant ``update_lymphatics`` values are reused: capillary conductivity,
capacity, compartment sizes/saturation, pressure limits, lymphatic area ratio,
conductivity threshold/polynomial and phase. Standalone mass, integration step
count, convergence tolerance, test time, integrity and reflection are not used.

Outputs and stop status
-----------------------

``<stem>.exflow`` is the ventilation history. ``<stem>.opcoupled`` logs stages,
absolute time and counts for running, converged, transit-capped, global-capped
and excluded terminals. Terminal results contain concentration, tension, surface
pressure/compliance, fluid/protein, mapped inputs, average flows, saturation
error and ``stop_status``:

* -1: surfactant-only;
* 1: saturation convergence after minimum active time;
* 2: terminal transit-time cap;
* 3: global settling cap;
* 4: excluded zero-area/zero-transit terminal.

Statuses 2 and 3 are limits, not physiological equilibrium.

Corrections and remaining differences
-------------------------------------

The staging is source-aligned but output is not bit-for-bit:

* explicit validated modes replace source commenting;
* parameters use ``parameter_types`` and paths are not hard-coded;
* inputs/outputs use public LungSim modules;
* capillary state is separate from shared indices, avoiding pressure aliasing;
* respiratory phase follows ``T_interval`` instead of hard-coded 0.25 Hz;
* surfactant stepping is bounded/substepped for stability;
* fluid/protein transfer is donor-limited and mass-conserving;
* shadowed arrays, uninitialised overflow values, repeated-run allocation,
  fixed unit 19272 diagnostics and unsafe Python array interfaces are removed;
* surface state is consistently initialised at stage boundaries;
* stopped terminals cannot reactivate, and endpoint time does not overshoot by
  one outer step.

These repairs can change flooding results. Protein permeability units and the
empirical exchange divisor still need scientific audit. Quantitative use requires
Ruobing's original mapping/reference output and timestep/duration sensitivity.
