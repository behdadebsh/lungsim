==============================
Pulmonary lymphatic transport
==============================

The ``lymphatics`` module implements the pulmonary fluid and lymphatic
transport equations published by Ashworth et al. (2023).  It was restored from
the historical EdeTede implementation and integrated with the current
``develop`` data structures without including the later surfactant model.

Model inputs
============

The workflow combines two independently calculated inputs:

* perfusion supplies mean capillary pressure, capillary sheet surface area,
  and blood transit time; and
* ventilation supplies the minimum and maximum terminal elastic recoil
  pressure through ``export_terminal_lymphatic_inputs`` and
  ``import_terminal``.

After perfusion geometry and results have been established, call
``define_problem_type('lymphatic_transport')``.  This preserves the existing
perfusion unit fields while allocating the additional lymphatic fields.  Then
import any archived inputs and call ``lymphatic_transport``.

Output fields
=============

``get_ne_lymph_flux`` and ``get_ne_lymph_intsat`` return the element-field
indices for average flux and interstitial saturation.  These are dedicated
fields: lymphatic transport does not overwrite the unstrained vascular radii.
Terminal results are available from ``export_terminal_lymphatic``.

Reproducibility and provenance
==============================

The equations in ``src/lib/lymphatics.f90`` retain the published
implementation.  Integration fixes are limited to elapsed-time reporting,
safe unit-field resizing, explicit terminal-to-unit matching, and dedicated
lymphatic export fields.  Runtime can vary with processor load; numerical
outputs, rather than the elapsed-time message, should be compared when checking
reproducibility.

The complete Python workflow is maintained in the matching
``lymphatics_Ashworth2023`` example in the lung-group-examples repository.

The source implementation contains a historical note that output units may
need further verification for a possible factor-of-1000 conversion.  That
scientific calibration has not been altered by this restoration and should be
resolved in a separately validated change.
