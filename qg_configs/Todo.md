# Config Todo

## Done
- [x] sample.toml to csv (now samples.csv)
- [x] sample_id into default sample pattern: `{date}_{run}_C{container}_S{sample_id}_{sample_name}`
- [x] polarity into sample pattern: `{polarity}` suffix for metabolomics/lipidomics
- [x] Pydantic validation for all configs (validate_config_pydantic.py)
  - CSV: samples, instruments, instrument_patterns, combinations
  - TOML: sampler, queue_patterns, qc_layouts, output_formats
  - Cross-validation: patterns/layouts reference valid samples, combos reference valid instruments

## Clarified
- Tube ID: NOT injected into file names (legacy R code uses Sample ID only)
  - File name template uses `S{sample_id}` not tube_id
  - Tube ID exists in B-Fabric input but is not part of file naming

## Todo
- [ ] Prototype implementation of queue file generation
- [ ] Sampler-specific code (Evosep position handling differs from grid samplers)
  - **Config structure captured in Pydantic models:**
    - Grid samplers (Vanquish, MClass48): `plates`, `rows`, `cols`, `position_format`
    - Evosep: `slots`, `positions_per_slot`, `fill_order="sequential"`
  - **QC positions differ:**
    - Grid: string like `"B:F9"` or `"1:F,8"`
    - Evosep: `{tray, position_start, position_end}` range
  - **Queue generation code needs to:**
    - Generate positions differently (grid row-major vs Evosep sequential)
    - Track Evosep position counters per sample type within range
    - Output different columns (Chronos needs `tray` + `position` separately)

