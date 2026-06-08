# Queue Generation Configs

Static configuration files for mass spectrometry queue generation.

Files live under `core/{structure,position,formatting,methods}/` and `ui/`:

| File | Location | Format | Purpose |
|------|----------|--------|---------|
| `samples.csv` | `core/structure/` | CSV | QC sample definitions (per tech_area) |
| `queue_patterns.toml` | `core/structure/` | TOML | Injection patterns (start/middle/end/separation) |
| `sampler.toml` | `core/position/` | TOML | Physical sampler layouts (Vanquish, MClass, Evosep) |
| `plate_layouts.toml` | `core/position/` | TOML | Plate layout definitions (rows × cols) |
| `sampler_plate_layouts.csv` | `core/position/` | CSV | Sampler → plate-layout mapping (with queue_type) |
| `qc_layouts_well.csv` | `core/position/` | CSV | QC positions for well-plate samplers |
| `qc_layouts_tip.csv` | `core/position/` | CSV | QC tip ranges for tip-plate samplers (Evosep) |
| `instruments.csv` | `core/formatting/` | CSV | Instrument definitions: methods_file, path_template |
| `output_formats.toml` | `core/formatting/` | TOML | Output column mappings (xcalibur, chronos, hystar) |
| `methods/<Tech>/<instr>_methods.csv` | `core/methods/` | CSV | Available methods per tech_area/instrument |
| `instrument_config.csv` | `ui/` | CSV | Instrument defaults: sampler, output_format, default_pattern |

See [`docs/reference/config.md`](../docs/reference/config.md) for the detailed per-file reference.
