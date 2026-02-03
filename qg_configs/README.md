# Queue Generation Configs

Static configuration files for mass spectrometry queue generation.

| File | Format | Purpose |
|------|--------|---------|
| `samples.csv` | CSV | QC sample definitions (per tech_area) |
| `instruments.csv` | CSV | Instrument definitions (per tech_area) |
| `instrument_patterns.csv` | CSV | Available patterns per instrument |
| `combinations.csv` | CSV | Valid instrument + sampler + output_format |
| `sampler.toml` | TOML | Physical sampler layouts (Vanquish, MClass48, Evosep) |
| `queue_patterns.toml` | TOML | Injection patterns (start/middle/end sequences) |
| `qc_layouts.toml` | TOML | QC positions per tech_area/sampler |
| `output_formats.toml` | TOML | Output column mappings (xcalibur, chronos, hystar) |
| `methods/` | CSV | Available methods per tech_area/instrument |

See `docs/` for detailed documentation.
