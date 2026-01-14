# Queue Generation Configs

Static configuration files for mass spectrometry queue generation.

| File | Format | Purpose |
|------|--------|---------|
| `samples.csv` | CSV | QC sample definitions (per technology) |
| `instruments.csv` | CSV | Instrument definitions (per technology) |
| `instrument_patterns.csv` | CSV | Available patterns per instrument |
| `combinations.csv` | CSV | Valid instrument + sampler + output_format |
| `sampler.toml` | TOML | Physical sampler layouts (Vanquish, MClass48, Evosep) |
| `queue_patterns.toml` | TOML | Injection patterns (start/middle/end sequences) |
| `qc_layouts.toml` | TOML | QC positions per technology/sampler |
| `output_formats.toml` | TOML | Output column mappings (xcalibur, chronos, hystar) |
| `methods/` | CSV | Available methods per technology/instrument |

See `docs/` for detailed documentation.
