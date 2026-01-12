# Config Todo

## Done
- [x] sample.toml to csv (now samples.csv)
- [x] sample_id into default sample pattern: `{date}_{run}_C{container}_S{sample_id}_{sample_name}`
- [x] polarity into sample pattern: `{polarity}` suffix for metabolomics/lipidomics

## Clarified
- Tube ID: NOT injected into file names (legacy R code uses Sample ID only)
  - File name template uses `S{sample_id}` not tube_id
  - Tube ID exists in B-Fabric input but is not part of file naming

## Todo
- [ ] Prototype implementation of queue file generation
- [ ] Sampler-specific code (Evosep position handling differs from grid samplers)
- [ ] Polarity handling in queue generation (expand `{polarity}` to `pos`/`neg`)

