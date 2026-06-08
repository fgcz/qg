# Queue Generation System

Generate sample queues with QC injections for mass spectrometry instruments
(XCalibur, Chronos, Hystar), with B-Fabric LIMS integration for sample loading
and workunit upload.

The system takes a runtime **Queue Parameters** JSON (instrument, sampler,
samples) plus a set of static **config files** and produces an instrument-ready
sequence file: user samples interleaved with QC injections at pattern-defined
positions, with file names and data paths filled from templates.

## Where to go next

| If you want to… | Read |
|------------------|------|
| Run the app or CLI, understand auth modes | [User modes](users/user_modes.md) |
| Add a QC pattern, sample, or layout via the GUI | [Config editor guide](users/editor_guide.md) |
| Understand the generation pipeline end to end | [Algorithm](reference/algorithm.md) |
| Look up a config file's purpose and schema | [Configuration](reference/config.md) |
| Deploy the app + editor in production | [Deployment](developers/deployment.md) |
| See how the system was built | [History](developers/history.md) |

## Quick start

Installation and the run/CLI quick start are maintained in the project
[README](https://gitlab.bfabric.org/metabolomics/queue-gen/-/blob/main/README.md).
Once installed, the routing table above points you to the right page for each task.

## Core concepts

- **Technology** — `proteomics`, `metabolomics`, or `lipidomics`. Metabolomics
  and lipidomics expand each slot into positive/negative polarity.
- **Sampler** — the physical autosampler (`Vanquish`, `MClass`, `Evosep`).
  Well-plate samplers use row/col positions; tip-plate samplers (Evosep) use
  tray + tip ranges.
- **Queue pattern** — the QC injection sequence (start / middle / end /
  separation) and how often QC is injected between user samples.
- **Container** — a B-Fabric order/container of samples; a queue can span
  multiple containers, separated by a `separation` QC block.
