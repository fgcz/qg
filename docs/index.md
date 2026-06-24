# Queue Generation System

Generate sample queues with QC injections for mass spectrometry instruments
(XCalibur, Chronos, Hystar). It runs **standalone** — upload a CSV/XLSX sample
table and download a queue — or as the FGCZ **B-Fabric portal** app with LIMS
sample loading and workunit upload.

The system takes a runtime **Queue Parameters** JSON (instrument, sampler,
samples) plus a set of static **config files** and produces an instrument-ready
sequence file: user samples interleaved with QC injections at pattern-defined
positions, with file names and data paths filled from templates.

## Where to go next

| If you want to… | Read |
|------------------|------|
| Generate a queue locally from a CSV/XLSX upload | [Local app](users/local_app.md) |
| Try ready-made example sample tables | load one from the local app's example dropdown, or browse [`docs/examples/`](https://gitlab.bfabric.org/metabolomics/queue-gen/-/tree/main/docs/examples) |
| Run the B-Fabric portal app, understand auth modes | [User modes](users/user_modes.md) |
| Add a QC pattern, sample, or layout via the GUI | [Config editor guide](users/editor_guide.md) |
| Understand the generation pipeline end to end | [Algorithm](reference/algorithm.md) |
| Look up a config file's purpose and schema | [Configuration](reference/config.md) |
| Deploy the app + editor in production | [Deployment](developers/deployment.md) |
| See how the system was built | [History](developers/history.md) |

## Quick start

Installation (core vs. the `qg[bfabric]` portal extra) and the run/CLI quick start
are maintained in the project
[README](https://gitlab.bfabric.org/metabolomics/queue-gen/-/blob/main/README.md).
The fastest path: `make app-local` (or `qg-app-local`) opens the standalone app —
load a bundled example from the dropdown (or upload one of the
[examples](https://gitlab.bfabric.org/metabolomics/queue-gen/-/tree/main/docs/examples))
and download a queue. The routing table above points you to the right page for each task.

## Core concepts

- **Technology** — `proteomics`, `metabolomics`, or `lipidomics`. Metabolomics
  and lipidomics expand each slot into positive/negative polarity.
- **Sampler** — the physical autosampler (`Vanquish`, `MClass`, `Evosep`).
  Well-plate samplers use row/col positions; tip-plate samplers (Evosep) use
  tray + tip ranges.
- **Queue pattern** — the QC injection sequence (start / middle / end /
  separation) and how often QC is injected between user samples.
- **Container** — a group of samples sharing a `container_id` (a B-Fabric
  order/container in portal mode, or just a column value in an uploaded table). A
  queue can span multiple containers, separated by a `separation` QC block;
  samples are shuffled only *within* a container.
