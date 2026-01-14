# Test Data for Queue Generator

This directory contains reference data from mass spectrometry instruments and the testing infrastructure for validating the queue generator.

## Directory Structure

```
test_data/
├── Snakefile                    # Combined workflow orchestration
├── README.md                    # This file
│
├── reference/                   # Ground truth data from instruments
│   ├── sld/                     # Raw Thermo Xcalibur SLD binaries
│   │   ├── astral/              # 212 files (Vanquish sampler)
│   │   ├── ascend/              # 559 files (MClass48 sampler)
│   │   ├── lumos2/              # 2,760 files (MClass48 sampler)
│   │   └── exploris2/           # 4,295 files (MClass48 sampler)
│   ├── csv/                     # Parsed queue CSVs
│   │   ├── astral/
│   │   ├── ascend/
│   │   ├── lumos2/
│   │   └── exploris2/
│   └── merged.csv               # All instruments combined
│
├── params_json/                 # Test input parameter files
│   ├── astral/                  # 108 JSON params (ready)
│   ├── ascend/                  # 196 JSON params (pending sampler)
│   └── lumos2/                  # 1,533 JSON params (pending sampler)
│
├── generated/                   # Queue generator output
│   └── astral/                  # Generated queue CSVs
│
├── results/                     # Test comparison results
│   ├── astral/                  # Per-file comparison JSONs
│   └── summary.csv              # Aggregated test results
│
└── tools/                       # Processing scripts
    ├── sld_to_csv.py            # SLD → CSV converter
    ├── csv_to_params.py         # CSV → param JSON generator
    ├── compare_queues.py        # Compare generated vs reference
    ├── summarize.py             # Aggregate comparison results
    ├── queue_analysis_merge.py  # Merge CSVs → reference/merged.csv
    └── queue_analysis_marimo.py # Interactive analysis app (uses merged.csv)
```

## Workflow

All commands run from the `test_data/` directory.

### Quick Start

```bash
cd test_data
snakemake help           # Show available targets
snakemake -j4 test       # Run test pipeline (most common)
```

### Full Pipeline

```bash
# Phase 1: Convert SLD files to CSV (one-time, data already converted)
snakemake -j4 sld_to_csv

# Phase 2: Generate test params from reference CSVs
snakemake params

# Phase 3: Run tests
snakemake -j4 test

# Or run everything:
snakemake -j4 all
```

### View Results

```bash
cat results/summary.csv           # Test summary
cat results/astral/<name>.json    # Individual comparison
```

### Clean Up

```bash
snakemake clean      # Remove generated/ and results/
snakemake clean_all  # Also remove params_json/
```

## Test Pipeline

The test pipeline validates the queue generator using round-trip testing:

1. **Reference Data**: Real queue CSV files exported from Thermo Xcalibur SLD files
2. **Param Generation**: Parse reference CSVs to extract sample info and parameters
3. **Queue Generation**: Run `queue_generator.py` on each param file
4. **Comparison**: Compare generated queue with original reference
5. **Summary**: Aggregate results into summary CSV

### Comparison Criteria

- **Sample IDs match** - Primary criterion (pass/fail)
- **QC count** - Warns if significantly different (>2)
- **Position plates** - Verifies same plates used

### Status Values

| Status | Meaning |
|--------|---------|
| `pass` | Sample IDs match exactly |
| `partial` | Sample IDs overlap but don't match exactly |
| `fail` | Sample IDs don't match |
| `error` | Generation failed or file error |

## Current Test Status

| Instrument | Sampler | Params | Tests | Status |
|------------|---------|--------|-------|--------|
| Astral | Vanquish | 108 | 106 pass | ✅ Complete |
| Ascend | MClass48 | 196 | - | ⏳ Pending (sampler not implemented) |
| Lumos2 | MClass48 | 1,533 | - | ⏳ Pending (sampler not implemented) |
| Exploris2 | MClass48 | - | - | ⏳ Pending (sampler not implemented) |

## SLD File Format

Thermo Xcalibur `.sld` files are binary sequence list files containing:
- Sample run information
- Vial positions
- Method paths
- Output directories

The parser (`tools/sld_to_csv.py`) handles both new (JSON metadata) and old (binary encoded) formats.

## Reference CSV Format

```csv
row,run,vial,filename,method,output_path
1,1,Y:A1,20250908_001_autoQC015,C:\Xcalibur\methods\...,D:\Data2San\...
2,2,Y:B1,20250908_002_S1000101_sample,C:\Xcalibur\methods\...,D:\Data2San\...
```

| Column | Description |
|--------|-------------|
| `row` | Row number in sequence list |
| `run` | Run/injection number |
| `vial` | Sample position (`Y:A1` for Vanquish, `1:A` for MClass48) |
| `filename` | Output filename with date, container ID, sample ID, name |
| `method` | Full path to Xcalibur method |
| `output_path` | Data output directory |

## Param JSON Format

```json
{
  "parameters": {
    "container_id": 39204,
    "instrument": "ASTRAL_1",
    "sampler": "Vanquish.vial",
    "date": "20250806",
    "user": "antdit"
  },
  "samples": [
    {"Sample Name": "sample_name", "Sample ID": 981508}
  ],
  "_source_file": "20250806.csv"
}
```
