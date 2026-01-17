# Private Function Candidates

This document lists functions that are candidates for making private (prefixing with `_`).

## Criteria Used

1. Function is only called from within the same module
2. Function is NOT important/complex enough to warrant explicit testing
3. Functions called from QueueGenerator pipeline should remain **PUBLIC**
4. Functions with explicit tests should remain **PUBLIC**

## Summary

| Status | Count |
|--------|-------|
| Already private | Many (best practices followed) |
| Keep public | Pipeline functions, tested functions |
| Candidates for `_` prefix | 19 functions |

---

## Modules Already Following Best Practices

These modules already correctly prefix internal functions with `_`:

- **builder.py** - All internal methods correctly prefixed
- **strategies.py** - Helper `_merge_positions()` already private
- **config.py** - All `_validate_*` and `_cross_validate_*` functions already private
- **tools/sld_to_csv.py** - Parsing helpers already private

---

## Functions to Keep Public

### queue_structure.py

| Function | Rationale |
|----------|-----------|
| `compute_middle_block_positions()` | Imported and tested in `tests/test_queue_structure.py` |
| `compute_extended_positions()` | Imported and tested in `tests/test_queue_structure.py` |

### generator.py

| Function | Rationale |
|----------|-----------|
| `lookup_sample_config()` | QueueGenerator pipeline function |
| `build_slots()` | QueueGenerator pipeline function |
| `expand_polarities()` | QueueGenerator pipeline function |
| `resolve_methods()` | QueueGenerator pipeline function |
| `format_file_names()` | QueueGenerator pipeline function |
| `build_queue_rows()` | QueueGenerator pipeline function |
| `format_csv()` | QueueGenerator pipeline function |

---

## Candidates for Private (`_` prefix)

### config.py

| Function | Current | Recommendation | Rationale |
|----------|---------|----------------|-----------|
| `derive_polarity_technologies()` | public | `_derive_polarity_technologies()` | Internal helper called only from `ConfigBundle.load()`, no direct tests |
| `derive_valid_samplers()` | public | `_derive_valid_samplers()` | Internal helper called only from `ConfigBundle.load()`, no direct tests |

### tools/compare.py

| Function | Current | Recommendation | Rationale |
|----------|---------|----------------|-----------|
| `extract_sample_ids()` | public | `_extract_sample_ids()` | Internal helper for comparison logic |
| `count_qc()` | public | `_count_qc()` | Internal helper for comparison logic |
| `get_plates_used()` | public | `_get_plates_used()` | Internal helper for comparison logic |

### tools/sld_to_csv.py

| Function | Current | Recommendation | Rationale |
|----------|---------|----------------|-----------|
| `check_run_numbering()` | public | `_check_run_numbering()` | Internal validation helper |
| `sanitize_samples()` | public | `_sanitize_samples()` | Internal data cleaning helper |
| `parse_sld_file_new_format()` | public | `_parse_sld_file_new_format()` | Internal parser, called by `parse_sld_file()` |
| `parse_sld_file_old_format()` | public | `_parse_sld_file_old_format()` | Internal parser, called by `parse_sld_file()` |
| `print_queue_table()` | public | `_print_queue_table()` | Internal display helper |
| `export_csv()` | public | `_export_csv()` | Internal I/O helper |
| `process_single_sld()` | public | `_process_single_sld()` | Internal orchestration helper |

### tools/csv_to_params.py

| Function | Current | Recommendation | Rationale |
|----------|---------|----------------|-----------|
| `parse_output_path()` | public | `_parse_output_path()` | Internal path parsing helper |
| `parse_filename()` | public | `_parse_filename()` | Internal filename parsing helper |

### tools/summarize.py

| Function | Current | Recommendation | Rationale |
|----------|---------|----------------|-----------|
| `extract_instrument_sampler()` | public | `_extract_instrument_sampler()` | Internal extraction helper |
| `aggregate_results()` | public | `_aggregate_results()` | Internal aggregation helper |

### tools/merge.py

| Function | Current | Recommendation | Rationale |
|----------|---------|----------------|-----------|
| `merge_all_csvs()` | public | `_merge_all_csvs()` | Internal merge helper |
| `classify_sample()` | public | `_classify_sample()` | Internal classification helper |

### cli/generate_params.py

| Function | Current | Recommendation | Rationale |
|----------|---------|----------------|-----------|
| `load_orders()` | public | `_load_orders()` | Internal data loading helper |
| `fetch_samples()` | public | `_fetch_samples()` | Internal B-Fabric interaction helper |
| `get_valid_combinations()` | public | `_get_valid_combinations()` | Internal config processing helper |
| `generate_queue_params()` | public | `_generate_queue_params()` | Internal generation helper |
| `params_filename()` | public | `_params_filename()` | Internal filename formatting helper |

### cli/find_projects.py

| Function | Current | Recommendation | Rationale |
|----------|---------|----------------|-----------|
| `get_proteomics_projects_with_samples()` | public | `_get_proteomics_projects_with_samples()` | Internal B-Fabric query helper |
| `get_order_info()` | public | `_get_order_info()` | Internal data extraction helper |

---

## Next Steps

1. Review this list and mark any functions that should remain public
2. Apply `_` prefix to approved candidates
3. Run tests to ensure no external imports break
