# Worked examples

Scripts, collected traces and outputs for the two worked examples in the manuscript. Run the scripts from the repository root with conCultuR 0.9.0 installed.

| Folder | Example | Scripts |
|---|---|---|
| `example1/` | Attention gaps in 18 threatened vertebrates, 1 August–30 October 2026 | `01_collect_live.R` (live collection), `02_analysis.R` (index, gaps, sensitivity, figures), `03_options.R` (one option changed at a time) |
| `example2/` | Event diagnostic for the Convention on Biological Diversity around the CBD COP17 Leaders' Summit, collected 1 August–2 November 2026 | `01_collect_live.R`, `02_analysis.R` (event diagnostic with placebo dates), `03_sensitivity.R` (without Google Trends) |

`outputs/05_raw_traces.csv` holds the traces collected on 7 November 2026. `02_analysis.R` and `03_*.R` run offline from these traces; `01_collect_live.R` collects new data, which will differ because Google Trends re-samples values and the GDELT DOC API keeps only about three months of timelines. `gdelt_saved/` holds browser-saved GDELT responses read with `read_gdelt_json()`.
