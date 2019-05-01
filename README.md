# 2019 School Diversity Hearing

## Objective

This repository contains code to generate charts, tables, and statistics for the May 1, 2019 hearing on Diversity in NYC public schools.

### Prerequisites

R was used to analyze data presented here. The following packages are used:

- `kableExtra` 
- `cowplot` 
- `mapview` 
- `leaflet` 
- `sf` 
- `plotly` 
- `scales` 
- `ggrepel` 
- `councildown` 
- `janitor` 
- `readxl` 
- `forcats` 
- `stringr` 
- `dplyr` 
- `purrr` 
- `readr` 
- `tidyr` 
- `tibble` 
- `ggplot2`

### Documents

Results can be found in the `docs/` and `results/` directories.

## Data Sources

- [DOE 2017-18 diversity report](https://infohub.nyced.org/docs/default-source/default-document-library/report-on-demographic-data-in-nyc-public-schools.xlsx?sfvrsn=2b7837cc_2)
- [DOE Demographic Snapshot](https://infohub.nyced.org/docs/default-source/default-document-library/demographic-snapshot-2014-15-to-2018-19-(public).xlsx)

Code expects these data files to be in the directory `data/original_data/`.
