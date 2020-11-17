# daily_hsi


# data_import
- import outcome data files (persons, encounters, DMISID location codes)
- clean data:
  - apply case definition:
    - individual considered an incident case only once per calendar year
    - heat stroke prioritized over heat exhaustion
    - medical encounters for each individual within a calendar year prioritized, 
      hospitalizations > reportable medical events > ambulatory visits
- outcome dataset summaries


# dmisid

- compile location codes from DMIS Year End 1989 - 2019 files
- link DMIS codes to encounters
- inspect encounter observations missing DMIS codes and observations with DMIS codes not present in year-end reference tables
- recode installation names to account for alternate site names and different abbreviations, examples:
  - `FORT` / `FT`
  - `CHERRY POINT` / `CAMP LEJEUNE`
  - `BEAUFORT` / `PARRIS ISLAND`
- summarize encounters by installation


# bases_extract

- re-compile NLDAS hourly dataframe for all study installations, including Portsmouth and Pensacola (which were not part of previous extractions).
- output: `nldas_hourly_df` (saved to externl drive) <-  read_rds(file = "D:/data/nldas_hourly_df.rds")

# indices_daily

- expand dataframe with indices and unit conversions
