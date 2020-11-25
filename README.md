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
- output: 
    - `nldas_hourly_df` (saved to externl drive) <-  read_rds(file = "D:/data/nldas_hourly_df.rds")  
    - `centroid_coords` <- read_rds(file = "D:/data/centroid_coords.rds")

# hourly_indices

- expand dataframe with indices and unit conversions
  - add timezone, local time
  - windspeed from UGRD/VGRD
  - temp in deg F and C from K
  - RH from SPFH, TMP, PRES
  - dew point from RH and TMP
  - heat index from Temp(F) and RH
  - WBGT from time (year, month, day, hour, minute, gmt, avg period), lat/lon, DSWRF, PRES/100, temp(C), RH, windspeed
  - Urban and rural WBGT estimates
 - output: `hourly_indices_all` 7,617,632 obs of 25 variables
 
 
# met_summary
- create 
  - daily indices (from hourly): mean, max, min, top percentile (95th)
  - month-year indices
  - monthly indices
  - annual indices
  - daily anomalies (mean daily temp, heat index, wet bulb, WBGT)
- 
    
