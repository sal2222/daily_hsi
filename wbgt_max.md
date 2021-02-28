---
title: "wbgt_max"
output: 
  html_document:
   keep_md: true
editor_options: 
  chunk_output_type: console
---






```r
cc_exposure_df <-
  read_rds(file = "data/cc_exposure_df.rds") 

cc_exposure_df <-
  cc_exposure_df %>% 
    mutate("wbgt_mean" = weathermetrics::celsius.to.fahrenheit(wbgt_mean),
           "wbgt_max" = weathermetrics::celsius.to.fahrenheit(wbgt_max))

base_service_df <-
   read_rds(file = "data/base_service_df.rds") 

# Add installation branch of service and climate region

cc_exposure_df <-
  cc_exposure_df %>% 
    left_join(base_service_df %>%
                rename(base_service = service) %>% 
                dplyr::select(installation_name, base_service, region), by = "installation_name") 



daily_indices <- 
  read_rds(file = "data/daily_indices.rds") %>% 
    filter(!installation %in% c("fort_drum", "fort_huachuca", "fort_lewis",
                                "lackland_afb", "fort_carson", "west_point_mil_reservation")) %>% 
    mutate("wbgt_mean" = weathermetrics::celsius.to.fahrenheit(wbgt_mean),
           "wbgt_max" = weathermetrics::celsius.to.fahrenheit(wbgt_max),
            installation = recode(installation,
              "mcb_camp_lejeune" = "camp_lejeune",
              "mcb_camp_pendleton" = "camp_pendleton",
              "fort_benning_ga" = "fort_benning",
              "fort_sam_houston" = "jbsa",
              "mcrd_beaufort_parris_island" = "parris_island",
              "mcb_quantico" = "quantico",
              "twentynine_palms_main_base" = "twentynine_palms")) 
     


daily_indices %>% 
  count(installation) 
```

```
## # A tibble: 24 x 2
## # Groups:   installation [24]
##    installation       n
##    <chr>          <int>
##  1 camp_lejeune   10958
##  2 camp_pendleton 10958
##  3 eglin_afb      10958
##  4 fort_benning   10958
##  5 fort_bliss     10958
##  6 fort_bragg     10958
##  7 fort_campbell  10958
##  8 fort_gordon    10958
##  9 fort_hood      10958
## 10 fort_jackson   10958
## # ... with 14 more rows
```

```r
cc_exposure_df %>% 
  count(installation_name) 
```

```
## # A tibble: 24 x 2
##    installation_name     n
##    <fct>             <int>
##  1 fort_benning      20795
##  2 fort_bragg        20964
##  3 camp_lejeune      13197
##  4 parris_island     10876
##  5 fort_campbell      8773
##  6 fort_polk          7466
##  7 fort_jackson       7782
##  8 camp_pendleton     6595
##  9 fort_hood          5830
## 10 mcrd_san_diego     5127
## # ... with 14 more rows
```


# cases at >92 deg F

```r
cc_exposure_df %>% 
  filter(wbgt_max >= 92)
```

```
## # A tibble: 6,061 x 48
##    installation_na~ studyid d_event    dmisid dx1   dx2   dx1_name source age  
##    <fct>            <chr>   <date>     <chr>  <chr> <chr> <chr>    <chr>  <chr>
##  1 fort_benning     S00040~ 2012-07-25 1315   9925  <NA>  <NA>     OUTPA~ 20-24
##  2 fort_benning     S00022~ 2004-06-29 0048   9925  E9000 <NA>     OUTPA~ <20  
##  3 fort_benning     S00014~ 1998-07-19 0048   9925  <NA>  <NA>     OUTPA~ 20-24
##  4 fort_benning     S00008~ 2013-07-30 0048   9925  E9000 <NA>     OUTPA~ 20-24
##  5 fort_benning     S00013~ 2010-06-23 0048   9925  E9000 <NA>     OUTPA~ 20-24
##  6 fort_benning     S00062~ 2005-08-22 1557   9925  <NA>  <NA>     OUTPA~ <20  
##  7 fort_benning     S00003~ 2000-08-09 0048   2765  9925  <NA>     OUTPA~ 20-24
##  8 fort_benning     S00030~ 2005-07-26 0048   9925  E9000 <NA>     OUTPA~ 25-29
##  9 fort_benning     S00052~ 2005-07-29 0048   9920  E9000 <NA>     INPAT~ 30-34
## 10 fort_benning     S00049~ 2007-08-13 0048   9920  2761  <NA>     INPAT~ 20-24
## # ... with 6,051 more rows, and 39 more variables: sex <chr>,
## #   race_ethnic <chr>, service <chr>, component <chr>, grade <chr>, hsi <chr>,
## #   height <chr>, weight <chr>, bmi <dbl>, hor_country <chr>, hor_state <chr>,
## #   year <dbl>, facility_name <chr>, facility_zipcode <chr>, dmis_fy <dbl>,
## #   n <int>, control_date <date>, case <dbl>, date <date>, tmp_f_mean <dbl>,
## #   heat_index_mean <dbl>, wbgt_mean <dbl>, wbgt_u_mean <dbl>, tmp_f_max <dbl>,
## #   heat_index_max <dbl>, wbgt_max <dbl>, wbgt_u_max <dbl>, tmp_f_min <dbl>,
## #   heat_index_min <dbl>, wbgt_min <dbl>, wbgt_u_min <dbl>, tmp_f_max95 <dbl>,
## #   heat_index_max95 <dbl>, wbgt_max95 <dbl>, wbgt_u_max95 <dbl>,
## #   stratum <fct>, wbgt_f_mean <dbl>, base_service <chr>, region <chr>
```

```r
# 6,061 cases out of 139,875 gt or equal to 92 max WBGT (~4.3% of cases at or beyond peak)
6061/139875
```

```
## [1] 0.04333155
```





```r
# DLNM
## Lags function

lags <- seq(5) # Number of lags

lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
  sep = "_")

lag_fun <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)
```




```r
selected_index <- "wbgt_max"


  # create lag matrix

lag_matrix <-
  daily_indices %>% 
    filter(date %in% as.Date("1997-12-15"):as.Date("2019-12-31")) %>% 
      dplyr::select(installation, date, selected_index) %>% 
   group_by(installation) %>% 
   mutate_at(vars(selected_index), funs_(lag_fun)
  )
```

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(selected_index)` instead of `selected_index` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
# join lag matrix to case-crossover dataframe

cc_lag_matrix <-
  cc_exposure_df %>% 
    filter(year %in% 1998:2019) %>% 
    dplyr::select(!selected_index) %>%    # remove to avoid double listing after join
    left_join(lag_matrix,  by = c("installation_name" = "installation", "date" = "date")) 


cc_lag_only <- 
  cc_lag_matrix %>% 
  dplyr::select(selected_index, lag_1:lag_5)


# Define dlnm cross-basis (penalized splines)

index_cb <-
    crossbasis(
      cc_lag_only,    
      lag = 5,    # of lags
      argvar = list(fun = "ns", df = 5),    #  functional form of the dose-response curve
      arglag = list(fun = "ns", df = 4))    #  functional form of the lags

summary(index_cb)
```

```
## CROSSBASIS FUNCTIONS
## observations: 139875 
## range: 9.26 to 107.69 
## lag period: 0 5 
## total df:  20 
## 
## BASIS FOR VAR:
## fun: ns 
## knots: 72.8 79.77 84.16 87.31 
## intercept: FALSE 
## Boundary.knots: 9.26 107.69 
## 
## BASIS FOR LAG:
## fun: ns 
## knots: 1.666667 3.333333 
## intercept: TRUE 
## Boundary.knots: 0 5
```

```r
# run model and get prediction for selected_index


index_dlnm <- survival::clogit(case ~ 
                  index_cb +  # lagged, nonlinear term for exposure
                  strata(stratum), 
                  method = "efron",
                  data =  cc_exposure_df) 


pred_dlnm <- crosspred(index_cb, index_dlnm, by = 1, from = 32, to = 110, cen = 60, cumul = TRUE)

summary(pred_dlnm)
```

```
## PREDICTIONS:
## values: 79 
## centered at: 60 
## range: 32 , 110 
## lag: 0 5 
## exponentiated: yes 
## cumulative: yes 
## 
## MODEL:
## parameters: 20 
## class: clogit coxph 
## link: logit
```

```r
## Overall effect RRs

pred_dlnm$allRRfit[c("80", "85", "90", "95", "100")]
```

```
##        80        85        90        95       100 
##  3.527382  5.309604 10.106615  9.145702  4.654569
```

```r
bind_cols(names(pred_dlnm$allRRfit), pred_dlnm$allRRfit, pred_dlnm$allRRlow, pred_dlnm$allRRhigh) %>% 
  dplyr::rename(var = 1, rr = 2, ci_low = 3, ci_high = 4) %>% 
  dplyr::filter(var %in% c("80", "85", "90", "95", "100")) %>% 
  knitr::kable()
```

```
## New names:
## * NA -> ...1
## * NA -> ...2
## * NA -> ...3
## * NA -> ...4
```



|var |        rr|   ci_low|   ci_high|
|:---|---------:|--------:|---------:|
|80  |  3.527382| 3.123472|  3.983523|
|85  |  5.309604| 4.725490|  5.965921|
|90  | 10.106615| 8.871038| 11.514286|
|95  |  9.145703| 7.815153| 10.702782|
|100 |  4.654569| 3.456924|  6.267136|

```r
bind_cols(names(pred_dlnm$allRRfit), pred_dlnm$allRRfit, pred_dlnm$allRRlow, pred_dlnm$allRRhigh) %>% 
  dplyr::rename(var = 1, rr = 2, ci_low = 3, ci_high = 4) %>% 
  dplyr::filter(var %in% c("80", "85", "90", "95", "100")) %>% 
  knitr::kable()
```

```
## New names:
## * NA -> ...1
## * NA -> ...2
## * NA -> ...3
## * NA -> ...4
```



|var |        rr|   ci_low|   ci_high|
|:---|---------:|--------:|---------:|
|80  |  3.527382| 3.123472|  3.983523|
|85  |  5.309604| 4.725490|  5.965921|
|90  | 10.106615| 8.871038| 11.514286|
|95  |  9.145703| 7.815153| 10.702782|
|100 |  4.654569| 3.456924|  6.267136|


## Plot model (ggplot)

```r
# As ggplot
  # https://www.rdocumentation.org/packages/season/versions/0.3.8/vignettes/season-vignette.Rmd link assisted with example "Plot of the temperature and death association averaging over all lags"

 
to_plot <- 
  data.frame(index = pred_dlnm$predvar, 
             mean = pred_dlnm$allRRfit,
             lower = pred_dlnm$allRRlow,
             upper = pred_dlnm$allRRhigh) %>% 
  filter(index <= 104)



plot_max_wbgt <-
  ggplot(data = to_plot, aes(x = index, y = mean, ymin = lower, ymax = upper)) +
    geom_hline(lty = 2, yintercept = 1) + # horizontal reference line at no change in odds
    geom_ribbon(alpha = 0.2, fill = "cadetblue", color = "cadetblue") +
    geom_line(size = 1.25) +
    xlab('Max WBGT (°F)') +
    ylab('Odds Ratio') +
    coord_cartesian(xlim = c(60, 104)) +
    theme_bw() 



# write_rds(plot_max_wbgt, file = "output/plot_max_wbgt.rds")

plot_max_wbgt +
  ggtitle("Daily max WBGT and HSI association \ncumulative over 0-5 days lag") +
  labs(caption = "ORs relative to 60°F max WBGT") +
  theme(plot.caption = element_text(hjust = 0)) 
```

![](wbgt_max_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# "Base R" plots by lag "slices"  

plot(pred_dlnm, "slices",
     lag = 0,
     ylim = c(0, 4),
     xlim = c(60, 104),
     lwd = 4,
     col = "red",
     main = "Exposure-Response Effects by Lag Day  \n  1998-2019", 
     xlab = "Max WBGT (°F)", 
     ylab = "HSI Rate Ratio")


lines(pred_dlnm, "slices",
     lag = 1,
     lwd = 4,
     col = "blue"
)

lines(pred_dlnm, "slices",
     lag = 2,
     lwd = 4,
     col = "green"
)


lines(pred_dlnm, "slices",
     lag = 3,
     lwd = 4,
     col = "orange")

legend("topleft", legend = c("Lag 0", "Lag 1", "Lag 2", "Lag 3"),
       col = c("red", "blue", "green", "orange"), lty = 1, cex = 1)
```

![](wbgt_max_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
#3D Plot prep

plot(pred_dlnm, 
     xlab = paste(selected_index), zlab = "\nRR", ylab = "\nLag", 
     theta = 40, phi = 30, lphi = 30,
     main = "1998-2019")
```

![](wbgt_max_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

```r
# https://rdrr.io/cran/dlnm/src/R/seqlag.R
seqlag <- function(lag,by = 1) seq(from = lag[1],to = lag[2],by = by)



to_plot <- 
  data.frame(index = pred_dlnm$predvar, 
             mean = pred_dlnm$allRRfit,
             lower = pred_dlnm$allRRlow,
             upper = pred_dlnm$allRRhigh)


to_3d <- list(x = matrix(pred_dlnm$predvar), 
              y = matrix(seqlag(pred_dlnm$lag, pred_dlnm$bylag)), 
              z = matrix(unlist(pred_dlnm$matfit), ncol = 6, byrow = TRUE))


x <- as_vector(pred_dlnm$predvar)
y <- as.vector(seqlag(pred_dlnm$lag, pred_dlnm$bylag))
z <- pred_dlnm$matRRfit

df_3d <-  
  z %>% 
    melt() %>% 
    as_tibble() %>% 
    rename(
      "Max WBGT" = Var1,
      "Lag" = Var2,
      "OR" = value
    ) %>% 
    mutate(Lag = as.numeric(str_remove(Lag, "lag"))) 

df_3d$OR %>% summary()
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.7320  0.9566  1.0270  1.1405  1.1159  3.1878
```

```r
# 3d plot with rayshader

gg_3d <-
  df_3d %>% 
  filter(`Max WBGT` >= 60) %>% 
  ggplot() +
    geom_raster(aes(x = `Max WBGT`, y = `Lag`, fill = `OR`)) +
    scale_fill_viridis()



plot_gg(gg_3d, multicore = TRUE, width = 5, height = 5, scale = 250)

# Render 3D movie

# filename_movie <- "output/max_wbgt_3d.mp4"
# 
# render_movie(filename = filename_movie, type = "orbit", 
#              frames = 360,  phi = 30, zoom = 0.8, theta = -90,
#              title_text = "Max WBGT Odds Ratio")


# 3D print file
#filename_stl = "output/mean_wbgt_3d.stl"
#save_3dprint(filename_stl, rotate = TRUE)
```



## Examine by region
Nest `cc_exposure_df` by region


```r
# join lag matrix to case-crossover dataframe
# mutate new list-column for lag only matrix

wbgt_max_nest_region <-
  cc_exposure_df %>%
    filter(year %in% 1998:2019) %>%
    dplyr::select(!selected_index) %>%    # remove to avoid double listing after join
    left_join(lag_matrix,  by = c("installation_name" = "installation", "date" = "date")) %>% 
    group_by(region) %>% 
    nest() %>% 
    rename(cc_lag_matrix = data) %>% 
    mutate(cc_lag_only =
             map(cc_lag_matrix, ~ .x %>% dplyr::select(selected_index, lag_1:lag_5)),
           index_cb = 
             map(cc_lag_only, ~ crossbasis(
                .x,    
                lag = 5,    # of lags
                argvar = list(fun = "ns", df = 5),    #  functional form of the dose-response curve
                arglag = list(fun = "ns", df = 4))                
              ),
           index_dlnm = 
             map2(.x = index_cb, .y = cc_lag_matrix,  ~ survival::clogit(case ~ 
                    .x +  # lagged, nonlinear term for exposure
                    strata(stratum), 
                    method = "efron",
                    data =  .y)),
            pred_dlnm = 
              map2(.x = index_cb, .y = index_dlnm, ~ crosspred(
                .x, .y, by = 1, from = 32, to = 110, cen = 60, cumul = TRUE)))
            

wbgt_max_nest_region
```

```
## # A tibble: 4 x 6
## # Groups:   region [4]
##   region   cc_lag_matrix     cc_lag_only     index_cb       index_dlnm pred_dlnm
##   <chr>    <list>            <list>          <list>         <list>     <list>   
## 1 Southea~ <tibble [87,561 ~ <tibble [87,56~ <crossbss [87~ <clogit>   <crosspr~
## 2 Ohio Va~ <tibble [12,613 ~ <tibble [12,61~ <crossbss [12~ <clogit>   <crosspr~
## 3 South    <tibble [23,547 ~ <tibble [23,54~ <crossbss [23~ <clogit>   <crosspr~
## 4 West     <tibble [16,154 ~ <tibble [16,15~ <crossbss [16~ <clogit>   <crosspr~
```

```r
# Plot cumulative lag

to_plot_region <-
  wbgt_max_nest_region %>% 
    mutate(to_plot = 
             map(.x = pred_dlnm, ~ 
                   data.frame(index = .x$predvar, 
                      mean = .x$allRRfit,
                      lower = .x$allRRlow,
                      upper = .x$allRRhigh))) %>% 
    dplyr::select(region, to_plot) %>% 
      unnest(to_plot) %>% 
  filter(index <= 104)



plot_max_wbgt_region <-
  ggplot(data = to_plot_region, aes(x = index, y = mean, ymin = lower, ymax = upper, color = region, fill = region)) +
    geom_hline(lty = 2, yintercept = 1) + # horizontal reference line at no change in odds
    geom_ribbon(alpha = 0.1, colour = NA) +
    geom_line(size = 1.25) +
    xlab('Max WBGT (°F)') +
    ylab('Odds Ratio') +
    coord_cartesian(xlim = c(60, 104)) +
    theme_bw() 

# write_rds(plot_max_wbgt_region, file = "output/plot_max_wbgt_region.rds")


plot_max_wbgt_region +
  ggtitle("Daily max WBGT and HSI association cumulative\nover 0-5 days lag by NOAA NCEI climate region") +
  labs(caption = "ORs relative to 55°F max WBGT") +
  theme(plot.caption = element_text(hjust = 0)) 
```

![](wbgt_max_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



## Examine by installation primary* branch of service

Note: there is overlap at sites (eg JBSA incl Lackland AFB and Fort Sam Houston)
Nest `cc_exposure_df` by `base_service`


```r
# join lag matrix to case-crossover dataframe
# mutate new list-column for lag only matrix

wbgt_max_nest_service <-
  cc_exposure_df %>%
    filter(year %in% 1998:2019) %>%
    dplyr::select(!selected_index) %>%    # remove to avoid double listing after join
    left_join(lag_matrix,  by = c("installation_name" = "installation", "date" = "date")) %>% 
    group_by(base_service) %>% 
    nest() %>% 
    rename(cc_lag_matrix = data) %>% 
    mutate(cc_lag_only =
             map(cc_lag_matrix, ~ .x %>% dplyr::select(selected_index, lag_1:lag_5)),
           index_cb = 
             map(cc_lag_only, ~ crossbasis(
                .x,    
                lag = 5,    # of lags
                argvar = list(fun = "ns", df = 5),    #  functional form of the dose-response curve
                arglag = list(fun = "ns", df = 4))                
              ),
           index_dlnm = 
             map2(.x = index_cb, .y = cc_lag_matrix,  ~ survival::clogit(case ~ 
                    .x +  # lagged, nonlinear term for exposure
                    strata(stratum), 
                    method = "efron",
                    data =  .y)),
            pred_dlnm = 
              map2(.x = index_cb, .y = index_dlnm, ~ crosspred(
                .x, .y, by = 1, from = 32, to = 110, cen = 60, cumul = TRUE)))
            

wbgt_max_nest_service
```

```
## # A tibble: 4 x 6
## # Groups:   base_service [4]
##   base_service cc_lag_matrix    cc_lag_only    index_cb     index_dlnm pred_dlnm
##   <chr>        <list>           <list>         <list>       <list>     <list>   
## 1 Army         <tibble [88,629~ <tibble [88,6~ <crossbss [~ <clogit>   <crosspr~
## 2 Marine Corps <tibble [42,871~ <tibble [42,8~ <crossbss [~ <clogit>   <crosspr~
## 3 Air Force    <tibble [5,449 ~ <tibble [5,44~ <crossbss [~ <clogit>   <crosspr~
## 4 Navy         <tibble [2,926 ~ <tibble [2,92~ <crossbss [~ <clogit>   <crosspr~
```

```r
# Plot cumulative lag

to_plot_region <-
  wbgt_max_nest_service %>% 
    mutate(to_plot = 
             map(.x = pred_dlnm, ~ 
                   data.frame(index = .x$predvar, 
                      mean = .x$allRRfit,
                      lower = .x$allRRlow,
                      upper = .x$allRRhigh))) %>% 
    dplyr::select(base_service, to_plot) %>% 
      unnest(to_plot) %>% 
  filter(index <= 104)



ggplot(data = to_plot_region, aes(x = index, y = mean, ymin = lower, ymax = upper, color = base_service, fill = base_service)) +
  geom_hline(lty = 2, yintercept = 1) + # horizontal reference line at no change in odds
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line(size = 1.25) +
  xlab('Max WBGT (°F)') +
  ylab('Odds Ratio') +
  coord_cartesian(xlim = c(60, 104)) +
  theme_bw() +
  ggtitle("Daily max WBGT and HSI association cumulative\nover 0-5 days lag by installation primary service branch") +
  labs(caption = "ORs relative to 60°F max WBGT") +
  theme(plot.caption = element_text(hjust = 0)) 
```

![](wbgt_max_files/figure-html/unnamed-chunk-7-1.png)<!-- -->




## Examine by time in season

1st look: pre vs post 4th of July 
days 0-185 (early season) vs days 186-366 (late season) 

  mutate(`Day of Year` = lubridate::yday(date))

```r
# join lag matrix to case-crossover dataframe
# mutate new list-column for lag only matrix

wbgt_max_nest_tis <-
  cc_exposure_df %>%
    filter(year %in% 1998:2019) %>%
    dplyr::select(!selected_index) %>%    # remove to avoid double listing after join
    left_join(lag_matrix,  by = c("installation_name" = "installation", "date" = "date")) %>%
    mutate(`Day of Year` = lubridate::yday(date),
           `Time in season` = case_when(
                 `Day of Year` <= 185 ~ "Early season",
                   `Day of Year` > 185 ~ "Late season"
              )) %>% 
    group_by(`Time in season`) %>% 
    nest() %>% 
    rename(cc_lag_matrix = data) %>% 
    mutate(cc_lag_only =
             map(cc_lag_matrix, ~ .x %>% dplyr::select(selected_index, lag_1:lag_5)),
           index_cb = 
             map(cc_lag_only, ~ crossbasis(
                .x,    
                lag = 5,    # of lags
                argvar = list(fun = "ns", df = 5),    #  functional form of the dose-response curve
                arglag = list(fun = "ns", df = 4))                
              ),
           index_dlnm = 
             map2(.x = index_cb, .y = cc_lag_matrix,  ~ survival::clogit(case ~ 
                    .x +  # lagged, nonlinear term for exposure
                    strata(stratum), 
                    method = "efron",
                    data =  .y)),
            pred_dlnm = 
              map2(.x = index_cb, .y = index_dlnm, ~ crosspred(
                .x, .y, by = 1, from = 32, to = 110, cen = 60, cumul = TRUE)))
            

wbgt_max_nest_tis
```

```
## # A tibble: 2 x 6
## # Groups:   Time in season [2]
##   `Time in season` cc_lag_matrix   cc_lag_only   index_cb   index_dlnm pred_dlnm
##   <chr>            <list>          <list>        <list>     <list>     <list>   
## 1 Late season      <tibble [83,63~ <tibble [83,~ <crossbss~ <clogit>   <crosspr~
## 2 Early season     <tibble [56,24~ <tibble [56,~ <crossbss~ <clogit>   <crosspr~
```

```r
# Plot cumulative lag

to_plot_region <-
  wbgt_max_nest_tis %>% 
    mutate(to_plot = 
             map(.x = pred_dlnm, ~ 
                   data.frame(index = .x$predvar, 
                      mean = .x$allRRfit,
                      lower = .x$allRRlow,
                      upper = .x$allRRhigh))) %>% 
    dplyr::select(`Time in season`, to_plot) %>% 
      unnest(to_plot) %>% 
  filter(index <= 104)



ggplot(data = to_plot_region, aes(x = index, y = mean, ymin = lower, ymax = upper, color = `Time in season`, fill = `Time in season`)) +
  geom_hline(lty = 2, yintercept = 1) + # horizontal reference line at no change in odds
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line(size = 1.25) +
  xlab('Max WBGT (°F)') +
  ylab('Odds Ratio') +
  coord_cartesian(xlim = c(60, 104)) +
  theme_bw() +
  ggtitle("Daily max WBGT and HSI association cumulative\nover 0-5 days lag by time in season") +
  labs(caption = "ORs relative to 60°F max WBGT") +
  theme(plot.caption = element_text(hjust = 0)) 
```

![](wbgt_max_files/figure-html/unnamed-chunk-8-1.png)<!-- -->




## Examine by installation



```r
# join lag matrix to case-crossover dataframe
# mutate new list-column for lag only matrix

wbgt_max_nest_base <-
  cc_exposure_df %>%
    filter(year %in% 1998:2019) %>%
    dplyr::select(!selected_index) %>%    # remove to avoid double listing after join
    left_join(lag_matrix,  by = c("installation_name" = "installation", "date" = "date")) %>%
    group_by(installation_name) %>% 
    nest() %>% 
    rename(cc_lag_matrix = data) %>% 
    mutate(cc_lag_only =
             map(cc_lag_matrix, ~ .x %>% dplyr::select(selected_index, lag_1:lag_5)),
           index_cb = 
             map(cc_lag_only, ~ crossbasis(
                .x,    
                lag = 5,    # of lags
                argvar = list(fun = "ns", df = 5),    #  functional form of the dose-response curve
                arglag = list(fun = "ns", df = 4))                
              ),
           index_dlnm = 
             map2(.x = index_cb, .y = cc_lag_matrix,  ~ survival::clogit(case ~ 
                    .x +  # lagged, nonlinear term for exposure
                    strata(stratum), 
                    method = "efron",
                    data =  .y)),
            pred_dlnm = 
              map2(.x = index_cb, .y = index_dlnm, ~ crosspred(
                .x, .y, by = 1, from = 32, to = 110, cen = 60, cumul = TRUE)))
            

wbgt_max_nest_base
```

```
## # A tibble: 24 x 6
## # Groups:   installation_name [24]
##    installation_name cc_lag_matrix  cc_lag_only  index_cb   index_dlnm pred_dlnm
##    <chr>             <list>         <list>       <list>     <list>     <list>   
##  1 fort_benning      <tibble [20,7~ <tibble [20~ <crossbss~ <clogit>   <crosspr~
##  2 fort_bragg        <tibble [20,9~ <tibble [20~ <crossbss~ <clogit>   <crosspr~
##  3 camp_lejeune      <tibble [13,1~ <tibble [13~ <crossbss~ <clogit>   <crosspr~
##  4 parris_island     <tibble [10,8~ <tibble [10~ <crossbss~ <clogit>   <crosspr~
##  5 fort_campbell     <tibble [8,77~ <tibble [8,~ <crossbss~ <clogit>   <crosspr~
##  6 fort_polk         <tibble [7,46~ <tibble [7,~ <crossbss~ <clogit>   <crosspr~
##  7 fort_jackson      <tibble [7,78~ <tibble [7,~ <crossbss~ <clogit>   <crosspr~
##  8 camp_pendleton    <tibble [6,59~ <tibble [6,~ <crossbss~ <clogit>   <crosspr~
##  9 fort_hood         <tibble [5,83~ <tibble [5,~ <crossbss~ <clogit>   <crosspr~
## 10 mcrd_san_diego    <tibble [5,12~ <tibble [5,~ <crossbss~ <clogit>   <crosspr~
## # ... with 14 more rows
```

```r
# Plot cumulative lag

to_plot_base <-
  wbgt_max_nest_base %>% 
    mutate(to_plot = 
             map(.x = pred_dlnm, ~ 
                   data.frame(index = .x$predvar, 
                      mean = .x$allRRfit,
                      lower = .x$allRRlow,
                      upper = .x$allRRhigh))) %>% 
    dplyr::select(installation_name, to_plot) %>% 
      unnest(to_plot) %>% 
  filter(index <= 104)



ggplot(data = to_plot_base, aes(x = index, y = mean, ymin = lower, ymax = upper, color = installation_name, fill = installation_name)) +
    geom_hline(lty = 2, yintercept = 1) + # horizontal reference line at no change in odds
   # geom_ribbon(alpha = 0.1, colour = NA) +
    geom_line(size = 1) +
    ggrepel::geom_text_repel(data = to_plot_base %>% group_by(installation_name) %>% 
            filter(index == 85,
                   mean >= 10),
            aes(label = installation_name),
            nudge_x = -10,
            nudge_y = 10) +
    labs(title = "min.segment.length = 0") +
    xlab('Max WBGT (°F)') +
    ylab('Odds Ratio') +
    coord_cartesian(xlim = c(60, 104), ylim = c(NA, 80)) +
    theme_bw() +
    ggtitle("Daily maximum WBGT and HSI association cumulative\nover 0-5 days lag by installation") +
    labs(caption = "ORs relative to 60°F maximum WBGT") +
    theme(plot.caption = element_text(hjust = 0)) 
```

![](wbgt_max_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
