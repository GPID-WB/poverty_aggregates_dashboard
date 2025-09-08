### GET THE NECESSARY DATA ###

library(tidyverse)
library(pipr)
library(haven)

# Definitions
povertylines <- c(3.00, 4.20, 8.30)

# Classification data

class_data <- read_dta(
    "https://raw.githubusercontent.com/GPID-WB/Class/6e6123c1e5f1eea1636dd99f387aa98517d1ac7f/OutputData/CLASS.dta"
) %>%
    filter(year_release == max(year_release, na.rm = TRUE)) %>%
    select(economy, code, region, incgroup_historical, incgroup_current, fcv_historical, fcv_current, ida_historical, ida_current) %>%
    rename(iso3c = code) %>%
    mutate(iso3c = trimws(toupper(iso3c)))



# Define old poverty regions

reg_old <- get_aux("country_list") %>%
    select(country_code,pcn_region) %>%
    rename(iso3c = country_code) %>%
    rename(region_old = pcn_region)



# Poverty data from PIP

country_data <- purrr::map_df(
    .x = povertylines,
    .f = pipr::get_stats,
    country = "all",
    year = "all",
    nowcast = TRUE
) %>%
    rename(iso3c = country_code) %>%
    left_join(class_data, by = "iso3c") %>%
    left_join(reg_old, by = "iso3c") %>%
    select(region_name, region_code, region_old, country_name, iso3c, year, poverty_line, headcount, pop, incgroup_historical, incgroup_current, fcv_historical, fcv_current, ida_historical, ida_current) %>%
    mutate(pop_in_pov = headcount * pop)

# write
fst::write_fst(x    = country_data,
               path = fs::path("data",
                               "country_data.fst"))
