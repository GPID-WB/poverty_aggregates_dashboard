### GET THE NECESSARY DATA ###

# devtools::install_github("worldbank/pip", ref = "0.10.15.9000")

rm(list = ls())

library(tidyverse)
library(pipr)
library(haven)
library(fst)
library(fs)

# Definitions
povertylines <- c(3.00, 4.20, 8.30)

# Classification data

class_data <- read_dta(
    "https://raw.githubusercontent.com/GPID-WB/Class/master/OutputData/CLASS.dta"
) %>%
    select(year_data, economy, code, region, regionpcn, incgroup, fcv, ida) %>%
    rename(iso3c = code,
           year = year_data,
           incgroup_historical = incgroup,
           fcv_historical = fcv,
           ida_historical = ida,
           region_name = region,
           region_old = regionpcn) %>%
    mutate(iso3c = trimws(toupper(iso3c))) %>%
    group_by(iso3c) %>%
    mutate(incgroup_current = incgroup_historical[year == 2024],
           fcv_current = fcv_historical[year == 2024],
           ida_current = ida_historical[year == 2024]) %>%
    ungroup()

# Population data: for Argentina

pip_pop <- read_dta("data/pip_population_20260324_2021_01_02_PROD.dta") %>%
    filter(country_code == "ARG" & data_level == "national")

# Coverage data
pip_cov <- read_dta("data/pip_country_coverage_20260324_2021_01_02_PROD_qa0326_v02.dta") %>%
    select(country_code, year, coverage) %>%
    rename(iso3c = country_code) %>%
    distinct(iso3c, year, .keep_all = TRUE)

# Poverty data from PIP

# (PLACEHOLDER) Import downloaded latest updates for PIP data

country_data <- read_dta("data/pip_fillgaps_20260324_2021_01_02_PROD_qa0326_V02.dta") %>%
    left_join(pip_pop, by = c("year", "country_code")) %>%
    mutate(population = if_else(country_code == "ARG", value, population)) %>%
    filter(country_name == "Argentina" | reporting_level == "national") %>%
    rename(iso3c = country_code,
           pop = population) %>%
    left_join(class_data, by = c("year","iso3c")) %>%
    rename(region_name = region_name.x) %>%
    select(region_name, region_code, region_old, country_name, iso3c, year, poverty_line, headcount, pop, incgroup_historical, incgroup_current, fcv_historical, fcv_current, ida_historical, ida_current) %>%
    mutate(pop_in_pov = headcount * pop)

# Create a new variable for WDI region (excluding HICs)
country_data <- country_data %>%
    mutate(
        region_WDI = case_when(
            incgroup_current != "High income" ~ paste0(region_name, " (excl. HICs)"),
            TRUE ~ NA_character_
        )
    )

# Merge with coverage information
country_data <- country_data %>%
    left_join(pip_cov, by = c("iso3c", "year"))

# Create two new groups for IDA+Blend (historical and current)
country_data <- country_data %>%
    mutate(
        ida_blend_historical = case_when(
            is.na(ida_historical) | ida_historical == "" ~ NA_character_,
            ida_historical %in% c("IDA", "Blend") ~ "Yes",
            TRUE ~ "No"
        ),
        ida_blend_current = case_when(
            is.na(ida_current) | ida_current == "" ~ NA_character_,
            ida_current %in% c("IDA", "Blend") ~ "Yes",
            TRUE ~ "No"
        )
    )


# # (RECOVER AFTER UPDATE) Extract data directly from PIP
# country_data_old <- purrr::map_df(
#     .x = povertylines,
#     .f = pipr::get_stats,
#     country = "all",
#     year = "all",
#     nowcast = TRUE
# ) %>%
#     rename(iso3c = country_code) %>%
#     left_join(class_data, by = c("year","iso3c")) %>%
#     left_join(reg_old, by = "iso3c") %>%
#     select(region, region_code, region_old, country_name, iso3c, year, poverty_line, headcount, pop, incgroup_historical, incgroup_current, fcv_historical, fcv_current, ida_historical, ida_current) %>%
#     mutate(pop_in_pov = headcount * pop) %>%
#     rename(region_name = region)
#

# write
fst::write_fst(x    = country_data,
               path = fs::path("country_data.fst"))
