

country_data <- read_dta("data/pip_fillgaps_20250930_2021_01_02_PROD.dta") %>%
    left_join(pip_pop, by = c("year", "country_code")) %>%
    mutate(population = if_else(country_code == "ARG", value, population)) %>%
    filter(country_name == "Argentina" | reporting_level == "national") %>%
    rename(iso3c = country_code,
           pop = population) %>%
    left_join(class_data, by = "iso3c") %>%
    left_join(reg_old, by = "iso3c") %>%
    select(region_name, region_code, region_old, country_name, iso3c, year, poverty_line, headcount, pop, incgroup_historical, incgroup_current, fcv_historical, fcv_current, ida_historical, ida_current) %>%
    mutate(pop_in_pov = headcount * pop)

# Create a new variable for WDI region (excluding HICs)
country_data <- country_data %>%
    mutate(
        region_WDI = case_when(
            incgroup_current != "High income" ~ paste0(region_name, " (excluding HICs)"),
            TRUE ~ NA_character_
        )
    )

library(dplyr)

# 1. Join coverage info
country_data_cov <- country_data %>%
    left_join(pip_cov, by = c("iso3c", "year"))

# 2. Compute total population by region-year
region_totals <- country_data_cov %>%
    group_by(region_old, year) %>%
    summarise(tot_pop = sum(pop, na.rm = TRUE), .groups = "drop")

# 3. Compute covered population by region-year (only where coverage == TRUE)
region_covered <- country_data_cov %>%
    filter(coverage == TRUE) %>%
    group_by(region_old, year) %>%
    summarise(cov_pop = sum(pop, na.rm = TRUE), .groups = "drop")

# 4. Compute pop share and flag whether region-year is "sufficiently covered"
region_coverage_flag <- region_totals %>%
    left_join(region_covered, by = c("region_old", "year")) %>%
    mutate(
        pop_share = cov_pop / tot_pop,
        cov_region_name = pop_share >= 0.5  # TRUE if >= 50% coverage
    )
# 5. Merge flag back to full dataset
country_data_cov <- country_data_cov %>%
    left_join(region_coverage_flag, by = c("region_old", "year"))
