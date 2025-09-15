
rm(list = ls())

library(fst)

path <- "country_data.fst"  # or adjust to your full path

country_data <- read_fst(path, as.data.table = TRUE)  # if you want a data.table


library(data.table)

dt <- as.data.table(country_data)

# Step 1: weighted headcount by region_name
hc_name <- dt[, .(headcount_region_name = weighted.mean(headcount, pop, na.rm = FALSE)),
              by = .(poverty_line, region = region_name)]

# Step 2: weighted headcount by region_old
hc_old <- dt[, .(headcount_region_old = weighted.mean(headcount, pop, na.rm = FALSE)),
             by = .(poverty_line, region = region_old)]

# Step 3: merge side by side on poverty_line + region
summary_wide <- merge(hc_name, hc_old, by = c("poverty_line", "region"), all = FALSE)

# Step 4: optional - round and order
summary_wide[, `:=`(
    headcount_region_name = round(headcount_region_name, 4),
    headcount_region_old  = round(headcount_region_old,  4)
)]
setorder(summary_wide, poverty_line, region)

# View
summary_wide



# Step 1: Get unique combinations of iso3c and their region classifications
region_map <- unique(dt[, .(iso3c, country_name, region_old, region_name)])

# Step 2: Identify rows where the region classification differs
region_diff <- region_map[region_old != region_name]

# View the result
region_diff_v2 <- region_diff[order(region_old, region_name)]

