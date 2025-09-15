


library(fst)

path <- "country_data.fst"  # or adjust to your full path

country_data <- read_fst(path, as.data.table = TRUE)  # if you want a data.table

library(data.table)

# Ensure data.table
setDT(country_data)

# Helper to summarize one pair
pair_summary <- function(DT, col_a, col_b) {
    stopifnot(col_a %in% names(DT), col_b %in% names(DT))

    # Row level equality, treating NA NA as equal
    same_flag <- ifelse(is.na(DT[[col_a]]) & is.na(DT[[col_b]]),
                        TRUE,
                        DT[[col_a]] == DT[[col_b]])
    share_equal_pct <- mean(same_flag) * 100

    # One to one mapping checks
    max_unique_B_per_A <- DT[, .(nB = uniqueN(get(col_b))), by = col_a][, max(nB)]
    max_unique_A_per_B <- DT[, .(nA = uniqueN(get(col_a))), by = col_b][, max(nA)]

    # Aggregation exactly as in the app
    agg_a <- DT[
        , .(
            headcount   = weighted.mean(headcount, pop, na.rm = TRUE),
            pop_in_pov  = sum(pop_in_pov, na.rm = TRUE)
        ),
        by = .(poverty_line, year, grp = get(col_a))
    ]

    agg_b <- DT[
        , .(
            headcount   = weighted.mean(headcount, pop, na.rm = TRUE),
            pop_in_pov  = sum(pop_in_pov, na.rm = TRUE)
        ),
        by = .(poverty_line, year, grp = get(col_b))
    ]

    cmp <- merge(
        agg_a, agg_b,
        by = c("poverty_line", "year", "grp"),
        all = TRUE,
        suffixes = c("_a", "_b")
    )[
        , `:=`(
            diff_headcount = headcount_a - headcount_b,
            diff_pop_mill  = (pop_in_pov_a - pop_in_pov_b) / 1e6
        )
    ]

    any_agg_diff <- any(abs(cmp$diff_headcount) > 1e-12 | abs(cmp$diff_pop_mill) > 1e-9, na.rm = TRUE)

    data.table(
        `Group Pair`                 = sprintf("%s vs %s", col_a, col_b),
        `Share Equal (%)`            = round(share_equal_pct, 2),
        `Max Unique (B per A)`       = max_unique_B_per_A,
        `Max Unique (A per B)`       = max_unique_A_per_B,
        `Any Aggregation Difference` = any_agg_diff
    )
}

# Define the four pairs
pairs <- list(
    c("region_name",         "region_old"),
    c("incgroup_historical", "incgroup_current"),
    c("fcv_historical",      "fcv_current"),
    c("ida_historical",      "ida_current")
)

# Build summary table
summary_table <- rbindlist(lapply(pairs, function(p) pair_summary(country_data, p[1], p[2])))

# View it
summary_table

# install.packages("gt")  # if needed
library(gt)

summary_gt <- summary_table |>
    gt() |>
    fmt_number(columns = "Share Equal (%)", decimals = 2) |>
    cols_label(
        `Group Pair`                 = "Group Pair",
        `Share Equal (%)`            = "Share Equal (%)",
        `Max Unique (B per A)`       = "Max Unique (B per A)",
        `Max Unique (A per B)`       = "Max Unique (A per B)",
        `Any Aggregation Difference` = "Any Aggregation Difference"
    )

summary_gt
