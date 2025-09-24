#' Replace underscores with spaces
#'
#' @param cn Character vector of names.
#' @return Character vector with `"_"` replaced by `" "`.
#' @examples
#' space_var(c("region_name", "fcv_current"))
#' @export
space_var <- function(cn) {
    gsub(pattern = "_", replacement = " ", x = cn)
}

#' Replace spaces with underscores
#'
#' @param cn Character vector of names.
#' @return Character vector with `" "` replaced by `"_"`.
#' @examples
#' underscore_var(c("region name", "fcv current"))
#' @export

# underscore_var <- function(cn) {
#     gsub(pattern = " ", replacement = "_", x = cn)
# }

underscore_var <- function(cn) {
    if (is.null(cn)) return(NULL)

    dplyr::recode(cn,
                  "Region (new WB classification)" = "region_name",
                  "Region (old PovcalNet classification)"     = "region_old",
                  "Region (new WB classification excl. HICs)" = "region_WDI",
                  "Income group (historical)" = "incgroup_historical",
                  "Income group (latest)"    = "incgroup_current",
                  "FCV (historical)"      = "fcv_historical",
                  "FCV (latest)"         = "fcv_current",
                  "IDA (historical)"      = "ida_historical",
                  "IDA (latest)"         = "ida_current",
                  .default = gsub(" ", "_", cn))
}



my_theme <- function(by,
                     base_size   = 5,
                     legend      = c("bottom", "top", "left", "right", "none"),
                     drop        = FALSE,
                     legend_nrow = 4) {
    legend <- match.arg(legend)

    # Map `by` to region / income / other
    gv <- tryCatch(underscore_var(by), error = function(e) by)
    group <- if (gv %chin% c("incgroup_historical", "incgroup_current", "income")) {
        "income"
    } else if (gv %chin% c("region_name", "region_old", "region_WDI", "region")) {
        "region"
    } else {
        "other"
    }

    parts <- list(
        theme_wb(chartType     = "line",
                 addYAxisTitle = TRUE),
        theme(
            text              = element_text(size = base_size),
            legend.position   = legend,
            legend.title      = element_blank(),
            legend.text       = element_text(size = 3.5, margin = margin(r = 6)),
            legend.key.size   = unit(0.5, "cm"),
            legend.key.width  = unit(0.5, "cm"),
            legend.spacing.x  = unit(2, "mm"),
            legend.spacing.y  = unit(1, "mm"),
            legend.margin     = margin(0.1, 0.1, 0.1, 0.1, "cm"),
            plot.margin       = margin(1, 1, 1, 1, "cm"),
            panel.background  = element_blank(),
            plot.background   = element_blank(),
            panel.grid.major.x   = element_line(color = "#E6E6E6", linewidth = 0.25),  # ✅ vertical
            panel.grid.major.y   = element_line(color = "#E6E6E6", linewidth = 0.25),  # ✅ horizontal
            panel.grid.minor  = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)

        ),
        scale_color_wb_d(),
        scale_fill_wb_d(),
        scale_linetype_manual(
            values = c("≥50% coverage" = "solid", "<50% coverage" = "longdash"),
            breaks = c("≥50% coverage", "<50% coverage"),
            guide  = guide_legend(title = NULL)
        ),
        guides(
            color    = guide_legend(nrow = legend_nrow, byrow = TRUE,
                                    label.theme = element_text(margin = margin(r = 40))),
            fill     = guide_legend(nrow = legend_nrow, byrow = TRUE,
                                    label.theme = element_text(margin = margin(r = 40))),
            linetype = guide_legend(nrow = legend_nrow, byrow = TRUE,
                                    label.theme = element_text(margin = margin(r = 40)))
        )

    )

    parts <- c(parts, list(
        scale_x_continuous(
            limits = c(1981, 2025),
            breaks = unique(c(1981, pretty(1981:2025, n = 6), 2025)),
            expand = expansion(mult = c(0, 0))
        )
    ))

    # Override with manual palettes for region / income
    if (group == "region") {
        parts <- c(parts, list(
            scale_region_color_manual(drop = drop),
            scale_region_fill_manual(drop = drop)
        ))
    } else if (group == "income") {
        parts <- c(parts, list(
            scale_income_color_manual(drop = drop),
            scale_income_fill_manual(drop = drop)
        ))
    }

    parts
}




# Color palette set up

# 1) Manually set up color palette
wb_region_colors <- function() {
    c(
        "East Asia & Pacific"                                                = "#F3578E",
        "East Asia & Pacific (excl. HICs)"                               = "#F3578E",
        "Europe & Central Asia"                                              = "#AA0000",
        "Europe & Central Asia (excl. HICs)"                             = "#AA0000",
        "Latin America & Caribbean"                                          = "#0C7C68",
        "Latin America & Caribbean (excl. HICs)"                         = "#0C7C68",
        "Middle East & North Africa"                                         = "#664AB6",
        "Middle East, North Africa, Afghanistan & Pakistan"                  = "#664AB6",
        "Middle East, North Africa, Afghanistan & Pakistan (excl. HICs)" = "#664AB6",
        "Other High Income Countries"                                        = "#34A7F2",
        "North America"                                                      = "#34A7F2",
        "South Asia"                                                         = "#4EC2C0",
        "South Asia (excl. HICs)"                                        = "#4EC2C0",
        "Sub-Saharan Africa"                                                 = "#FF9800",
        "Sub-Saharan Africa (excl. HICs)"                                = "#FF9800"
    )
}

wb_income_colors <- function() {
    c(
        "Low income"          = "#3B4DA6",
        "Lower middle income" = "#DB95D7",
        "Upper middle income" = "#73AF48",
        "High income"         = "#016B6C"
    )
}


# 2) Set up manual scale functions for use in plots
scale_region_color_manual <- function(drop = FALSE) {
    scale_color_manual(values = wb_region_colors(), drop = drop)
}

scale_region_fill_manual <- function(drop = FALSE) {
    scale_fill_manual(values = wb_region_colors(), drop = drop)
}

scale_income_color_manual <- function(drop = FALSE) {
    scale_color_manual(values = wb_income_colors(), drop = drop)
}

scale_income_fill_manual <- function(drop = FALSE) {
    scale_fill_manual(values = wb_income_colors(), drop = drop)
}


# Coverage set up

region_coverage_flag <- function(dt, group_var = "region_name") {
    dt <- as.data.table(dt)
    group_var <- underscore_var(group_var)

    # Dynamically reference group_var column
    tot_pop_dt <- dt[, .(tot_pop = sum(pop, na.rm = TRUE)), by = c(group_var, "year")]
    cov_pop_dt <- dt[coverage == TRUE, .(cov_pop = sum(pop, na.rm = TRUE)), by = c(group_var, "year")]

    cov_flag_dt <- merge(tot_pop_dt, cov_pop_dt, by = c(group_var, "year"), all.x = TRUE)
    cov_flag_dt[, pop_share := cov_pop / tot_pop]
    cov_flag_dt[, cov := as.integer(pop_share >= 0.5)]

    # Return only needed columns
    cov_flag_dt[, .SD, .SDcols = c(group_var, "year", "cov")]
}




