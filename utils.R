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
                     legend_nrow = 3) {
    legend <- match.arg(legend)

    # Map `by` to region / income / other
    gv <- tryCatch(underscore_var(by), error = function(e) by)
    group <- if (gv %chin% c("incgroup_historical", "incgroup_current", "income")) {
        "income"
    } else if (gv %chin% c("region_name", "region_old", "region")) {
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
            legend.text       = element_text(size = 5),
            legend.key.size   = unit(0.5, "cm"),
            legend.key.width  = unit(0.5, "cm"),
            legend.spacing.x  = unit(1, "mm"),
            legend.spacing.y  = unit(1, "mm"),
            legend.margin     = margin(0.1, 0.1, 0.1, 0.1, "cm"),
            plot.margin       = margin(1, 1, 1, 1, "cm"),
            panel.background  = element_blank(),
            plot.background   = element_blank(),
            panel.grid.major  = element_line(color = "#E6E6E6", linewidth = 0.25),
            panel.grid.minor  = element_blank()
        ),
        scale_color_wb_d(),
        scale_fill_wb_d(),
        guides(
            color = guide_legend(nrow = legend_nrow, byrow = TRUE),
            fill  = guide_legend(nrow = legend_nrow, byrow = TRUE)
        )
    )

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
        "East Asia & Pacific"           = "#F3578E",
        "Europe & Central Asia"         = "#AA0000",
        "Latin America & Caribbean"     = "#0C7C68",
        "Middle East & North Africa"    = "#664AB6",
        "Middle East, North Africa, Afghanistan & Pakistan" = "#664AB6",
        "Other High Income Countries"   = "#34A7F2",
        "South Asia"                    = "#4EC2C0",
        "Sub-Saharan Africa"            = "#FF9800"
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





