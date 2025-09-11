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
underscore_var <- function(cn) {
    gsub(pattern = " ", replacement = "_", x = cn)
}


# my_theme <- function(base_size = 11, legend = c("bottom", "none", "right")) {
#     legend <- match.arg(legend)
#     theme_minimal(base_size = base_size) +
#         theme(
#             legend.position  = legend,
#             legend.title     = element_blank(),
#             legend.text      = element_text(size = 8),
#             legend.key.size  = unit(0.3, "cm"),
#             legend.spacing.x = unit(0.1, "cm"),
#             plot.margin      = margin(2, 2, 2, 2)
#         )
# }


# WBG Theme using wbplot()

my_theme <- function(base_size = 11, legend = "bottom") {
    legend <- match.arg(legend)
    wbplot::theme_wb() +
        theme(
            legend.position   = legend,
            legend.title      = element_blank(),
            legend.text       = element_text(size = 8),
            legend.key.size   = unit(0.5, "cm"),
            legend.key.width  = unit(0.5, "cm"),
            legend.spacing.x     = unit(1, "mm"),     # tighter horizontal spacing
            legend.spacing.y     = unit(1, "mm"),      # vertical between rows
            legend.margin     = margin(0.1, 0.1, 0.1, 0.1, "cm"),
            plot.margin       = margin(1, 1, 1, 1, "cm"),
            panel.background  = element_blank(),
            plot.background   = element_blank(),
            panel.grid.major  = element_line(color = "#E6E6E6", linewidth = 0.25),
            panel.grid.minor  = element_blank()
        )
}


# Color palette set up

# 1) Manually set up color palette
wb_region_colors <- function() {
    c(
        "East Asia & Pacific"           = "#F3578E",
        "Europe & Central Asia"         = "#AA0000",
        "Latin America & Caribbean"     = "#0C7C68",
        "Middle East & North Africa"    = "#664AB6",
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


# 3) Auto-switch helper based on group_var
add_group_palette <- function(aesthetic = c("color", "fill"), group_var) {
    aesthetic <- match.arg(aesthetic)
    gv <- underscore_var(group_var)

    if (gv %in% c("region_name", "region_old")) {
        if (aesthetic == "color") scale_region_color_manual()
        else                      scale_region_fill_manual()
    }

    else if (gv %in% c("incgroup_historical", "incgroup_current")) {
        if (aesthetic == "color") scale_income_color_manual()
        else                      scale_income_fill_manual()
    }

    else {
        if (aesthetic == "color") wbplot::scale_color_wb_d()
        else                      wbplot::scale_fill_wb_d()
    }
}




