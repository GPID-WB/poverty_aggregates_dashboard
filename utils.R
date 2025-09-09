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


my_theme <- function(base_size = 11, legend = c("bottom", "none", "right")) {
    legend <- match.arg(legend)
    theme_minimal(base_size = base_size) +
        theme(
            legend.position  = legend,
            legend.title     = element_blank(),
            legend.text      = element_text(size = 8),
            legend.key.size  = unit(0.3, "cm"),
            legend.spacing.x = unit(0.1, "cm"),
            plot.margin      = margin(2, 2, 2, 2)
        )
}
