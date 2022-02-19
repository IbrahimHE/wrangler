#' @title Automatically create faceted plot with ggplot2
#' @description This function Automatically create faceted plot with ggplot2
#' @param variable specify columns
#' @param group specify a variable to be faceted with
#' @param title title of the plot
#' @param dat specify dataframe
#' @returns returns a faceted plot
#' @export
#' @import ggplot2
facet_plots <- function(variable, group, title = "Title", dat) {
  pct <- c()
  lbl <- c()
  variable <- sym(variable)
  group <- sym(group)
  sumdat <- dat %>%
    filter(!is.na(!!variable)) %>%
    group_by(!!group) %>%
    add_count() %>%
    mutate(lbl = paste0(!!group, " (N = ", n, ")")) %>%
    group_by(!!group, !!variable) %>%
    mutate(pct = 100 * n() / n) %>%
    slice(1L) %>%
    ungroup() %>%
    select(!!variable, !!group, n, pct, lbl)

  ggplot(sumdat, aes(x = !!variable, y = pct, group = !!group)) +
    geom_histogram(stat = "identity") +
    labs(
      title = title
    ) +
    facet_grid(~lbl, scales = "free")
}
