
#' Check Bias Result for a Moderator Level
#'
#' Extracts summary statistics from models used to detect publication bias.
#' Compares results before and after outlier removal.
#'
#' @param moderator_name Character string for the moderator name (e.g., "Habitat").
#' @param moderator_level Character string for the specific level of the moderator (e.g., "Lake").
#' @param a_mod Model testing asymmetry (Egger-style regression).
#' @param b_mod Model before outlier removal.
#' @param g_mod Model after outlier removal.
#' @param data_before Original dataset before removing outliers.
#' @param data_after Dataset after removing outliers.
#'
#' @return A one-row data frame with publication bias diagnostics.
#' @export
check.bias <- function(moderator_name, moderator_level, a_mod, b_mod, g_mod, data_before, data_after) {
  z_val <- round(a_mod$zval[1], 4)
  p_val <- round(a_mod$pval[1], 4)

  g_before <- round(as.numeric(b_mod$b), 4)
  ci_before <- round(b_mod$se * 1.96, 4)

  g_after <- round(as.numeric(g_mod$b), 4)
  ci_after <- round(g_mod$se * 1.96, 4)

  n_before <- nrow(data_before)
  n_after <- nrow(data_after)
  n_outliers <- n_before - n_after

  significativo <- function(p) !is.na(p) && p < 0.05
  change <- ifelse(significativo(b_mod$pval[1]) != significativo(g_mod$pval[1]), "Yes", "No")

  out <- data.frame(
    Moderator = moderator_name,
    n = n_before,
    g = g_before,
    CI_95_before = ci_before,
    z_value = z_val,
    p_value = p_val,
    Outliers = n_outliers,
    n_after = n_after,
    g_after = g_after,
    CI_95_after = ci_after,
    Change = change
  )

  out[[moderator_name]] <- moderator_level
  out <- out[, c("Moderator", moderator_name, setdiff(names(out), c("Moderator", moderator_name)))]

  return(out)
}
