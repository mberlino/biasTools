#' Bias Analysis for Publication Bias Detection
#'
#' Performs Egger-like publication bias analysis across all levels of a selected moderator within a specific realm.
#' Optionally returns the IDs of outlier observations to be removed.
#'
#' @param dat Dataframe containing the meta-analysis dataset.
#' @param realm_name Name of the realm to analyze (e.g., "freshwater", "marine", etc.).
#' @param moderator_col Column name of the moderator variable (e.g., "habitat", "stressor").
#' @param moderator_label Optional. Display name for the moderator (e.g., "Habitat").
#' @param id_col Column used to identify each row or case (e.g., "row_id" for row-level, "paper_count" for case-level).
#' @param only_change Logical. If TRUE (default), only considers outliers where publication bias significance changes.
#' @param return_outliers Logical. If TRUE (default), returns IDs of detected outliers.
#'
#' @return A list with:
#' \describe{
#'   \item{results}{A dataframe with statistics for each moderator level.}
#'   \item{outliers}{(Optional) A vector of IDs (from `id_col`) corresponding to outlier rows.}
#' }
#'
#' @details
#' The function runs three models per moderator level:
#' \itemize{
#'   \item \code{a_mod}: tests asymmetry using ~sqrt(vi)
#'   \item \code{b_mod}: baseline model including all data
#'   \item \code{g_mod}: baseline model after outlier removal
#' }
#' Outliers are determined by standardized residuals > |3| in model b.
#'
#'#' Output of the function are organized in a  table as follow:
#'\itemize{
#'  \item \code{Moderator}: name od the selected moderator (es. "habitat")
#'  \item \code{level}: levels of the moderator (es. "lake", "stream"...)
#'  \item \code{n}: number od initial observation
#'  \item \code{g}`: estimate (effect size)
#'  \item \code{CI_95_before}: confidence interval al 95% before outliers exclusion
#'  \item \code{z_value}: z-score for asimmetry (Egger test)
#'  \item \code{p_value}: p-value for asimmetry
#'  \item \code{Outliers}:number of outliers identified
#'  \item \code{n_after}: number of observation without outliers
#'  \item \code{g_after}:new estimate for the model without outliers
#'  \item \code{CI_95_after}: new confidence interval
#'  \item \code{Change}: Indicate whether there are differences in significance levels between the initial and final models ("Yes" or "No")
#' }
#'
#' @examples
#' # Esempio di uso base:
#' dat$row_id <- 1:nrow(dat)
#' result <- bias.analysis(dat, "freshwater", "habitat", id_col = "row_id")
#'
#' # Output examples:
#' #   Moderator   Habitat    n      g   CI_95_before  z_value  p_value  Outliers  n_after  g_after  CI_95_after  Change
#' #   Habitat     lake       120  0.21      0.13         2.91     0.004     5         115      0.19     0.12         No
#' #   Habitat     stream     300 -0.02      0.11         -0.45    0.65      8         292     -0.01     0.10         No
#' 
#' dat_clean <- dat[!(dat$row_id %in% res$outliers), ]
#'
#' @export
bias.analysis <- function(dat, realm_name, moderator_col, 
                          moderator_label = NULL, id_col = "paper_count", 
                          only_change = TRUE, return_outliers = TRUE) {
  if (is.null(moderator_label)) moderator_label <- tools::toTitleCase(moderator_col)

  cat("\n\n▶️ Analisi per realm:", realm_name, "- moderatore:", moderator_col, "\n")

  sub_dat <- dat[dat$realm == realm_name & complete.cases(dat[[moderator_col]]), ]
  levels_mod <- sort(unique(na.omit(sub_dat[[moderator_col]])), decreasing = FALSE)

  righe <- list()
  outlier_ids <- c()

  for (lev in levels_mod) {
    cat("\n🔸 Livello:", lev, "\n")

    data_lev <- sub_dat[sub_dat[[moderator_col]] == lev, ]
    if (nrow(data_lev) < 3) {
      cat("⏭️  Skip: meno di 3 osservazioni\n")
      next
    }

    data_lev$id <- 1:nrow(data_lev)

    mod_a <- tryCatch({
      rma.mv(yi, vi, mods = ~sqrt(vi), random = ~1 | id, data = data_lev, method = "ML", sparse = TRUE)
    }, error = function(e) {
      cat("❌ Errore modello a:", e$message, "\n"); return(NULL)
    })
    if (is.null(mod_a)) next

    mod_b <- tryCatch({
      rma.mv(yi, vi, random = ~1 | paper, data = data_lev, method = "ML", sparse = TRUE)
    }, error = function(e) {
      cat("❌ Errore modello b:", e$message, "\n"); return(NULL)
    })
    if (is.null(mod_b)) next

    resid_vals <- tryCatch({
      rstandard.rma.mv(mod_b)$resid
    }, error = function(e) {
      cat("❌ Errore calcolo residui:", e$message, "\n"); return(rep(0, nrow(data_lev)))
    })

    data_with_resid <- cbind(data_lev, resid_vals)
    data_no_out <- data_with_resid[resid_vals > -3 & resid_vals < 3, ]

    ids_all <- data_with_resid[[id_col]]
    ids_keep <- data_no_out[[id_col]]
    ids_out <- setdiff(ids_all, ids_keep)

    data_no_out$id <- 1:nrow(data_no_out)

    mod_g <- tryCatch({
      rma.mv(yi, vi, random = ~1 | paper, data = data_no_out, method = "ML", sparse = TRUE)
    }, error = function(e) {
      cat("❌ Errore modello g:", e$message, "\n"); return(NULL)
    })
    if (is.null(mod_g)) next

    riga <- check.bias(
      moderator_name = moderator_label,
      moderator_level = lev,
      a_mod = mod_a,
      b_mod = mod_b,
      g_mod = mod_g,
      data_before = data_lev,
      data_after = data_no_out
    )

    righe[[length(righe) + 1]] <- riga

    if (!return_outliers) next
    if (!only_change || riga$Change == "Yes") {
      outlier_ids <- c(outlier_ids, ids_out)
    }
  }

  if (length(righe) == 0) {
    cat("⚠️ Nessun risultato valido per realm:", realm_name, "\n")
    return(NULL)
  }

  result <- do.call(rbind, righe)
  result[[moderator_label]] <- factor(result[[moderator_label]], levels = levels_mod)

  if (return_outliers) {
    return(list(results = result, outliers = unique(outlier_ids)))
  } else {
    return(list(results = result))
  }
}
