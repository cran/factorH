#' Plan-level diagnostics for ANOVA/rank-based workflows
#'
#' Runs all assumption checks in one call: raw normality per subgroup (Shapiro-Wilk),
#' residual normality per cell (from a full-factorial ANOVA on the specified factors),
#' Levene/Brown-Forsythe for the full plan (median by default), and count-balance
#' chi-square tests for all factor combinations. Prints a concise summary and returns
#' all detailed tables in a list.
#'
#' @param formula A model formula of the form \code{y ~ A + B (+ C ...)}.
#' @param data A data frame containing the variables in the model.
#' @param force_factors Logical; if TRUE, coerces RHS predictors to factors.
#'
#' @return An invisible list with:
#' \itemize{
#' \item \code{$summary}: overall percent_ok, ok_count, total, overall, plus per-type percentages
#'       (\code{percent_ok_normality_raw}, \code{percent_ok_residuals_cellwise},
#'       \code{percent_ok_balance_chisq}, \code{percent_ok_levene_full_plan}).
#' \item \code{$results}: data.frames for \code{normality_raw},
#'       \code{residuals_cellwise_normality}, \code{levene_full_plan}, \code{balance_chisq}.
#' }
#'
#' @details Requires helper functions defined in this package:
#' \code{normality.datatable}, \code{residuals.cellwise.normality.datatable},
#' \code{levene.plan.datatable}, \code{balance.chisq.datatable}.
#' Levene's test uses \pkg{car}; if unavailable, the Levene block returns NA rows with a warning.
#'
#' @examples
#' \dontrun{
#' diag_out <- plan.diagnostics(liking ~ gender + condition + age_cat, data = mimicry)
#' diag_out$summary
#' diag_out$results$normality_raw
#' }
#'
#' @seealso \code{\link{normality.datatable}},
#'   \code{\link{residuals.cellwise.normality.datatable}},
#'   \code{\link{levene.plan.datatable}},
#'   \code{\link{balance.chisq.datatable}}
#' @export
plan.diagnostics <- function(formula, data, force_factors = TRUE) {
  # 0) basic validation
  if (!inherits(formula, "formula")) stop("Argument 'formula' must be of class 'formula'.")
  vars <- all.vars(formula)
  if (length(vars) < 2L) stop("The formula must contain a response and at least one factor.")
  resp <- vars[1]
  facs <- attr(stats::terms(formula), "term.labels")
  if (length(facs) < 1L) stop("Provide at least one factor on the right-hand side of '~'.")
  if (!resp %in% names(data)) stop("Response variable '", resp, "' not found in 'data'.")
  if (!all(facs %in% names(data))) {
    miss <- facs[!facs %in% names(data)]
    stop("Missing columns in 'data': ", paste(miss, collapse = ", "))
  }
  if (!is.numeric(data[[resp]])) stop("The response '", resp, "' must be numeric.")

  # 0b) check helpers/packages
  need_fun <- c("normality.datatable",
                "residuals.cellwise.normality.datatable",
                "levene.plan.datatable",
                "balance.chisq.datatable")
  for (fn in need_fun) if (!exists(fn, mode = "function"))
    stop("Helper function not found: ", fn, ". Please define it in the environment.")
  if (!requireNamespace("car", quietly = TRUE)) {
    warning("Package 'car' is not installed - Levene (Brown-Forsythe) may fail.")
  }

  # run diagnostics
  res_norm_raw   <- normality.datatable(formula, data, force_factors = force_factors)
  res_norm_cells <- residuals.cellwise.normality.datatable(formula, data, force_factors = force_factors)
  res_levene <- tryCatch(
    levene.plan.datatable(formula, data, center = "median", force_factors = force_factors),
    error = function(e) {
      warning("Levene (Brown-Forsythe) failed: ", conditionMessage(e))
      data.frame(Effect = paste(facs, collapse = ":"),
                 n.groups = NA_integer_, min.n = NA_integer_,
                 df.num = NA_integer_, df.den = NA_integer_,
                 F = NA_character_, p = NA_character_, OK = NA_character_,
                 stringsAsFactors = FALSE)
    }
  )
  res_balance <- balance.chisq.datatable(formula, data, force_factors = force_factors)

  # helper: percentage OK (NA treated as NOT OK)
  perc_ok <- function(x) {
    if (length(x) == 0L) return(NA_real_)
    ok_cnt <- sum(x == "OK", na.rm = FALSE)
    round(100 * ok_cnt / length(x), 1)
  }

  # per-type percentages
  p_normality  <- perc_ok(res_norm_raw$OK)
  p_residuals  <- perc_ok(res_norm_cells$OK)
  p_balance    <- perc_ok(res_balance$OK)
  p_levene     <- perc_ok(res_levene$OK)

  # overall
  ok_vec <- c(res_norm_raw$OK, res_norm_cells$OK, res_levene$OK, res_balance$OK)
  total  <- length(ok_vec)
  ok_cnt <- sum(ok_vec == "OK", na.rm = FALSE)
  percent_ok_overall <- if (total > 0) round(100 * ok_cnt / total, 1) else NA_real_
  overall_status <- if (is.na(percent_ok_overall)) NA_character_ else if (percent_ok_overall == 100) "OK" else "NOT OK"

  # console output
  cat("\n=== PLAN DIAGNOSTICS SUMMARY ===\n")
  cat("1) Overall share of OK: ",
      if (is.na(percent_ok_overall)) "NA" else paste0(percent_ok_overall, "%"),
      " (", ok_cnt, "/", total, ")\n", sep = "")
  cat("2) Overall status: ", overall_status, " (condition: 100% OK)\n", sep = "")
  cat("3) Full result tables are returned in the list (see $results)\n")

  # return
  out <- list(
    summary = list(
      percent_ok = percent_ok_overall,
      ok_count   = ok_cnt,
      total      = total,
      overall    = overall_status,
      # added per-type fields:
      percent_ok_normality_raw      = p_normality,
      percent_ok_residuals_cellwise = p_residuals,
      percent_ok_balance_chisq      = p_balance,
      percent_ok_levene_full_plan   = p_levene
    ),
    results = list(
      normality_raw                = res_norm_raw,
      residuals_cellwise_normality = res_norm_cells,
      levene_full_plan             = res_levene,
      balance_chisq                = res_balance
    ),
    call = match.call()
  )
  invisible(out)
}