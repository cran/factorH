#' Full pipeline: rank-based k-way ANOVA + descriptives + post hocs
#'
#' Runs a complete nonparametric, rank-based workflow for factorial designs:
#' (1) SRH-style ANOVA table, (2) compact descriptive stats with global ranks,
#' (3) Dunn-Bonferroni post hoc matrices for all effects, and
#' (4) simple-effects post hocs (Bonferroni within each by-table).
#'
#' Choice of the ANOVA engine:
#' \itemize{
#'   \item 1 factor: \code{srh.kway()} (KW-like),
#'   \item 2 factors: \code{srh.effsize()} (SRH 2-way + effect sizes),
#'   \item 3+ factors: \code{srh.kway()} (general k-way on ranks).
#' }
#'
#' @param formula A formula \code{y ~ A (+ B + ...)}.
#' @param data A \code{data.frame} with variables present in \code{formula}.
#' @param max_levels Safety cap for number of levels per factor (default 30).
#'
#' @return A list with elements:
#' \itemize{
#'   \item \code{anova} – ANOVA-like table,
#'   \item \code{summary} – descriptive stats data.frame,
#'   \item \code{posthoc_cells} – list of p.adj matrices for all effects
#'         (from \code{srh.posthocs}), or a string when failed,
#'   \item \code{posthoc_simple} – list of simple-effect tables
#'         (from \code{srh.simple.posthocs}); for 1 factor: \code{"[not applicable]"},
#'   \item \code{meta} – list with call, n, factor levels, and empty-cell info (if 2+ factors).
#' }
#' Components that cannot be computed for the given design are returned as the
#' string \code{"[not applicable]"}; failures are reported as \code{"[failed] <message>"}.
#'
#' @examples
#' data(mimicry, package = "factorH")
#' # 1 factor
#' f1 <- srh.kway.full(liking ~ condition, data = mimicry)
#' # 2 factors
#' f2 <- srh.kway.full(liking ~ gender + condition, data = mimicry)
#' # 3 factors
#' f3 <- srh.kway.full(liking ~ gender + condition + age_cat, data = mimicry)
#'
#' @export
#' @importFrom stats terms complete.cases
srh.kway.full <- function(formula, data, max_levels = 30) {
  stopifnot(inherits(formula, "formula"))
  if (!is.data.frame(data)) stop("'data' must be a data.frame.")

  # --- parse and basic checks
  rhs <- attr(stats::terms(formula), "term.labels")
  kf  <- length(rhs)
  if (kf < 1L) stop("Provide at least one factor on the RHS of '~'.")

  vars <- all.vars(formula)
  resp <- vars[1]

  missing_cols <- setdiff(c(resp, rhs), names(data))
  if (length(missing_cols))
    stop("Missing variables in 'data': ", paste(missing_cols, collapse = ", "))

  d0 <- data[, c(resp, rhs), drop = FALSE]
  cc <- stats::complete.cases(d0)
  if (!any(cc))
    stop("No complete cases after subsetting to response and RHS factors.")
  d  <- droplevels(d0[cc, , drop = FALSE])

  if (!is.numeric(d[[resp]]))
    stop("Response variable '", resp, "' must be numeric.")

  # ensure factors + level counts
  for (nm in rhs) if (!is.factor(d[[nm]])) d[[nm]] <- factor(d[[nm]])
  lev_counts <- vapply(rhs, function(z) nlevels(d[[z]]), integer(1))
  too_few    <- names(lev_counts)[lev_counts < 2L]
  if (length(too_few))
    stop("Factors with < 2 levels: ", paste(too_few, collapse = ", "),
         ". Add levels or remove the factor from the model.")
  too_many   <- names(lev_counts)[lev_counts > max_levels]
  if (length(too_many))
    stop("Factors with > ", max_levels, " levels: ",
         paste(too_many, collapse = ", "),
         ". Reduce categories or increase 'max_levels'.")

  # empty-cell info (only meaningful for 2+ factors)
  empty_info <- NULL
  if (kf >= 2L) {
    xt <- stats::xtabs(~ ., data = d[, rhs, drop = FALSE])
    empty_info <- list(
      dims       = sapply(dimnames(xt), length),
      n_cells    = length(xt),
      n_empty    = sum(xt == 0),
      prop_empty = if (length(xt)) mean(xt == 0) else NA_real_
    )
  }

  # helper to protect calls and return "[failed] <msg>" instead of stopping
  safe <- function(expr) {
    tryCatch(eval.parent(substitute(expr)),
             error = function(e) paste0("[failed] ", conditionMessage(e)))
  }

  # --- (1) ANOVA-like table
  anova_res <-
    if (kf == 1L) {
      safe(srh.kway(formula, data = d))
    } else if (kf == 2L) {
      # 2-way SRH with effect sizes
      safe(srh.effsize(formula, data = d))
    } else {
      # general k-way
      safe(srh.kway(formula, data = d))
    }

  # --- (2) Descriptive statistics (global ranks, quartiles, etc.)
  stats_res <- safe(nonpar.datatable(formula, data = d))
  if (is.character(stats_res)) {
    # failed -> leave the failure string
  } else {
    stats_res <- as.data.frame(stats_res)
  }

  # --- (3) Post hocs: all effects (p.adj matrices; lower triangle by default)
  posthoc_cells <- safe(srh.posthocs(formula, data = d))

  # --- (4) Simple effects (within-Bonferroni).
  posthoc_simple <-
    if (kf == 1L) {
      "[not applicable]"
    } else {
      safe(srh.simple.posthocs(formula, data = d))
    }

  out <- list(
    anova          = anova_res,
    summary        = stats_res,
    posthoc_cells  = posthoc_cells,
    posthoc_simple = posthoc_simple,
    meta = list(
      call         = match.call(),
      n            = nrow(d),
      levels       = lev_counts,
      empty_cells  = empty_info
    )
  )
  class(out) <- c("srh_kway_full", class(out))
  out
}
