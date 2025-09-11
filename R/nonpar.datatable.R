#' Compact descriptive tables (APA-style) with global rank means
#'
#' Produces descriptive statistics for all main effects and interaction cells
#' implied by the RHS of \code{formula}. Ranks are computed \emph{globally}
#' (across all observations) and cell-wise mean ranks are reported
#' (\strong{recommended} for interpreting rank-based factorial effects).
#'
#' @param formula A formula of the form \code{y ~ A (+ B + ...)}.
#' @param data A \code{data.frame} containing \code{y} and the grouping factors.
#' @param force_factors Logical; coerce grouping variables to \code{factor}
#'   (default \code{TRUE}).
#'
#' @details
#' The function first subsets to complete cases on \code{y} and all RHS factors,
#' then computes global ranks of \code{y} (\code{ties.method = "average"}).
#' For each effect (every non-empty combination of factors up to full order),
#' it returns a row per cell with:
#' \code{count}, \code{mean}, \code{sd}, \code{median}, quartiles
#' (\code{q1}, \code{q3}), \code{IQR}, and \code{mean_rank}.
#' The column \code{Effect} identifies the effect (e.g., \code{"A"}, \code{"B"},
#' \code{"A:B"}). Missing factor columns for a given effect are added with
#' \code{NA} values but retain the proper factor levels for easy binding.
#'
#' @return A base \code{data.frame} with columns:
#' \itemize{
#'   \item \code{Effect} (character),
#'   \item factor columns for all RHS factors (factors, possibly \code{NA} in some rows),
#'   \item \code{count}, \code{mean}, \code{sd}, \code{median}, \code{q1}, \code{q3}, \code{IQR}, \code{mean_rank}.
#' }
#' The original call is attached as attribute \code{"call"}.
#'
#' @examples
#' data(mimicry, package = "factorH")
#'
#' # One factor
#' nonpar.datatable(liking ~ condition, data = mimicry)
#'
#' # Two factors: rows for gender, for condition, and for gender:condition
#' nonpar.datatable(liking ~ gender + condition, data = mimicry)
#'
#' # Three factors: all mains + 2-way and 3-way cells
#' nonpar.datatable(liking ~ gender + condition + age_cat, data = mimicry)
#'
#' @export
#' @importFrom stats complete.cases terms median IQR quantile sd
#' @importFrom dplyr %>% group_by summarise mutate relocate across all_of bind_rows n
#' @importFrom utils combn
#' @importFrom rlang .data
nonpar.datatable <- function(formula, data, force_factors = TRUE) {
  stopifnot(inherits(formula, "formula"))
  mc   <- match.call()

  vars <- all.vars(formula)
  resp <- vars[1]
  facs <- attr(stats::terms(formula), "term.labels")
  if (length(facs) < 1L) stop("Provide at least one factor on the RHS of '~'.")
  if (!is.numeric(data[[resp]]))
    stop("The response '", resp, "' must be numeric.")

  needed <- c(resp, facs)
  cc <- stats::complete.cases(data[, needed, drop = FALSE])
  d  <- droplevels(data[cc, needed, drop = FALSE])

  if (force_factors) {
    for (nm in facs) if (!is.factor(d[[nm]])) d[[nm]] <- factor(d[[nm]])
  }

  d$.R <- base::rank(d[[resp]], ties.method = "average")

  summarize_for <- function(grp_vars) {
    eff <- paste(grp_vars, collapse = ":")
    res <- d %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars))) %>%
      dplyr::summarise(
        count     = dplyr::n(),
        mean      = mean(.data[[resp]], na.rm = TRUE),
        sd        = stats::sd(.data[[resp]], na.rm = TRUE),
        median    = stats::median(.data[[resp]], na.rm = TRUE),
        q1        = stats::quantile(.data[[resp]], 0.25, na.rm = TRUE),
        q3        = stats::quantile(.data[[resp]], 0.75, na.rm = TRUE),
        IQR       = stats::IQR(.data[[resp]], na.rm = TRUE),
        mean_rank = mean(.data$.R, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(Effect = eff)

    # add missing factor columns (with proper levels) for binding
    missing <- setdiff(facs, grp_vars)
    if (length(missing) > 0L) {
      for (m in missing) res[[m]] <- factor(NA, levels = levels(d[[m]]))
    }

    res %>% dplyr::relocate(Effect, dplyr::all_of(facs))
  }

  # all non-empty combinations of factors
  combos <- unlist(lapply(seq_along(facs), function(k)
    utils::combn(facs, k, simplify = FALSE)), recursive = FALSE)

  out <- dplyr::bind_rows(lapply(combos, summarize_for))
  out <- as.data.frame(out)

  attr(out, "call") <- mc
  out
}
