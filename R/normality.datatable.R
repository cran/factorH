#' Raw normality per subgroup (Shapiro–Wilk) across factor combinations
#'
#' Runs Shapiro–Wilk tests on the raw response within each subgroup for all non-empty
#' combinations of RHS factors (main effects and interaction cells).
#'
#' @param formula A model formula \code{y ~ A + B (+ C ...)}.
#' @param data A data frame with the variables.
#' @param force_factors Logical; if TRUE, coerces RHS predictors to factors.
#'
#' @return A data.frame with rows per subgroup/cell. Columns: \code{Effect}, factor columns,
#' \code{count}, \code{W}, \code{p.shapiro} (4 decimals), \code{OK}.
#'
#' @examples
#' \dontrun{
#' normality.datatable(liking ~ gender + condition + age_cat, data = mimicry)
#' }
#' @importFrom stats shapiro.test
#' @seealso \code{\link{plan.diagnostics}}
#' @export
normality.datatable <- function(formula, data, force_factors = TRUE) {
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

  summarize_for <- function(grp_vars) {
    eff <- paste(grp_vars, collapse = ":")
    sp <- split(d, d[grp_vars], drop = TRUE)

    rows <- lapply(sp, function(df_g) {
      x <- df_g[[resp]]
      n <- length(x)

      if (n >= 3L && n <= 5000L) {
        st <- try(stats::shapiro.test(x), silent = TRUE)
        if (inherits(st, "try-error")) {
          W <- NA_real_; p_raw <- NA_real_
        } else {
          W <- unname(st$statistic); p_raw <- st$p.value
        }
      } else {
        W <- NA_real_; p_raw <- NA_real_
      }

      # format p bez notacji naukowej, max 4 miejsca po przecinku
      p_fmt <- if (is.na(p_raw)) NA_character_ else formatC(p_raw, format = "f", digits = 4)

      # decyzja OK / NOT OK (p < 0.05 => NOT OK)
      ok_col <- if (is.na(p_raw)) NA_character_ else if (p_raw < 0.05) "NOT OK" else "OK"

      # komplet kolumn czynników
      fac_cols <- setNames(vector("list", length(facs)), facs)
      for (f in facs) {
        if (f %in% grp_vars) {
          val <- df_g[[f]][1]
          fac_cols[[f]] <- factor(val, levels = levels(d[[f]]))
        } else {
          fac_cols[[f]] <- factor(NA, levels = levels(d[[f]]))
        }
      }

      as.data.frame(c(list(Effect = eff), fac_cols,
                      list(count = n, W = W, p.shapiro = p_fmt, OK = ok_col)),
                    optional = TRUE, stringsAsFactors = FALSE)
    })

    do.call(rbind, rows)
  }

  combos <- unlist(lapply(seq_along(facs), function(k)
    utils::combn(facs, k, simplify = FALSE)), recursive = FALSE)

  out <- do.call(rbind, lapply(combos, summarize_for))
  rownames(out) <- NULL

  # kolumny już są w kolejności: Effect, facs..., count, W, p.shapiro, OK
  attr(out, "call") <- mc
  out
}
