#' Cellwise residual normality (Shapiro–Wilk) from ANOVA models
#'
#' Fits, for each subset of RHS factors, a full-factorial ANOVA to the response and
#' tests Shapiro–Wilk normality of residuals within each cell defined by those factors.
#' Matches the classical ANOVA assumption of normal errors per cell.
#'
#' @param formula A model formula \code{y ~ A + B (+ C ...)}.
#' @param data A data frame with the variables.
#' @param force_factors Logical; if TRUE, coerces RHS predictors to factors.
#'
#' @return A data.frame with rows per cell across all factor combinations. Columns include:
#' \code{Effect}, factor columns (with NA for factors not in the current subset),
#' \code{count}, \code{W}, \code{p.shapiro} (4 decimals), \code{OK}.
#'
#' @examples
#' \dontrun{
#' residuals.cellwise.normality.datatable(liking ~ gender + condition + age_cat, data = mimicry)
#' }
#' @importFrom stats lm update resid shapiro.test as.formula
#' @seealso \code{\link{normality.datatable}}, \code{\link{plan.diagnostics}}
#' @rawNamespace export(residuals.cellwise.normality.datatable)
residuals.cellwise.normality.datatable <- function(formula, data, force_factors = TRUE) {
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

  full_rhs_for <- function(grp_vars) {
    k <- length(grp_vars)
    if (k == 1L) {
      stats::as.formula(paste("~", grp_vars))
    } else {
      stats::as.formula(paste0("~ (", paste(grp_vars, collapse = " + "), ")^", k))
    }
  }

  summarize_for <- function(grp_vars) {
    eff <- paste(grp_vars, collapse = ":")

    # 1) pełny model dla tych czynników
    rhs <- full_rhs_for(grp_vars)
    fit <- stats::lm(stats::update(rhs, stats::as.formula(paste(resp, "~ ."))), data = d)

    # 2) reszty
    r <- stats::resid(fit)

    # 3) test Shapiro w każdej komórce (poziomy/grupy wg grp_vars)
    sp_idx <- split(seq_len(nrow(d)), d[grp_vars], drop = TRUE)

    rows <- lapply(sp_idx, function(idx) {
      rr <- r[idx]
      n  <- length(rr)

      if (n >= 3L && n <= 5000L) {
        st <- try(stats::shapiro.test(rr), silent = TRUE)
        if (inherits(st, "try-error")) {
          W <- NA_real_; p_raw <- NA_real_
        } else {
          W <- unname(st$statistic); p_raw <- st$p.value
        }
      } else {
        W <- NA_real_; p_raw <- NA_real_
      }

      p_fmt <- if (is.na(p_raw)) NA_character_ else formatC(p_raw, format = "f", digits = 4)
      ok_col <- if (is.na(p_raw)) NA_character_ else if (p_raw < 0.05) "NOT OK" else "OK"

      # z poziomów w d[grp_vars][idx[1], ] odczytujemy wartości komórki
      fac_vals <- setNames(vector("list", length(facs)), facs)
      for (f in facs) {
        if (f %in% grp_vars) {
          val <- d[[f]][idx[1]]
          fac_vals[[f]] <- factor(val, levels = levels(d[[f]]))
        } else {
          fac_vals[[f]] <- factor(NA, levels = levels(d[[f]]))
        }
      }

      as.data.frame(c(list(Effect = eff), fac_vals,
                      list(count = n, W = W, p.shapiro = p_fmt, OK = ok_col)),
                    optional = TRUE, stringsAsFactors = FALSE)
    })

    do.call(rbind, rows)
  }

  # wszystkie niepuste kombinacje czynników
  combos <- unlist(lapply(seq_along(facs), function(k)
    utils::combn(facs, k, simplify = FALSE)), recursive = FALSE)

  out <- do.call(rbind, lapply(combos, summarize_for))
  rownames(out) <- NULL

  attr(out, "call") <- mc
  out
}
