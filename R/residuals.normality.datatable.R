#' Global residual normality (Shapiro–Wilk) from ANOVA models
#'
#' For each subset of RHS factors, fits a full-factorial ANOVA and runs a single
#' Shapiro–Wilk test on the model residuals (global test per model). Use
#' \code{residuals.cellwise.normality.datatable} for the stricter per-cell assumption.
#'
#' @param formula A model formula \code{y ~ A + B (+ C ...)}.
#' @param data A data frame with the variables.
#' @param force_factors Logical; if TRUE, coerces RHS predictors to factors.
#'
#' @return A data.frame with one row per \code{Effect} (A, B, A:B, ...), with
#' \code{count}, \code{W}, \code{p.shapiro} (4 decimals), \code{OK}.
#'
#' @examples
#' \dontrun{
#' residuals.normality.datatable(liking ~ gender + condition + age_cat, data = mimicry)
#' }
#' @importFrom stats lm update resid shapiro.test as.formula
#' @seealso \code{\link{residuals.cellwise.normality.datatable}}
#' @rawNamespace export(residuals.normality.datatable)
residuals.normality.datatable <- function(formula, data, force_factors = TRUE) {
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

  # zbuduj formułę pełnego układu dla podzbioru zmiennych
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

    # dopasowanie klasycznego modelu ANOVA dla 'grp_vars'
    rhs <- full_rhs_for(grp_vars)
    fit <- stats::lm(stats::update(rhs, stats::as.formula(paste(resp, "~ ."))), data = d)

    r <- stats::resid(fit)
    n <- length(r)

    if (n >= 3L && n <= 5000L) {
      st <- try(stats::shapiro.test(r), silent = TRUE)
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

    # w tej funkcji test dotyczy reszt „globalnie” dla danego modelu, więc kolumny czynników ustawiamy na NA
    fac_cols <- lapply(facs, function(f) factor(NA, levels = levels(d[[f]])))
    names(fac_cols) <- facs

    as.data.frame(c(list(Effect = eff), fac_cols,
                    list(count = n, W = W, p.shapiro = p_fmt, OK = ok_col)),
                  optional = TRUE, stringsAsFactors = FALSE)
  }

  # wszystkie niepuste kombinacje czynników
  combos <- unlist(lapply(seq_along(facs), function(k)
    utils::combn(facs, k, simplify = FALSE)), recursive = FALSE)

  out <- do.call(rbind, lapply(combos, summarize_for))
  rownames(out) <- NULL

  attr(out, "call") <- mc
  out
}
