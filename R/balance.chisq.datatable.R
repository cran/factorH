#' Count-balance chi-square diagnostics across factors
#'
#' For one factor: chi-square goodness-of-fit vs equal proportions.
#' For two factors: chi-square test of independence.
#' For three or more: log-linear independence (Poisson, main effects only) via deviance and df.
#'
#' @param formula A model formula \code{y ~ A + B (+ C ...)}; the response is ignored.
#' @param data A data frame with the variables.
#' @param force_factors Logical; if TRUE, coerces RHS predictors to factors.
#' @param correct Logical; continuity correction for 2x2 tables in \code{chisq.test} (default FALSE).
#'
#' @return A data.frame with one row per factor combination (\code{Effect}) and columns:
#' \code{n}, \code{ChiSq} (4 decimals), \code{df}, \code{p.chisq} (4 decimals), \code{OK}.
#'
#' @details Uses \code{stats::chisq.test} for 1–2 factors. For 3+ factors, prefers \code{MASS::loglm}
#' if available; otherwise falls back to a Poisson GLM on the count table.
#'
#' @examples
#' \dontrun{
#' balance.chisq.datatable(liking ~ gender + condition + age_cat, data = mimicry)
#' }
#' @importFrom stats chisq.test xtabs glm deviance df.residual pchisq
#' @seealso \code{\link{plan.diagnostics}}
#' @export
balance.chisq.datatable <- function(formula, data, force_factors = TRUE, correct = FALSE) {
  stopifnot(inherits(formula, "formula"))
  mc <- match.call()

  vars <- all.vars(formula)
  resp <- vars[1]
  facs <- attr(stats::terms(formula), "term.labels")
  if (length(facs) < 1L) stop("Provide at least one factor on the RHS of '~'.")

  # kompletne przypadki względem zmiennych użytych (w tym y, jeśli jest w formule)
  needed <- unique(c(vars))
  cc <- stats::complete.cases(data[, needed, drop = FALSE])
  d  <- droplevels(data[cc, c(facs), drop = FALSE])  # do testów używamy wyłącznie czynników

  if (force_factors) {
    for (nm in facs) if (!is.factor(d[[nm]])) d[[nm]] <- factor(d[[nm]])
  }

  summarize_for <- function(grp_vars) {
    eff <- paste(grp_vars, collapse = ":")
    k <- length(grp_vars)

    if (k == 1L) {
      # jednorodność (równe udziały)
      tbl <- table(d[[grp_vars]])
      n <- sum(tbl)
      exp_p <- rep(1 / length(tbl), length(tbl))
      cs <- suppressWarnings(stats::chisq.test(tbl, p = exp_p, correct = correct))
      X2 <- unname(cs$statistic)
      df <- unname(cs$parameter)
      p  <- cs$p.value

    } else if (k == 2L) {
      # niezależność 2-czynnikowa
      tbl <- table(d[[grp_vars[1]]], d[[grp_vars[2]]])
      n <- sum(tbl)
      cs <- suppressWarnings(stats::chisq.test(tbl, correct = correct))
      X2 <- unname(cs$statistic)
      df <- unname(cs$parameter)
      p  <- cs$p.value

    } else {
      # log-linear: wzajemna niezależność (tylko efekty główne)
      xt <- stats::xtabs(~ ., data = d[grp_vars])
      n  <- sum(xt)

      if (requireNamespace("MASS", quietly = TRUE)) {
        f_ind <- stats::as.formula(paste("~", paste(grp_vars, collapse = " + ")))
        fit   <- MASS::loglm(f_ind, data = xt)
        X2 <- fit$deviance
        df <- fit$df
        p  <- stats::pchisq(X2, df, lower.tail = FALSE)
      } else {
        # fallback: GLM Poissona na danych zliczeń
        dfc <- as.data.frame(xt)
        f_ind <- stats::as.formula(paste("Freq ~", paste(grp_vars, collapse = " + ")))
        fit <- stats::glm(f_ind, family = stats::poisson(), data = dfc)
        X2 <- stats::deviance(fit)
        df <- stats::df.residual(fit)
        p  <- stats::pchisq(X2, df, lower.tail = FALSE)
      }
    }

    chi_fmt <- if (is.na(X2)) NA_character_ else formatC(X2, format = "f", digits = 4)
    p_fmt <- if (is.na(p)) NA_character_ else formatC(p, format = "f", digits = 4)
    ok    <- if (is.na(p)) NA_character_ else if (p < 0.05) "NOT OK" else "OK"

    # kolumny czynników ustawiamy na NA (test dotyczy całej tablicy)
    fac_cols <- lapply(facs, function(f) factor(NA, levels = levels(d[[f]])))
    names(fac_cols) <- facs

    as.data.frame(c(list(Effect = eff), fac_cols,
                list(n = n, ChiSq = chi_fmt, df = df, p.chisq = p_fmt, OK = ok)),
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