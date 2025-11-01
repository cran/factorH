#' Levene/Brown-Forsythe test for full-plan cells
#'
#' Tests homogeneity of variances across the highest-order interaction (all RHS factors combined),
#' using Levene's test (Brown-Forsythe with median by default).
#'
#' @param formula A model formula \code{y ~ A + B (+ C ...)}.
#' @param data A data frame with the variables.
#' @param center Character, \code{"median"} (default) for Brown-Forsythe or \code{"mean"} for classical Levene.
#' @param force_factors Logical; if TRUE, coerces RHS predictors to factors.
#'
#' @return A one-row data.frame with columns:
#' \code{Effect}, \code{n.groups}, \code{min.n}, \code{df.num}, \code{df.den}, \code{F}, \code{p}, \code{OK}.
#' Values \code{F} and \code{p} are formatted to 4 decimals (no scientific notation); \code{OK} is
#' "OK" if \code{p >= 0.05}, otherwise "NOT OK".
#'
#' @details Internally relies on \code{car::leveneTest}. If fewer than two groups or any group
#' has < 2 observations, NA values are returned with a warning.
#'
#' @examples
#' \dontrun{
#' levene.plan.datatable(liking ~ gender + condition + age_cat, data = mimicry)
#' levene.plan.datatable(liking ~ gender + condition, data = mimicry, center = "mean")
#' }
#' @importFrom stats median
#' @seealso \code{\link{plan.diagnostics}}
#' @export
levene.plan.datatable <- function(formula, data, center = c("median","mean"), force_factors = TRUE) {
  stopifnot(inherits(formula, "formula"))
  if (!requireNamespace("car", quietly = TRUE)) {
    stop("Package 'car' is required for leveneTest(). Please install it.")
  }
  mc <- match.call()
  center <- match.arg(center)
  center_fun <- if (center == "median") stats::median else base::mean

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

  # groups = full plan cells
  grp <- interaction(d[facs], drop = TRUE)

  cell_sizes <- table(grp)
  n_groups   <- length(cell_sizes)
  min_n      <- if (n_groups) min(cell_sizes) else 0L

  Fval <- df1 <- df2 <- p <- NA_real_

  if (n_groups >= 2L && min_n >= 2L) {
    lt <- car::leveneTest(d[[resp]] ~ grp, center = center_fun)

    rn <- rownames(lt)
    # the stats row is the one that is not "Residuals"
    stat_row_idx <- which(rn != "Residuals")
    if (length(stat_row_idx) >= 1L) {
      stat_row_idx <- stat_row_idx[1L]
      # find columns: Df, F (or "F value") and p (Pr(>F))
      df_col <- which(tolower(colnames(lt)) %in% c("df", "d.f.", "degrees of freedom"))
      F_col  <- grep("^f(\\s*value)?$", colnames(lt), ignore.case = TRUE)
      p_col  <- grep("^pr\\(>f\\)$|^p(\\.value)?$", colnames(lt), ignore.case = TRUE)

      df1 <- suppressWarnings(as.numeric(lt[stat_row_idx, df_col][1]))
      df2 <- suppressWarnings(as.numeric(lt[which(rn == "Residuals"), df_col][1]))
      Fval <- suppressWarnings(as.numeric(lt[stat_row_idx, F_col][1]))
      p    <- suppressWarnings(as.numeric(lt[stat_row_idx, p_col][1]))
    }
  } else {
    warning("Levene's test needs >=2 groups with >=2 obs each; returning NA.")
  }

  F_fmt <- if (length(Fval) == 0L || is.na(Fval)) NA_character_ else formatC(Fval, format = "f", digits = 4)
  p_fmt <- if (length(p)    == 0L || is.na(p))    NA_character_ else formatC(p,    format = "f", digits = 4)
  ok    <- if (is.na(p)) NA_character_ else if (p < 0.05) "NOT OK" else "OK"

  out <- data.frame(
    Effect   = paste(facs, collapse = ":"),
    n.groups = n_groups,
    min.n    = as.integer(min_n),
    df.num   = if (is.na(df1)) NA_integer_ else as.integer(df1),
    df.den   = if (is.na(df2)) NA_integer_ else as.integer(df2),
    F        = F_fmt,
    p        = p_fmt,
    OK       = ok,
    stringsAsFactors = FALSE
  )
  attr(out, "call") <- mc
  out
}
