#' Dunn post hoc in a symmetric matrix form (one specified effect)
#'
#' Computes Dunn's rank-based pairwise comparisons for the effect implied by
#' \code{formula} and returns symmetric matrices for Z, unadjusted p-values,
#' and adjusted p-values. Cells on one triangle (or both) can be blanked for
#' compact reporting. For multi-factor RHS, factors are combined into a single
#' grouping via \code{interaction()} (e.g., \code{"A:B"} cells).
#'
#' @param formula A formula of the form \code{y ~ factor} or \code{y ~ A + B}
#'   (the latter is treated as \emph{one} combined grouping via interaction).
#' @param data A \code{data.frame} containing variables in \code{formula}.
#' @param method P-value adjustment method passed to \code{FSA::dunnTest()}.
#'   Default \code{"bonferroni"}. See \code{p.adjust.methods} for options.
#' @param digits Number of digits for rounding in the returned matrices
#'   when \code{numeric = FALSE}. Default \code{3}.
#' @param triangular Which triangle to show (\code{"lower"}, \code{"upper"},
#'   or \code{"full"}). Default \code{"lower"}.
#' @param numeric Logical; if \code{TRUE}, return numeric matrices/data frames
#'   with \code{NA} on the masked triangle/diagonal. If \code{FALSE} (default),
#'   return character data frames with masked cells as empty strings.
#' @param force_factors Logical; coerce grouping variables to \code{factor}
#'   (default \code{TRUE}).
#' @param sep Separator used in \code{interaction()} when combining factors.
#'   Default \code{"."}.
#'
#' @details
#' The function subsets to complete cases on \code{y} and RHS factors, optionally
#' coerces factors, builds a single grouping variable (\code{._grp}) and calls
#' \code{FSA::dunnTest(y ~ ._grp, data = ..., method = ...)}. The pairwise
#' results are placed into symmetric matrices \code{Z}, \code{P.unadj}, and
#' \code{P.adj}. By default only the lower triangle (excluding diagonal) is
#' shown for compactness.
#'
#' @return A list with three \code{data.frame}s:
#' \itemize{
#'   \item \code{Z} – Z statistics,
#'   \item \code{P.unadj} – unadjusted p-values,
#'   \item \code{P.adj} – adjusted p-values (per \code{method}).
#' }
#' The original call is attached as attribute \code{"call"}.
#'
#' @examples
#' data(mimicry, package = "factorH")
#'
#' # One factor
#' ph1 <- srh.posthoc(liking ~ condition, data = mimicry)
#' ph1$`P.adj`    # gotowa macierz p po korekcji
#'
#' # Two factors combined (all A:B cells vs all A:B cells)
#' ph2 <- srh.posthoc(liking ~ gender + condition, data = mimicry)
#' ph2$`P.adj`
#'
#' # Upper triangle, numeric frames
#' ph3 <- srh.posthoc(liking ~ condition, data = mimicry,
#'                    triangular = "upper", numeric = TRUE)
#' ph3$Z
#'
#' @export
#' @importFrom stats complete.cases terms as.formula
srh.posthoc <- function(formula, data,
                        method = "bonferroni",
                        digits = 3,
                        triangular = c("lower","upper","full"),
                        numeric = FALSE,
                        force_factors = TRUE,
                        sep = ".") {
  stopifnot(inherits(formula, "formula"))
  triangular <- match.arg(triangular)

  resp <- all.vars(formula)[1]
  facs <- attr(stats::terms(formula), "term.labels")
  if (length(facs) < 1L)
    stop("Provide at least one factor on the RHS (e.g., y ~ condition).")
  if (!is.numeric(data[[resp]]))
    stop("The response '", resp, "' must be numeric.")

  needed <- c(resp, facs)
  cc <- stats::complete.cases(data[, needed, drop = FALSE])
  d  <- droplevels(data[cc, needed, drop = FALSE])

  if (force_factors) {
    for (nm in facs) if (!is.factor(d[[nm]])) d[[nm]] <- factor(d[[nm]])
  }

  # combine factors into one grouping (cells)
  if (length(facs) == 1L) {
    d$._grp <- d[[facs]]
  } else {
    d$._grp <- interaction(d[, facs, drop = FALSE], drop = TRUE, sep = sep)
  }
  levs <- levels(d$._grp)
  if (length(levs) < 2L) stop("Need >= 2 levels for post hoc tests.")

  # Dunn post hoc (Dunn-Bonferroni etc.)
  DT <- FSA::dunnTest(stats::as.formula(paste(resp, "~ ._grp")),
                      data = d, method = method)
  res <- DT$res
  need <- c("Comparison","Z","P.unadj","P.adj")
  miss <- setdiff(need, names(res))
  if (length(miss)) stop("Missing columns in dunnTest(): ", paste(miss, collapse=", "))

  # empty symmetric matrices
  k <- length(levs)
  z_mat  <- matrix(NA_real_, k, k, dimnames = list(levs, levs))
  p1_mat <- matrix(NA_real_, k, k, dimnames = list(levs, levs))
  p2_mat <- matrix(NA_real_, k, k, dimnames = list(levs, levs))

  # fill from pair table
  pairs <- strsplit(as.character(res$Comparison), "\\s*-\\s*")
  for (i in seq_len(nrow(res))) {
    a <- pairs[[i]][1]; b <- pairs[[i]][2]
    z <- res$Z[i]; p1 <- res$P.unadj[i]; p2 <- res$P.adj[i]
    z_mat [a, b] <-  z;  z_mat [b, a] <- -z
    p1_mat[a, b] <-  p1; p1_mat[b, a] <-  p1
    p2_mat[a, b] <-  p2; p2_mat[b, a] <-  p2
  }

  # rounding
  z_mat  <- round(z_mat,  digits)
  p1_mat <- round(p1_mat, digits)
  p2_mat <- round(p2_mat, digits)

  # mask the chosen triangle (+ diagonal)
  if (triangular == "lower") {
    mask <- upper.tri(z_mat, diag = TRUE)
  } else if (triangular == "upper") {
    mask <- lower.tri(z_mat, diag = TRUE)
  } else {
    mask <- matrix(FALSE, k, k)
  }
  z_mat [mask] <- NA_real_
  p1_mat[mask] <- NA_real_
  p2_mat[mask] <- NA_real_

  if (numeric) {
    out <- list(
      Z       = as.data.frame(z_mat,  stringsAsFactors = FALSE, check.names = FALSE),
      P.unadj = as.data.frame(p1_mat, stringsAsFactors = FALSE, check.names = FALSE),
      P.adj   = as.data.frame(p2_mat, stringsAsFactors = FALSE, check.names = FALSE)
    )
  } else {
    # pretty printing: NA -> ""
    fmt <- function(M) {
      X <- apply(M, 2, function(col)
        ifelse(is.na(col), "", formatC(col, format = "f", digits = digits)))
      as.data.frame(X, stringsAsFactors = FALSE, check.names = FALSE)
    }
    out <- list(
      Z       = fmt(z_mat),
      P.unadj = fmt(p1_mat),
      P.adj   = fmt(p2_mat)
    )
  }

  attr(out, "call") <- match.call()
  out
}
