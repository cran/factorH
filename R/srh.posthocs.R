#' Dunn post hoc tables (p.adj only) for all effects in a factorial design
#'
#' For a given \code{y ~ A (+ B + ...)} formula, runs \code{\link{srh.posthoc}}
#' for every main effect and interaction implied by the RHS (all non-empty
#' combinations of factors) and returns a named list of adjusted p-value
#' matrices (\code{P.adj}) for each effect.
#'
#' @param formula A formula of the form \code{y ~ A (+ B + ...)}.
#' @param data A \code{data.frame} containing variables in \code{formula}.
#' @param method P-value adjustment method passed to \code{FSA::dunnTest()} via
#'   \code{\link{srh.posthoc}}. Default \code{"bonferroni"}.
#' @param digits Rounding used inside \code{\link{srh.posthoc}} when
#'   \code{numeric = FALSE}. Default \code{3}.
#' @param triangular Which triangle to show in each matrix
#'   (\code{"lower"}, \code{"upper"}, \code{"full"}). Default \code{"lower"}.
#' @param numeric Logical; if \code{TRUE}, return numeric data frames with
#'   \code{NA}s on the masked triangle/diagonal; if \code{FALSE} (default),
#'   return character data frames with masked cells as empty strings.
#' @param force_factors Logical; coerce grouping variables to \code{factor}
#'   before analysis (default \code{TRUE}).
#' @param sep Separator for combined factor labels when needed (passed through
#'   to \code{\link{srh.posthoc}}). Default \code{"."}.
#'
#' @details
#' The function enumerates all non-empty subsets of RHS factors (mains, 2-way,
#' ..., k-way) and calls \code{\link{srh.posthoc}} on each corresponding
#' sub-formula. If a subset has fewer than 2 observed levels (e.g., due to
#' missing data after subsetting to complete cases), that effect is skipped.
#'
#' @return A named \code{list} where each element is a \code{data.frame}
#' of adjusted p-values (\code{P.adj}) for an effect. Names use \code{"A"},
#' \code{"B"}, \code{"A:B"}, ..., matching the effect structure.
#' The original call is attached as attribute \code{"call"}.
#'
#' @examples
#' data(mimicry, package = "factorH")
#'
#' # Two-factor design: p.adj for 'gender', 'condition', and 'gender:condition'
#' L2 <- srh.posthocs(liking ~ gender + condition, data = mimicry)
#' names(L2)
#' L2$gender
#' L2$condition
#' L2$`gender:condition`
#'
#' # Three-factor design: includes mains, all 2-ways, and the 3-way effect
#' L3 <- srh.posthocs(liking ~ gender + condition + age_cat, data = mimicry)
#' names(L3)
#'
#' @export
#' @importFrom stats terms as.formula
#' @importFrom utils combn
srh.posthocs <- function(formula, data,
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

  # all non-empty combinations of factors
  combos <- unlist(
    lapply(seq_along(facs), function(k) utils::combn(facs, k, simplify = FALSE)),
    recursive = FALSE
  )

  out <- list()
  for (S in combos) {
    rhs <- paste(S, collapse = " + ")
    fS  <- stats::as.formula(paste(resp, "~", rhs))
    key <- paste(S, collapse = ":")

    tabs <- tryCatch(
      srh.posthoc(fS, data = data,
                  method = method,
                  digits = digits,
                  triangular = triangular,
                  numeric = numeric,
                  force_factors = force_factors,
                  sep = sep),
      error = function(e) NULL
    )
    if (!is.null(tabs)) out[[key]] <- tabs$`P.adj`
  }

  attr(out, "call") <- match.call()
  out
}
