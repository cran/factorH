#' Simple-effects post hoc tables for all possible effects (within-scope)
#'
#' For a formula \code{y ~ A + B (+ C ...)}, enumerates **all simple-effect
#' setups** of the form \code{COMPARE(target) | BY(other factors)} and runs
#' \code{\link{srh.simple.posthoc}} with \code{scope = "within"} for each.
#' Returns a named list of data frames (one per simple-effect configuration).
#'
#' @param formula A formula \code{y ~ A + B (+ C ...)} with at least two RHS factors.
#' @param data A \code{data.frame} containing the variables in \code{formula}.
#'
#' @details
#' For each choice of the comparison factor \code{target} from the RHS, all
#' non-empty combinations of the remaining factors are treated as conditioning
#' sets \code{BY}. For each pair \code{(target, BY)} we call
#' \code{srh.simple.posthoc()} with \code{compare = target} and
#' \code{scope = "within"}. Effects where the conditioning subset has < 2 levels
#' of \code{target} are skipped; messages are collected in attribute \code{"skipped"}.
#'
#' Labels use ASCII: \code{"COMPARE(A) | BY(B x C)"} (plain \code{" x "}).
#'
#' @return A named \code{list} of \code{data.frame}s. Each element contains the
#' columns produced by \code{\link{srh.simple.posthoc}} (e.g., \code{Comparison},
#' \code{Z}, \code{P.unadj}, \code{P.adj}, \code{m.tests}, \code{adj.note}).
#' Attributes: \code{"call"} and (optionally) \code{"skipped"} with messages.
#'
#' @examples
#' data(mimicry, package = "factorH")
#'
#' # All simple-effect tables for a 2-factor design
#' tabs2 <- srh.simple.posthocs(liking ~ gender + condition, data = mimicry)
#' names(tabs2)
#' # e.g., tabs2[["COMPARE(gender) | BY(condition)"]]
#'
#' # Three factors: all COMPARE(target) | BY(conditioning) combinations
#' tabs3 <- srh.simple.posthocs(liking ~ gender + condition + age_cat, data = mimicry)
#' names(tabs3)
#' attr(tabs3, "skipped")  # any skipped combos with reasons
#'
#' @export
#' @importFrom stats terms as.formula
#' @importFrom utils combn
srh.simple.posthocs <- function(formula, data) {
  stopifnot(inherits(formula, "formula"))
  rhs <- attr(stats::terms(formula), "term.labels")
  if (length(rhs) < 2L) stop("At least two factors required on the RHS of '~'.")

  resp <- all.vars(formula)[1]
  out <- list()
  skipped <- character(0)

  for (cmp in rhs) {
    rest <- setdiff(rhs, cmp)
    # all non-empty BY combinations from 'rest'
    BY_sets <- unlist(lapply(seq_along(rest), function(k)
      utils::combn(rest, k, simplify = FALSE)), recursive = FALSE)

    for (BY in BY_sets) {
      fS <- stats::as.formula(paste(resp, "~", paste(c(cmp, BY), collapse = " + ")))
      # ASCII-only label (use " x " instead of a Unicode multiplication sign)
      label <- paste0("COMPARE(", cmp, ") | BY(", paste(BY, collapse = " x "), ")")

      res <- tryCatch(
        srh.simple.posthoc(fS, data = data, compare = cmp, scope = "within"),
        error = function(e) { skipped <<- c(skipped, paste0(label, ": ", e$message)); NULL }
      )
      if (!is.null(res)) out[[label]] <- res
    }
  }

  attr(out, "call") <- match.call()
  if (length(skipped)) attr(out, "skipped") <- skipped
  class(out) <- c("srh_simple_posthocs", class(out))
  out
}
