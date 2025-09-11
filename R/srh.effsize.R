#' SRH with effect sizes for two-factor designs
#'
#' Extends \code{rcompanion::scheirerRayHare()} by adding popular
#' rank-based effect sizes for each SRH term: eta^2_H and epsilon^2_H,
#' and stores the original function call.
#'
#' @param formula A formula of the form \code{y ~ A + B}.
#' @param data A \code{data.frame} containing all variables in \code{formula}.
#' @param clamp0 Logical; if \code{TRUE} (default), negative eta^2_H is
#'   truncated to 0 and epsilon^2_H truncated to the interval \eqn{[0, 1]}.
#' @param ... Passed to \code{rcompanion::scheirerRayHare()}.
#'
#' @details
#' Let \eqn{H} be the SRH H-statistic for a given term, \eqn{n} the sample size
#' used by SRH (complete cases on \code{y} and factors), and \eqn{k} the number
#' of groups compared by that term (for interactions, the number of
#' observed combinations).
#'
#' Effect sizes computed:
#' \itemize{
#' \item \strong{Eta^2_H}: \eqn{(H - k + 1) / (n - k)}.
#' \item \strong{Epsilon^2_H} (KW-like): \eqn{H * (n + 1) / (n^2 - 1)}.
#' }
#'
#' The original call is stored as an attribute and can be retrieved with
#' \code{getCall()}.
#'
#' @return A \code{data.frame} (classed as \code{c("srh_with_call","anova","data.frame")})
#' with the SRH table extended by columns:
#' \code{k}, \code{n}, \code{eta2H}, \code{eps2H}.
#'
#' @examples
#' data(mimicry, package = "factorH")
#' res <- srh.effsize(liking ~ gender + condition, data = mimicry)
#' res
#' getCall(res)
#'
#' @export
#' @importFrom stats complete.cases getCall setNames
srh.effsize <- function(formula, data, clamp0 = TRUE, ...) {
  cl  <- match.call()
  out <- rcompanion::scheirerRayHare(formula, data = data, ...)
  tab <- as.data.frame(out)

  # variables from formula
  allvars <- all.vars(formula)
  resp    <- allvars[1]
  facs    <- allvars[-1]

  # same sample as SRH uses (complete cases on y and factors)
  cc   <- stats::complete.cases(data[, allvars, drop = FALSE])
  dsub <- droplevels(data[cc, facs, drop = FALSE])
  n    <- sum(cc)

  # compute k (number of groups compared) for each SRH term
  rn    <- rownames(tab)
  kvals <- setNames(rep(NA_integer_, length(rn)), rn)
  for (term in rn) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    if (!all(parts %in% colnames(dsub))) next
    if (length(parts) == 1L) {
      kvals[term] <- nlevels(as.factor(dsub[[parts]]))
    } else {
      kvals[term] <- nlevels(interaction(dsub[, parts, drop = FALSE], drop = TRUE))
    }
  }

  tab$k <- kvals
  tab$n <- n

  if (!"H" %in% names(tab))
    stop("SRH table lacks column 'H'. Available: ", paste(names(tab), collapse = ", "))

  tab$eta2H <- (tab$H - tab$k + 1) / (n - tab$k)
  if (clamp0) tab$eta2H <- pmax(0, tab$eta2H)

  tab$eps2H <- (tab$H * (n + 1)) / (n^2 - 1)
  if (clamp0) tab$eps2H <- pmin(1, pmax(0, tab$eps2H))

  # carry over heading, attach call, and set class
  attr(tab, "heading") <- attr(out, "heading")
  attr(tab, "call")    <- cl
  class(tab) <- c("srh_with_call", class(out))  # typically c("srh_with_call","anova","data.frame")
  tab
}

#' @export
#' @method getCall srh_with_call
getCall.srh_with_call <- function(x, ...) attr(x, "call")
