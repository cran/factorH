#' K-way SRH on ranks with tie-corrected p-values and rank-based effect sizes
#'
#' Generalizes the Scheirer–Ray–Hare (SRH) approach to \emph{k}-factor designs
#' by using sums of squares from a linear model on ranks, with a standard tie
#' correction \eqn{D} applied to p-values. The function returns H, tie-corrected
#' H (\code{Hadj}), \eqn{p}-values and rank-based effect sizes (\code{eta2H},
#' \code{eps2H}) for each main effect and interaction up to the full order
#' (i.e., \code{(A + B + ...)^k}).
#'
#' @param formula A formula of the form \code{y ~ A + B (+ C ...)}.
#' @param data A \code{data.frame} with the variables in \code{formula}.
#' @param clamp0 Logical; if \code{TRUE} (default), negative \code{eta2H} is
#'   truncated to 0 and \code{eps2H} truncated to the interval \eqn{[0, 1]}.
#' @param force_factors Logical; coerce grouping variables to \code{factor}
#'   (default \code{TRUE}).
#' @param type Integer; the SS type to use in \code{car::Anova}. Defaults to
#'   \code{2} (Type II). Set \code{type = 3} for Type III (internally uses
#'   sum-to-zero contrasts for factors in the model fit; global options are
#'   not modified).
#' @param ... Passed to \code{stats::lm()} if applicable.
#'
#' @details
#' Ranks are computed globally on \code{y} with \code{ties.method = "average"}.
#' Sums of squares are obtained from \code{car::Anova()} on the rank model
#' \code{R ~ (A + B + ...)^k}. Tie correction:
#' \deqn{D = 1 - \frac{\sum (t^3 - t)}{n^3 - n},}
#' where \eqn{t} are tie block sizes and \eqn{n} is the number of complete cases.
#' We report \code{Hadj = H / D} and \eqn{p = P(\chi^2_{df} \ge Hadj)}.
#'
#' Rank-based effect sizes are computed from the \emph{uncorrected} \code{H}
#' (classical SRH convention): \code{eta2H = (H - k + 1) / (n - k)} and
#' \code{eps2H = H * (n + 1) / (n^2 - 1)}, where \code{k} is the number of
#' non-empty groups compared by the term.
#'
#' For \code{type = 3}, the model is fitted with sum-to-zero contrasts
#' (\code{stats::contr.sum}) for RHS factors having at least 2 levels, so that
#' Type III tests have the standard interpretation. Global contrast options are
#' not altered.
#'
#' @return A \code{data.frame} with class \code{c("srh_kway","anova","data.frame")}
#'   containing columns: \code{Effect}, \code{Df}, \code{Sum Sq}, \code{H},
#'   \code{Hadj}, \code{p.chisq}, \code{k}, \code{n}, \code{eta2H}, \code{eps2H}.
#'   The original call is attached as an attribute and can be retrieved with
#'   \code{getCall()}.
#'
#' @examples
#' \dontrun{
#' data(mimicry, package = "factorH")
#' # One factor (KW-style check)
#' srh.kway(liking ~ condition, data = mimicry)
#'
#' # Two factors (Type II by default)
#' srh.kway(liking ~ gender + condition, data = mimicry)
#'
#' # Three factors
#' srh.kway(liking ~ gender + condition + age_cat, data = mimicry)
#'
#' # Type III SS (with sum-to-zero contrasts set locally)
#' srh.kway(liking ~ gender + condition, data = mimicry, type = 3)
#' }
#'
#' @importFrom stats complete.cases lm pchisq terms update as.formula contr.sum
#' @seealso \code{\link[car]{Anova}}
#' @export
srh.kway <- function(formula, data, clamp0 = TRUE, force_factors = TRUE, type = 2, ...) {
  stopifnot(inherits(formula, "formula"))
  cl <- match.call()

  # variables
  vars <- all.vars(formula)
  resp <- vars[1]
  facs <- attr(stats::terms(formula), "term.labels")
  if (length(facs) < 1L) stop("Provide at least one factor on the RHS of '~'.")
  if (!is.numeric(data[[resp]]))
    stop("The response '", resp, "' must be numeric.")

  # same subset as in the test: complete cases on y and factors
  needed <- c(resp, facs)
  cc <- stats::complete.cases(data[, needed, drop = FALSE])
  d  <- droplevels(data[cc, needed, drop = FALSE])
  n  <- nrow(d)
  if (n < 2L) stop("Not enough complete cases after subsetting (n < 2).")

  if (force_factors) {
    for (nm in facs) if (!is.factor(d[[nm]])) d[[nm]] <- factor(d[[nm]])
  }

  # full factorial: all interactions up to k-way
  korder <- length(facs)
  if (korder == 1L) {
    rhs_full <- stats::as.formula(paste("~", facs))
  } else {
    rhs_full <- stats::as.formula(paste0("~ (", paste(facs, collapse = " + "), ")^", korder))
  }

  # ranks + linear model on ranks
  d$R <- base::rank(d[[resp]], ties.method = "average")

  # For Type III, set sum-to-zero contrasts locally (only for factors with >= 2 levels)
  contr_list <- NULL
  if (identical(type, 3L) || identical(type, 3)) {
    use <- facs[vapply(facs, function(nm) nlevels(d[[nm]]) > 1L, logical(1))]
    if (length(use)) {
      contr_list <- lapply(use, function(nm) stats::contr.sum(nlevels(d[[nm]])))
      names(contr_list) <- use
    }
  }

  fit <- stats::lm(stats::update(rhs_full, stats::as.formula(paste("R ~ ."))),
                   data = d, contrasts = contr_list, ...)

  # Type II/III SS (per 'type')
  tab <- as.data.frame(car::Anova(fit, type = type))
  tab$Effect <- rownames(tab)

  # tie correction (D) used for Hadj and p-values
  ties_tab <- table(d$R)
  D  <- 1 - sum(ties_tab^3 - ties_tab) / (n^3 - n)
  MS_tot <- n * (n + 1) / 12

  # map SS column name
  ss_name <- intersect(c("Sum Sq","Sum.Sq","SS","Sumsq","Sum Sq."), names(tab))
  if (length(ss_name) == 0L) stop("Could not find SS column in car::Anova() table.")
  ss_name <- ss_name[1L]

  tab$H       <- tab[[ss_name]] / MS_tot
  tab$Hadj    <- tab$H / D
  tab$p.chisq <- stats::pchisq(tab$Hadj, df = tab$Df, lower.tail = FALSE)

  # k = number of groups compared by the term (non-empty cells)
  d_f <- droplevels(d[, facs, drop = FALSE])
  kval <- function(term) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    if (!all(parts %in% names(d_f))) return(NA_integer_)
    if (length(parts) == 1L) {
      nlevels(d_f[[parts]])
    } else {
      nlevels(interaction(d_f[, parts, drop = FALSE], drop = TRUE))
    }
  }
  tab$k <- vapply(tab$Effect, kval, integer(1))

  tab$n <- n

  # effect sizes from unadjusted H (classical SRH convention)
  tab$eta2H <- (tab$H - tab$k + 1) / (n - tab$k)
  if (clamp0) tab$eta2H <- pmax(0, tab$eta2H)

  tab$eps2H <- (tab$H * (n + 1)) / (n^2 - 1)
  if (clamp0) tab$eps2H <- pmin(1, pmax(0, tab$eps2H))
  tab$eps2H[tab$Effect == "Residuals"] <- NA_real_

  # reorder columns + standardize SS name
  tab <- tab[, c("Effect","Df", ss_name, "H","Hadj","p.chisq","k","n","eta2H","eps2H")]
  names(tab)[names(tab) == ss_name] <- "Sum Sq"

  attr(tab, "call") <- cl
  class(tab) <- c("srh_kway", "anova", "data.frame")
  tab
}


#' @export
#' @method getCall srh_kway
getCall.srh_kway <- function(x, ...) attr(x, "call")
