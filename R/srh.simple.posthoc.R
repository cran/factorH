#' Simple-effects post hoc (Dunn) with Bonferroni adjustment
#'
#' Computes Dunn's pairwise comparisons for **simple effects** of one target
#' factor (\code{compare}) within levels of the remaining conditioning factors
#' (\code{by}). Adjustment can be done **within** each conditioning table
#' (SPSS-like) or **globally** across all tests.
#'
#' @param formula A formula of the form \code{y ~ A + B (+ C ...)}; requires
#'   at least two RHS factors to define a simple effect.
#' @param data A \code{data.frame} containing variables in \code{formula}.
#' @param compare Character; the factor to compare pairwise. By default, the
#'   first factor on the RHS of \code{formula}.
#' @param scope \code{"within"} (default) applies Bonferroni adjustment **within
#'   each by-table**; \code{"global"} applies one Bonferroni across **all**
#'   pairwise tests produced for all by-tables combined.
#' @param digits Number of digits for rounding numeric columns (\code{Z},
#'   \code{P.unadj}, \code{P.adj}). Default \code{3}.
#'
#' @details
#' The data are subset to complete cases on \code{y} and all RHS factors.
#' All RHS variables are coerced to \code{factor}. The table is split by all
#' factors except \code{compare} and Dunn's test (\code{FSA::dunnTest}) is run
#' per split. With \code{scope = "within"}, the Bonferroni correction is applied
#' separately in each split (with \code{m.tests = choose(k,2)} for that split).
#' With \code{scope = "global"}, \code{P.adj} is re-computed once with
#' \code{stats::p.adjust(..., method = "bonferroni")} across **all** pairwise
#' tests from all splits (and \code{m.tests} is set to the total number of
#' tests).
#'
#' @return A \code{data.frame} with columns:
#' \itemize{
#'   \item conditioning factor columns (one value repeated per split),
#'   \item \code{Comparison}, \code{Z}, \code{P.unadj}, \code{P.adj},
#'   \item \code{m.tests} (number of tests used for Bonferroni),
#'   \item \code{adj.note} (human-readable note).
#' }
#' Attributes: \code{"adjustment"} (one-line description) and \code{"call"}.
#'
#' @examples
#' data(mimicry, package = "factorH")
#'
#' # Two factors: pairwise comparisons for 'gender' within levels of 'condition'.
#' # By default, compare = first RHS factor ('gender' here).
#' # p.adj uses Bonferroni within each by-table (scope = "within").
#' tab1 <- srh.simple.posthoc(liking ~ gender + condition, data = mimicry)
#' head(tab1); attr(tab1, "adjustment")
#'
#' # One global family of tests (global Bonferroni across all subgroup tests):
#' tab2 <- srh.simple.posthoc(liking ~ gender + condition, data = mimicry,
#'                            scope = "global")
#' head(tab2); attr(tab2, "adjustment")
#'
#' # Three factors: compare 'gender' within each condition × age_cat cell.
#' tab3 <- srh.simple.posthoc(liking ~ gender + condition + age_cat, data = mimicry)
#' head(tab3)
#'
#' # Choose a different target factor to compare: here 'condition'
#' # (within each gender × age_cat cell).
#' tabA <- srh.simple.posthoc(liking ~ gender + condition + age_cat, data = mimicry,
#'                            compare = "condition")
#' head(tabA)
#'
#' # Global Bonferroni variants (less common, but sometimes requested):
#' tabG  <- srh.simple.posthoc(liking ~ gender + condition + age_cat, data = mimicry,
#'                             scope = "global")
#' tabG2 <- srh.simple.posthoc(liking ~ condition + gender, data = mimicry)
#' tabG3 <- srh.simple.posthoc(liking ~ condition + gender, data = mimicry,
#'                             scope = "global")
#' head(tabG); head(tabG2); head(tabG3)
#' @export
#' @importFrom stats complete.cases terms as.formula p.adjust
srh.simple.posthoc <- function(formula, data,
                               compare = NULL,
                               scope = c("within","global"),
                               digits = 3) {
  scope <- match.arg(scope)
  stopifnot(inherits(formula, "formula"))
  if (!requireNamespace("FSA", quietly = TRUE))
    stop("Please install package 'FSA' (needed for FSA::dunnTest).")

  # parse formula
  vars <- all.vars(formula)
  resp <- vars[1]
  rhs  <- attr(stats::terms(formula), "term.labels")
  if (!is.numeric(data[[resp]]))
    stop("The response '", resp, "' must be numeric.")
  if (length(rhs) < 2L)
    stop("Provide at least two factors on the RHS, e.g., y ~ A + B.")


  if (is.null(compare)) compare <- rhs[1]
  if (!compare %in% rhs)
    stop("Argument 'compare' must be one of: ", paste(rhs, collapse=", "))

  by_facs <- setdiff(rhs, compare)

  # complete cases and ensure factors
  keep <- stats::complete.cases(data[, c(resp, rhs), drop = FALSE])
  d <- droplevels(data[keep, c(resp, rhs), drop = FALSE])
  for (nm in rhs) if (!is.factor(d[[nm]])) d[[nm]] <- factor(d[[nm]])

  # split by all conditioning factors (or one 'ALL' group)
  grp_index <- if (length(by_facs)) interaction(d[, by_facs, drop = FALSE], drop = TRUE, sep = " | ")
               else factor("ALL")
  spl <- split(d, grp_index, drop = TRUE)

  res_list <- lapply(names(spl), function(key) {
    sub <- spl[[key]]
    g_levels <- nlevels(sub[[compare]])
    if (g_levels < 2L) return(NULL)

    # Dunn per subgroup
    if (scope == "within") {
      dtab <- FSA::dunnTest(stats::as.formula(paste(resp, "~", compare)),
                            data = sub, method = "bonferroni")$res
      m_b  <- choose(g_levels, 2)
      add_note <- paste0("bonferroni for ", m_b, if (m_b == 1) " test" else " tests")
    } else {
      dtab <- FSA::dunnTest(stats::as.formula(paste(resp, "~", compare)),
                            data = sub, method = "none")$res
      m_b  <- NA_integer_  # will be overwritten after global adjust
      add_note <- NA_character_
    }

    # add conditioning columns once per split
    add_cols <- if (length(by_facs)) lapply(by_facs, function(f) as.character(sub[[f]][1])) else list()
    names(add_cols) <- by_facs

    cbind(as.data.frame(add_cols, stringsAsFactors = FALSE),
          dtab,
          m.tests = m_b,
          adj.note = add_note,
          stringsAsFactors = FALSE)
  })

  res_list <- Filter(Negate(is.null), res_list)
  if (!length(res_list))
    stop("No subgroup had ? 2 levels of '", compare, "'.")

  res <- do.call(rbind, res_list)
  rownames(res) <- NULL

  # global correction if requested
  if (scope == "global") {
    if (!"P.unadj" %in% names(res)) stop("No 'P.unadj' column from dunnTest().")
    res$P.adj   <- stats::p.adjust(res$P.unadj, method = "bonferroni")
    m_global    <- nrow(res)
    res$m.tests <- m_global
    res$adj.note <- paste0("bonferroni for ", m_global, if (m_global == 1) " test" else " tests")
  }

  # select & round
  keep_cols <- c(by_facs, "Comparison", "Z", "P.unadj", "P.adj", "m.tests", "adj.note")
  out <- res[, keep_cols]
  out$Z       <- round(out$Z,       digits)
  out$P.unadj <- round(out$P.unadj, digits)
  out$P.adj   <- round(out$P.adj,   digits)

  # header-like attribute
  hdr <- if (scope == "within") {
    "Adjustment: Bonferroni within each by-table (see 'm.tests' / 'adj.note')."
  } else {
    paste0("Adjustment: Bonferroni global across all tests (m = ", nrow(out), ").")
  }
  attr(out, "adjustment") <- hdr
  attr(out, "call") <- match.call()
  out
}
