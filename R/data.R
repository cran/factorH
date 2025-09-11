#' Mimicry dataset
#'
#' A dataset used to demonstrate rank-based (nonparametric) multifactor ANOVA.
#'
#' @format A data frame with 533 rows and 7 variables:
#' \describe{
#'   \item{condition}{factor; 5 levels}
#'   \item{gender}{factor; 2 levels}
#'   \item{age}{numeric}
#'   \item{age_cat}{factor; 2 levels}
#'   \item{age_cat2}{factor; 3 levels}
#'   \item{field}{factor; 2 levels}
#'   \item{liking}{numeric; dependent variable}
#' }
#'
#' @details Factor encodings follow the original SPSS labels converted to R factors.
#'
#' @references
#' Trzmielewska, W., Duras, J., Juchacz, A., & Rak, T. (2025).
#' Examining the impact of control condition design in mimicry–liking link research:
#' how motor behavior may impact liking. \emph{Annals of Psychology}, 4, 351–378.
#' \doi{10.18290/rpsych2024.0019}
#'
#' @source Converted from an SPSS file as part of the factorH package examples.
#'
#' @keywords datasets
#' @usage data(mimicry)
"mimicry"
