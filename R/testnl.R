#' Jointly Test Non-Linear Functions of Model Parameters
#'
#' Given a model returned by a function such as `lm()` and one or more
#' expressions to evaluate, jointly test the null hypothesis that the expression
#' are all zero, using the approximate delta method to construct an chi-square
#' statistic. This is an asymptotic Wald test.
#'
#' @inherit nlcom details
#' @inheritParams nlcom
#' @export
#'
#' @examples
#' library(fixest)
#' model <- feols(mpg ~ wt + hp, data = mtcars, vcov = "HC1")
#'
#' testnl(model, wt^2, sqrt(hp / wt))
#'
#' # equivalently, supply your own parameter names
#' library(glue)
#' testnl(model, b2^2, sqrt(b3 / b2), params = glue("b{1:3}"))
testnl <- postEstimation("testnl")

summaryTable.testnl <- function(x) {
  u <- matrix(x$estimate, ncol = 1)
  chisq_stat <- t(u) %*% solve(x$vcov, u)
  df <- nrow(u)
  p_val <- stats::pchisq(chisq_stat, df, lower.tail = F)

  glue::glue("
Joint Chi-Square Test of the hypotheses
{paste0(glue::glue('• {names(x$estimate)} = 0'), collapse='\n')}
{cli::ansi_align(glue::glue('chisq({df}) = {round(chisq_stat, 6)}'), align='center', width = 50)}
{cli::ansi_align(glue::glue('Pr(>chisq) = {round(p_val,6)}'), align='center', width = 50)}

")
}
