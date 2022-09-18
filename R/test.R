#' Jointly Test Linear Functions of Model Parameters
#'
#' Given a model returned by a function such as `lm()` and one or more
#' expressions to evaluate, jointly test the null hypothesis that the expression
#' are all zero, using the exact delta method to construct an F statistic. This
#' is a finite-sample Wald test.
#'
#' @inherit nlcom details
#' @inheritParams nlcom
#' @export
#'
#' @examples
#' library(fixest)
#' model <- feols(mpg ~ wt + hp, data = mtcars, vcov = "HC1")
#'
#' test(model, wt + 3, 2 * hp)
#'
#' # equivalently, supply your own parameter names
#' library(glue)
#' test(model, wt + 3, 2 * hp, params = glue("b{1:3}"))
test <- postEstimation("test")

summaryTable.test <- function(x) {
  u <- matrix(x$estimate, ncol = 1)
  df1 <- nrow(u)
  df2 <- stats::df.residual(model)
  f_stat <- (t(u) %*% solve(x$vcov, u)) / df1
  p_val <- stats::pf(f_stat, df1, df2, lower.tail = F)

  glue::glue("
Joint F Test of the linear hypotheses
{paste0(glue::glue('• {names(x$estimate)} = 0'), collapse='\n')}
{cli::ansi_align(glue::glue('F({df1},{df2}) = {round(f_stat, 6)}'), align='center', width = 50)}
{cli::ansi_align(glue::glue('Pr(>F) = {round(p_val,6)}'), align='center', width = 50)}

")
}
