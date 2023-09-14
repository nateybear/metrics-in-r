#' Estimate Linear Functions of Model Parameters
#'
#' Given a model returned by a function such as `lm()` and one or more
#' expressions to evaluate, construct a point estimate and confidence intervals
#' on the expression(s) using the exact delta method. Estimate p-values using
#' the t distribution as an exact finite-sample distribution.
#'
#' @inherit nlcom details
#' @inheritParams nlcom
#' @export
#'
#' @examples
#' library(fixest)
#' model <- feols(mpg ~ wt + hp, data = mtcars, vcov = "HC1")
#'
#' lincom(model, wt + 3, 2 * hp)
#'
#' # equivalently, supply your own parameter names
#' library(glue)
#' lincom(model, b2 + 3, 2 * b3, params = glue("b{1:3}"))
lincom <- postEstimation("lincom")

summaryTable.lincom <- function(x) {
  estimate <- x$estimate
  se <- sqrt(diag(x$vcov))
  t_val <- estimate / se
  p_val <- 2 * stats::pt(-abs(t_val), df = stats::df.residual(x$model))

  t_crit <- abs(stats::qt((1 - x$alpha) / 2, df = stats::df.residual(x$model)))

  tibble::tibble(
    `Expression` = names(estimate),
    `Estimate` = estimate,
    `Std. Error` = se,
    `t-Value` = t_val,
    `Pr(>|t|)` = p_val,
    `CI Lower` = estimate - se * t_crit,
    `CI Upper` = estimate + se * t_crit
  )
}
