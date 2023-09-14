#' Estimate Non-Linear Functions of Model Parameters
#'
#' Given a model returned by a function such as `lm()` and one or more
#' expressions to evaluate, construct a point estimate and confidence intervals
#' on the expression(s) using the delta method. Estimate p-values using the normal
#' distribution as an asymptotic approximation.
#'
#' There are two ways to refer to model parameters in your expressions.
#'
#' - The names of your model coefficients, such as `educ` and `abil`. Note that
#' "strange" expressions that do not evaluate to a symbol in R code must be
#' surrounded by backticks, e.g. `` `(Intercept)` `` or `` `educ:abil` ``.
#' - Using the names supplied as the `params` argument to this function. The
#' order of the coefficients is the order returned by `coef(model)`.
#'
#' By default, the covariances of the model parameters are taken from the
#' [stats::vcov()] function. This is a generic function that will produce
#' different results for different model types. That means that `lm()` models
#' will have a homoskedastic covariance matrix, whereas `feols` and `lm_robust`
#' models will have the type of robust covariance matrix that you specify when
#' creating the model. You can override this default behavior by using the
#' `vcov.` argument.
#'
#' @param model A linear model, fit by a call to `lm`, `feols`, or `lm_robust`
#' @param ... One or more expressions involving parameters in `model`
#' @param vcov. Optional specify the covariance of the model. Can be a matrix,
#'   function or lambda. Defaults to `stats::vcov(model)`
#' @param params Optional character vector. rename the model parameters to make
#'   them easier to refer to in your expressions in `...`
#'
#' @return A list containing the results of running the delta method, along with
#'   a specialized `print` method to print a summary table with p-values
#'
#' @export
#'
#' @examples
#' library(fixest)
#' model <- feols(mpg ~ wt + hp, data = mtcars, vcov = "HC1")
#'
#' nlcom(model, wt^2, sqrt(hp / wt))
#'
#' # equivalently, supply your own parameter names
#' library(glue)
#' nlcom(model, b2^2, sqrt(b3 / b2), params = glue("b{1:3}"))
#'
nlcom <- postEstimation("nlcom")

summaryTable.nlcom <- function(x) {
  estimate <- x$estimate
  se <- sqrt(diag(x$vcov))
  z_val <- estimate / se
  p_val <- 2 * stats::pnorm(-abs(z_val))

  t_crit <- abs(stats::qt((1 - x$alpha) / 2, df = stats::df.residual(x$model)))


  tibble::tibble(
    `Expression` = names(estimate),
    `Estimate` = estimate,
    `Std. Error` = se,
    `Z-Value` = z_val,
    `Pr(>|z|)` = p_val,
    `CI Lower` = estimate - se * t_crit,
    `CI Upper` = estimate + se * t_crit
  )
}
