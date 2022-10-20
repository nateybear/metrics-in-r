#' @export
#' @importFrom sandwich vcovBS
#' @importFrom mosaic do resample
vcovBS.rqs <- function(x, cluster = NULL, R = 250) {
  fit_model <- function(m, data) {
    normalized_coef(quantreg::rq(m$formula, data, tau = m$tau))
  }

  stats::cov(do(R) * fit_model(x, resample(data, groups = cluster)))
}

#' @export
normalized_coef <- function(model) {
  t <- as.numeric(stats::coef(model))

  stats::coef(model) %>%
    {expand.grid(rownames(.), colnames(.))} %>%
    glue::glue_data("{Var1}[{Var2}]") %>%
    rlang::set_names(t, .)
}

#' @export
#' @importFrom stats vcov
vcov.rqs <- function(object, ...) sandwich::vcovBS(object, ...)

#' @export
#' @importFrom stats df.residual
df.residual.rqs <- function(object, ...) {
  nrow(stats::resid(object)) - nrow(stats::coef(object))
}
