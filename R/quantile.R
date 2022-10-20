#' @export
#' @importFrom sandwich vcovBS
vcovBS.rqs <- function(x, cluster = NULL, R = 250) {
  fit_model <- function(m, data) {
    normalized_coef(quantreg::rq(m$formula, data, tau = m$tau))
  }

  mosaic::do(R) * fit_model(x, mosaic::resample(data, groups = cluster)) %>%
  stats::cov
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
