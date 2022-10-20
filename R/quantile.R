#' @export
#' @importFrom sandwich vcovBS
#' @importFrom mosaic do resample
vcovBS.rqs <- function(x, cluster = NULL, R = 250) {
  coefnames <- names(normalized_coef(model))
  fit_model <- function(m, data) {
    normalized_coef(quantreg::rq(m$formula, data, tau = m$tau))
  }

  do_resample <- function() {
    rlang::eval_tidy(rlang::expr(resample(data, groups = !!cluster)))
  }

  stats::cov(do(R) * fit_model(x, do_resample())) %>%
    magrittr::set_colnames(coefnames) %>%
    magrittr::set_rownames(coefnames)
}

#' @export
normalized_coef <- function(model) {
  if(is.matrix(stats::coef(model))){ 
    t <- as.numeric(stats::coef(model))

    pull_tau <- . %>% stringr::str_match("tau= (.*)") %>% magrittr::extract(,2)

    stats::coef(model) %>%
      {expand.grid(rownames(.), colnames(.))} %>%
      glue::glue_data("{Var1}[{pull_tau(Var2)}]") %>%
      rlang::set_names(t, .)
  } else {
    stats::coef(model)
  }
}

#' @export
#' @importFrom stats vcov
vcov.rqs <- function(object, ...) sandwich::vcovBS(object, ...)

#' @export
#' @importFrom stats df.residual
df.residual.rqs <- function(object, ...) {
  nrow(stats::resid(object)) - nrow(stats::coef(object))
}
