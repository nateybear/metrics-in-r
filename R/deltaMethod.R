deltaMethod <- function(..., coefs, vcov.) {
  exprs <- rlang::enquos(...)

  params <- names(coefs)

  # create an environment that has bindings to the model parameters
  # assume that e_parent is the same for all e in exprs
  env_model <- local({
    e_parent <- rlang::quo_get_env(exprs[[1]])
    rlang::as_environment(coefs, parent = e_parent)
  })

  # get colnames so that our jacobian can use them
  quo_string <- purrr::compose(rlang::expr_deparse, rlang::quo_get_expr)
  col_names <- purrr::map_chr(exprs, quo_string)

  # the hard part: get a jacobian by doing
  # symbolic differentiation using the
  # stats::D function. Then plug into the derivative.
  jacobian <-
    purrr::map(params, function(p) {
      purrr::map_dbl(exprs, function(e) {
        derivative <- stats::D(rlang::get_expr(e), p)
        rlang::eval_tidy(derivative, env = env_model)
      })
    }) %>%
    purrr::reduce(rbind) %>%
    magrittr::set_rownames(params) %>%
    magrittr::set_colnames(col_names)

  quo_eval <- purrr::compose(~ rlang::eval_tidy(., env = env_model), rlang::quo_get_expr)

  return(list(
    estimate = purrr::map_dbl(exprs, quo_eval) %>% magrittr::set_names(col_names),
    vcov = t(jacobian) %*% vcov. %*% jacobian
  ))
}
