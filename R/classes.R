#### All post estimation is essentially the same. This file formalizes this
#### notion by creating an S3 superclass called "postEstimation" and creating
#### print methods for it. Individual subclasses (e.g. nlcom) just create their
#### own summaryTable using the correct test statistics.

postEstimation <- function(clazz) {
  function(model, ..., vcov. = NULL, params = NULL, alpha = 0.95) {
    # if different names are supplied via params, use them
    coefs <- normalized_coef(model)
    if (!is.null(params)) {
      names(coefs) <- params
    }

    # homoskedastic vcov. by default, or pass one in as a matrix or function
    V <- if (is.null(vcov.)) {
      stats::vcov(model)
    } else if (is.matrix(vcov.)) {
      vcov.
    } else {
      rlang::as_function(vcov.)(model)
    }

    out <- deltaMethod(..., coefs = coefs, vcov. = V)

    structure(append(out, list(model = model, params = params, alpha = alpha)), class = c(clazz, "postEstimation", "list"))
  }
}

summaryTable <- function(x) {
  UseMethod("summaryTable")
}

#' @export
print.postEstimation <- function(x, ...) {
  table <- summaryTable(x)
  if (is.character(table)) {
    cat(table)
  } else {
    print(table)
  }
  if (!is.null(x$params)) {
    model_names <- names(normalized_coef(x$model))
    conversions <- glue::glue("{x$params}={model_names}") %>% paste0(collapse = ", ")
    cat("\n")
    cli::cli_alert_info(glue::glue("Where {conversions}"))
  }
}

#' @export
#' @importFrom stats df.residual
df.residual.fixest <- function(object, ...) object$nobs - object$nparams
