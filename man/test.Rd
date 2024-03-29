% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test.R
\name{test}
\alias{test}
\title{Jointly Test Linear Functions of Model Parameters}
\usage{
test(model, ..., vcov. = NULL, params = NULL)
}
\arguments{
\item{model}{A linear model, fit by a call to \code{lm}, \code{feols}, or \code{lm_robust}}

\item{...}{One or more expressions involving parameters in \code{model}}

\item{vcov.}{Optional specify the covariance of the model. Can be a matrix,
function or lambda. Defaults to \code{stats::vcov(model)}}

\item{params}{Optional character vector. rename the model parameters to make
them easier to refer to in your expressions in \code{...}}
}
\description{
Given a model returned by a function such as \code{lm()} and one or more
expressions to evaluate, jointly test the null hypothesis that the expression
are all zero, using the exact delta method to construct an F statistic. This
is a finite-sample Wald test.
}
\details{
There are two ways to refer to model parameters in your expressions.
\itemize{
\item The names of your model coefficients, such as \code{educ} and \code{abil}. Note that
"strange" expressions that do not evaluate to a symbol in R code must be
surrounded by backticks, e.g. \code{`(Intercept)`} or \code{`educ:abil`}.
\item Using the names supplied as the \code{params} argument to this function. The
order of the coefficients is the order returned by \code{coef(model)}.
}

By default, the covariances of the model parameters are taken from the
\code{\link[stats:vcov]{stats::vcov()}} function. This is a generic function that will produce
different results for different model types. That means that \code{lm()} models
will have a homoskedastic covariance matrix, whereas \code{feols} and \code{lm_robust}
models will have the type of robust covariance matrix that you specify when
creating the model. You can override this default behavior by using the
\code{vcov.} argument.
}
\examples{
library(fixest)
model <- feols(mpg ~ wt + hp, data = mtcars, vcov = "HC1")

test(model, wt + 3, 2 * hp)

# equivalently, supply your own parameter names
library(glue)
test(model, wt + 3, 2 * hp, params = glue("b{1:3}"))
}
