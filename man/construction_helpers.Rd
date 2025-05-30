% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construction_helpers.R
\name{construction_helpers}
\alias{construction_helpers}
\alias{construct_model}
\alias{construct_model.data.frame}
\alias{construct_model.survey.design}
\alias{reformulate2}
\alias{bt}
\alias{bt_strip}
\title{Construction Helpers}
\usage{
construct_model(data, ...)

\method{construct_model}{data.frame}(
  data,
  formula,
  method,
  method.args = list(),
  package = "base",
  env = caller_env(),
  ...
)

\method{construct_model}{survey.design}(
  data,
  formula,
  method,
  method.args = list(),
  package = "survey",
  env = caller_env(),
  ...
)

reformulate2(
  termlabels,
  response = NULL,
  intercept = TRUE,
  env = parent.frame(),
  pattern_term = NULL,
  pattern_response = NULL
)

bt(x, pattern = NULL)

bt_strip(x)
}
\arguments{
\item{data}{\itemize{
\item \code{construct_model.data.frame()} (\code{data.frame}) a data frame
\item \code{construct_model.survey.design()} (\code{survey.design}) a survey design object
}}

\item{...}{These dots are for future extensions and must be empty.}

\item{formula}{(\code{formula})\cr
a formula}

\item{method}{(\code{string})\cr
string of function naming the function to be called, e.g. \code{"glm"}.
If function belongs to a library that is not attached, the package name
must be specified in the \code{package} argument.}

\item{method.args}{(named \code{list})\cr
named list of arguments that will be passed to \code{method}.

Note that this list may contain non-standard evaluation components.
If you are wrapping this function in other functions, the argument
must be passed in a way that does not evaluate the list, e.g.
using rlang's  embrace operator \code{{{ . }}}.}

\item{package}{(\code{string})\cr
a package name that will be temporarily loaded when function
specified in \code{method} is executed.}

\item{env}{The environment in which to evaluate \code{expr}. This
environment is not applicable for quosures because they have
their own environments.}

\item{termlabels}{character vector giving the right-hand side of a
    model formula.  May be zero-length.}

\item{response}{a character string, symbol or call giving the left-hand
    side of a model formula, or \code{NULL}.}

\item{intercept}{logical: should the formula have an intercept?}

\item{x}{(\code{character})\cr
character vector, typically of variable names}

\item{pattern, pattern_term, pattern_response}{DEPRECATED}
}
\value{
depends on the calling function
}
\description{
These functions help construct calls to various types of models.
}
\details{
\itemize{
\item \code{construct_model()}: Builds models of the form \verb{method(data = data, formula = formula, method.args!!!)}.
If the \code{package} argument is specified, that package is temporarily attached
when the model is evaluated.
\item \code{reformulate2()}: This is a copy of \code{reformulate()} except that variable
names that contain a space are wrapped in backticks.
\item \code{bt()}: Adds backticks to a character vector.
\item \code{bt_strip()}: Removes backticks from a string if it begins and ends with a backtick.
}
}
\examples{
\dontshow{if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("withr", "lme4", "broom.helpers", "broom.mixed")))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
construct_model(
  data = mtcars,
  formula = am ~ mpg + (1 | vs),
  method = "glmer",
  method.args = list(family = binomial),
  package = "lme4"
) |>
  broom.mixed::tidy()

construct_model(
  data = mtcars |> dplyr::rename(`M P G` = mpg),
  formula = reformulate2(c("M P G", "cyl"), response = "hp"),
  method = "lm"
) |>
  ard_regression() |>
  dplyr::filter(stat_name \%in\% c("term", "estimate", "p.value"))
\dontshow{\}) # examplesIf}
}
