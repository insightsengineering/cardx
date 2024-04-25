#' Construction Helpers
#'
#' These functions help construct calls to various types of models.
#'
#' - `construct_model()`: Builds models of the form `method(data = data, formula = formula, method.args!!!)`.
#'    If the `package` argument is specified, that package is temporarily attached
#'    when the model is evaluated.
#'
#' - `reformulate2()`: This is a copy of `reformulate()` except that variable
#'    names that contain a space are wrapped in backticks.
#'
#' - `bt()`: Adds backticks to a character vector.
#'
#' - `bt_strip()`: Removes backticks from a string if it begins and ends with a backtick.
#'
#' @param x
#'  - `construct_model.data.frame()` (`data.frame`) a data frame
#'  - `construct_model.survey.design()` (`survey.design`) a survey design object
#'  - `bt()`/`bt_strip()` (`character`) character vector, typically of variable names
#' @param formula (`formula`)\cr
#'   a formula
#' @param method (`string`)\cr
#'   string naming the function to be called, e.g. `"glm"`.
#'   If function belongs to a library that is not attached, the package name
#'   must be specified in the `package` argument.
#' @param method.args (named `list`)\cr
#'   named list of arguments that will be passed to `fn`.
#'
#'   Note that this list may contain non-standard evaluation components.
#'   If you are wrapping this function in other functions, the argument
#'   must be passed in a way that does not evaluate the list, e.g.
#'   using rlang's  embrace operator `{{ . }}`.
#' @param package (`string`)\cr
#'   string of package name that will be temporarily loaded when function
#'   specified in `method` is executed.
#' @param pattern (`string`)\cr
#'   regular expression string. If the regex matches, backticks are added
#'   to the string. When `NULL`, backticks are not added.
#' @param pattern_term,pattern_response passed to `bt(pattern)` for arguments
#'   `stats::reformulate(termlabels, response)`.
#' @inheritParams rlang::eval_tidy
#' @inheritParams stats::reformulate
#' @inheritParams rlang::args_dots_empty
#'
#' @return depends on the calling function
#' @name construction_helpers
#'
#' @examplesIf do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("withr", "lme4"), reference_pkg = "cardx"))
#' construct_model(
#'   x = mtcars,
#'   formula = am ~ mpg + (1 | vs),
#'   method = "glmer",
#'   method.args = list(family = binomial),
#'   package = "lme4"
#' )
#'
#' construct_model(
#'   x = mtcars |> dplyr::rename(`M P G` = mpg),
#'   formula = reformulate2(c("M P G", "cyl"), response = "hp"),
#'   method = "lm"
#' ) |>
#'   ard_regression() |>
#'   dplyr::filter(stat_name %in% c("term", "estimate", "p.value"))
NULL

#' @rdname construction_helpers
#' @export
construct_model <- function(x, ...) {
  UseMethod("construct_model")
}

#' @rdname construction_helpers
#' @export
construct_model.data.frame <- function(x, formula, method, method.args = list(), package = "base", env = caller_env(), ...) {
  set_cli_abort_call()
  # check pkg installations ----------------------------------------------------
  check_dots_empty()
  check_pkg_installed(c("withr", package), reference_pkg = "cardx")

  check_not_missing(formula)
  check_class(formula, cls = "formula")

  check_not_missing(method)
  check_string(method)
  check_not_namespaced(method)

  # convert method.args to list of expressions (to account for NSE inputs) -----
  method.args <- .as_list_of_exprs({{ method.args }})

  # build model ----------------------------------------------------------------
  withr::with_namespace(
    package = package,
    call2(.fn = method, formula = formula, data = x, !!!method.args) |>
      eval_tidy(env = env)
  )
}

#' @rdname construction_helpers
#' @export
construct_model.survey.design <- function(x, formula, method, method.args = list(), package = "survey", env = caller_env(), ...) {
  set_cli_abort_call()
  # check pkg installations ----------------------------------------------------
  check_dots_empty()
  check_pkg_installed(c("withr", package), reference_pkg = "cardx")

  check_not_missing(formula)
  check_class(formula, cls = "formula")

  check_not_missing(method)
  check_string(method)
  check_not_namespaced(method)

  # convert method.args to list of expressions (to account for NSE inputs) -----
  method.args <- .as_list_of_exprs({{ method.args }})

  # build model ----------------------------------------------------------------
  withr::with_namespace(
    package = package,
    call2(.fn = method, formula = formula, design = x, !!!method.args) |>
      eval_tidy(env = env)
  )
}

.as_list_of_exprs <- function(x) {
  call_args(enexpr(x))
}

#' @rdname construction_helpers
#' @export
reformulate2 <- function(termlabels, response = NULL, intercept = TRUE,
                         pattern_term = "[ \n\r]", pattern_response = "[ \n\r]",
                         env = parent.frame()) {
  stats::reformulate(
    termlabels = bt(termlabels, pattern_term),
    response = bt(response, pattern_response),
    intercept = intercept,
    env = env
  )
}

#' @rdname construction_helpers
#' @export
bt <- function(x, pattern = "[ \n\r]") {
  if (is_empty(x)) {
    return(x)
  }
  if (is_empty(pattern)) {
    return(x)
  }
  ifelse(
    str_detect(x, pattern = pattern),
    paste0("`", x, "`"),
    x
  )
}

#' @rdname construction_helpers
#' @export
bt_strip <- function(x) {
  ifelse(
    str_detect(x, "^`.*`$"),
    substr(x, 2, nchar(x) - 1),
    x
  )
}

check_not_namespaced <- function(x,
                                 arg_name = rlang::caller_arg(x),
                                 class = "check_not_namespaced",
                                 call = get_cli_abort_call()) {
  check_string(x, arg_name = arg_name, call = call, class = "check_not_namespaced")

  if (str_detect(x, "::")) {
    c("Argument {.arg {arg_name}} cannot be namespaced.",
      i = "Put the package name in the {.arg package} argument."
    ) |>
      cli::cli_abort(call = call, class = class)
  }

  invisible(x)
}
