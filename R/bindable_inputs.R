#' Bindable Numeric Input
#'
#' A drop-in replacement for [shiny::numericInput()] that registers itself with the
#' BindableWidgets state manager, enabling automatic two-way synchronization and
#' serialization via [bind_all_settings()], [save_settings()], and [load_settings()].
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param label Display label for the control, or `NULL` for no label.
#' @param value Initial value.
#' @param min Minimum allowed value.
#' @param max Maximum allowed value.
#' @param step Interval to use when stepping through values.
#' @param width The width of the input, e.g. \code{'400px'} or \code{'100\%'}.
#' @param subgroups Optional character vector of setting subgroups this input belongs to.
#'
#' @return A numeric input control that can be added to a UI definition.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   numericInput2("gain", "Detector Gain", value = 1.0, min = 0.1, max = 10, subgroups = "camera")
#' }
numericInput2 <- function(inputId, label, value, min = NA, max = NA, step = NA, width = NULL, subgroups = character(0)) {
  register_input(inputId, "numeric", subgroups)
  shiny::numericInput(inputId = inputId, label = label, value = value, min = min, max = max, step = step, width = width)
}

#' Bindable Text Input
#'
#' A drop-in replacement for [shiny::textInput()] that registers itself with the
#' BindableWidgets state manager.
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param label Display label for the control, or `NULL` for no label.
#' @param value Initial value.
#' @param width The width of the input, e.g. `'400px'`.
#' @param placeholder A character string giving the user a hint as to what can be entered.
#' @param subgroups Optional character vector of setting subgroups this input belongs to.
#'
#' @return A text input control that can be added to a UI definition.
#' @export
textInput2 <- function(inputId, label, value = "", width = NULL, placeholder = NULL, subgroups = character(0)) {
  register_input(inputId, "text", subgroups)
  shiny::textInput(inputId = inputId, label = label, value = value, width = width, placeholder = placeholder)
}

#' Bindable Select Input
#'
#' A drop-in replacement for [shiny::selectInput()] that registers itself with the
#' BindableWidgets state manager.
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param label Display label for the control, or `NULL` for no label.
#' @param choices List of values to select from.
#' @param selected The initially selected value (or multiple values if `multiple = TRUE`).
#' @param multiple Is selection of multiple items allowed?
#' @param selectize Whether to use selectize.js.
#' @param width The width of the input.
#' @param size The number of items to show in the selection list.
#' @param subgroups Optional character vector of setting subgroups this input belongs to.
#'
#' @return A select list control that can be added to a UI definition.
#' @export
selectInput2 <- function(inputId, label, choices, selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL, subgroups = character(0)) {
  type <- if (isTRUE(multiple)) "select-multiple" else "select"
  register_input(inputId, type, subgroups)
  shiny::selectInput(inputId = inputId, label = label, choices = choices, selected = selected, multiple = multiple, selectize = selectize, width = width, size = size)
}

#' Bindable Checkbox Input
#'
#' A drop-in replacement for [shiny::checkboxInput()] that registers itself with the
#' BindableWidgets state manager.
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param label Display label for the control, or `NULL` for no label.
#' @param value Initial value (`TRUE` or `FALSE`).
#' @param width The width of the input.
#' @param subgroups Optional character vector of setting subgroups this input belongs to.
#'
#' @return A checkbox input control that can be added to a UI definition.
#' @export
checkboxInput2 <- function(inputId, label, value = FALSE, width = NULL, subgroups = character(0)) {
  register_input(inputId, "checkbox", subgroups)
  shiny::checkboxInput(inputId = inputId, label = label, value = value, width = width)
}

#' Bindable Slider Input
#'
#' A drop-in replacement for [shiny::sliderInput()] that registers itself with the
#' BindableWidgets state manager.
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param label Display label for the control, or `NULL` for no label.
#' @param min The minimum value (inclusive).
#' @param max The maximum value (inclusive).
#' @param value The initial value of the slider.
#' @param step Specifies the interval between each slider value.
#' @param round `TRUE` to round all values to the nearest integer.
#' @param format Deprecated in Shiny, included for compatibility.
#' @param locale The locale to use for formatting.
#' @param ticks `FALSE` to hide tick marks, `TRUE` to show them.
#' @param animate `TRUE` to show a simple animation control.
#' @param width The width of the input.
#' @param sep Separator between thousands.
#' @param pre A prefix string to put in front of the value.
#' @param post A suffix string to put after the value.
#' @param timeFormat Only used if the slider values are Date or POSIXt objects.
#' @param timezone Only used if the slider values are POSIXt objects.
#' @param dragRange This option is used only if it is a slider of numbers with range.
#' @param subgroups Optional character vector of setting subgroups this input belongs to.
#' @param ... Additional arguments passed to [shiny::sliderInput()].
#'
#' @return A slider control that can be added to a UI definition.
#' @export
sliderInput2 <- function(inputId, label, min, max, value, step = NULL, round = FALSE,
                         format = NULL, locale = "us", ticks = TRUE, animate = FALSE,
                         width = NULL, sep = ",", pre = NULL, post = NULL,
                         timeFormat = NULL, timezone = NULL, dragRange = TRUE,
                         subgroups = character(0), ...) {
  register_input(inputId, "slider", subgroups)
  shiny::sliderInput(
    inputId = inputId, label = label, min = min, max = max, value = value,
    step = step, round = round, format = format, locale = locale, ticks = ticks,
    animate = animate, width = width, sep = sep, pre = pre, post = post,
    timeFormat = timeFormat, timezone = timezone, dragRange = dragRange, ...
  )
}

#' Toggle the Enabled / Disabled State of UI Inputs
#'
#' Disables or enables a set of input widgets. Extremely useful for freezing
#' the user interface while a hardware protocol, long-running calculation, or task sequence is running.
#' Requires `shinyjs::useShinyjs()` in the UI definition.
#'
#' @param enabled Logical; if `TRUE`, enables the inputs; if `FALSE`, disables them.
#' @param ids Character vector of input IDs to toggle. If omitted or empty, all registered inputs are toggled.
#'
#' @return Invisibly returns `ids`.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Freeze all settings while running
#'   setWidgetState(enabled = FALSE)
#'   # Later, re-enable
#'   setWidgetState(enabled = TRUE)
#' }
setWidgetState <- function(enabled = TRUE, ids = character(0)) {
  if (length(ids) == 0) {
    types <- get_registered_types()
    ids <- names(types)
  }

  if (requireNamespace("shinyjs", quietly = TRUE)) {
    for (id in ids) {
      if (isTRUE(enabled)) {
        shinyjs::enable(id)
      } else {
        shinyjs::disable(id)
      }
    }
  } else {
    warning("package 'shinyjs' is required to enable/disable widgets via setWidgetState()")
  }

  invisible(ids)
}
