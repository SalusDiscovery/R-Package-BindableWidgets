#' Bindable Table Input Widget
#'
#' Creates an interactive tabular sequencer widget that supports cell editing,
#' optional contiguous multi-row grouping with repeat counts, drag-and-drop reordering,
#' row duplication, and deletion. The widget seamlessly synchronizes its tabular state
#' to Shiny inputs and settings managers via two-way reactive binding.
#'
#' @param inputId The `input` slot that will be used to access the table value.
#' @param label Optional display label for the control.
#' @param columns Named list describing the column specifications. Each entry should specify:
#'   \itemize{
#'     \item \code{name}: Display header label.
#'     \item \code{type}: Type definition (e.g. \code{logical(0)}, \code{numeric(0)}, \code{character(0)}).
#'     \item \code{default}: Default value for newly inserted rows.
#'     \item \code{min}, \code{max}, \code{step}: Optional constraints for numeric columns.
#'     \item \code{width}: Optional CSS width for the column (e.g. \code{"100px"}).
#'   }
#' @param data Initial data as a \code{data.frame} or \code{data.table}.
#' @param value Alias for \code{data}.
#' @param allow_grouping Logical; if \code{TRUE} (default), enables multi-row grouping, loop repeats, and group drag handles. If \code{FALSE}, all grouping and repeating functionality is omitted.
#' @param enable_groups Alias for \code{allow_grouping}.
#' @param subgroups Optional character vector of setting subgroups this input belongs to.
#' @param width Width of the widget container (e.g. \code{"100\%"}).
#' @param height Height of the widget container (optional).
#'
#' @return A Shiny UI widget element.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(data.table)
#'
#'   cols <- list(
#'     use = list(name = "Use", type = logical(0), default = TRUE),
#'     name = list(name = "Step Name", type = character(0), default = "Step"),
#'     position = list(name = "Position (\\%)", type = numeric(0), min = 0, max = 100, default = 0),
#'     speed = list(name = "Speed (\\%)", type = numeric(0), min = 1, max = 100, default = 100),
#'     hold = list(name = "Hold (s)", type = numeric(0), min = 0.1, default = 1.0)
#'   )
#'   bindableTableInput("seq_table", label = "Protocol Steps", columns = cols, allow_grouping = TRUE)
#' }
bindableTableInput <- function(inputId,
                               label = NULL,
                               columns = NULL,
                               data = NULL,
                               value = NULL,
                               allow_grouping = TRUE,
                               enable_groups = TRUE,
                               subgroups = character(0),
                               width = "100%",
                               height = NULL) {
  # Register widget for automatic state management
  register_input(inputId, "table", subgroups)

  # Resolve data / value alias
  actual_data <- if (!is.null(value)) value else data

  # Resolve allow_grouping / enable_groups alias
  is_grouping_allowed <- if (!missing(allow_grouping)) isTRUE(allow_grouping) else isTRUE(enable_groups)

  # Fallback default columns if omitted
  if (is.null(columns)) {
    columns <- list(
      use = list(name = "Use", type = logical(0), default = TRUE),
      name = list(name = "Step Name", type = character(0), default = "Step"),
      position = list(name = "Position (%)", type = numeric(0), min = 0, max = 100, default = 0.0),
      speed = list(name = "Speed (%)", type = numeric(0), min = 1, max = 100, default = 100.0),
      hold = list(name = "Hold (s)", type = numeric(0), min = 0.1, default = 1.0)
    )
  }

  norm_data <- normalize_bindable_table_data(actual_data, columns)

  # Serialize columns as a named list matching schema expectations
  col_specs <- list()
  for (col_id in names(columns)) {
    col_def <- columns[[col_id]]
    col_specs[[col_id]] <- list(
      id = col_id,
      name = col_def$name %||% col_id,
      type = if (is.logical(col_def$type) || identical(col_def$type, "logical")) "logical"
             else if (is.numeric(col_def$type) || identical(col_def$type, "numeric")) "numeric"
             else "character",
      default = col_def$default %||% if (is.logical(col_def$type) || identical(col_def$type, "logical")) TRUE
                                     else if (is.numeric(col_def$type) || identical(col_def$type, "numeric")) 0 else "",
      min = col_def$min,
      max = col_def$max,
      step = col_def$step,
      width = col_def$width
    )
  }

  initial_json <- jsonlite::toJSON(norm_data, dataframe = "rows", auto_unbox = TRUE, null = "null")
  cols_json <- jsonlite::toJSON(col_specs, auto_unbox = TRUE, null = "null")

  style_attr <- paste0(
    "width: ", htmltools::validateCssUnit(width), ";",
    if (!is.null(height)) paste0(" height: ", htmltools::validateCssUnit(height), ";") else ""
  )

  htmltools::tagList(
    htmltools::singleton(
      htmltools::tags$head(
        htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "bindableWidgets/bindable_table.css"),
        htmltools::tags$script(src = "bindableWidgets/bindable_table.js")
      )
    ),
    htmltools::tags$div(
      id = inputId,
      class = "bindable-table-widget",
      style = style_attr,
      `data-input-id` = inputId,
      `data-allow-grouping` = if (is_grouping_allowed) "true" else "false",
      `data-enable-groups` = if (is_grouping_allowed) "true" else "false",
      if (!is.null(label)) htmltools::tags$label(class = "bindable-table-label", label),
      htmltools::tags$script(type = "application/json", class = "bindable-table-schema", htmltools::HTML(cols_json)),
      htmltools::tags$script(type = "application/json", class = "bindable-table-initial-data", htmltools::HTML(initial_json)),
      htmltools::tags$div(class = "bindable-table-container")
    )
  )
}

#' Update a Bindable Table Input on the Client
#'
#' Sends a message to the browser to replace the rows in a [bindableTableInput()].
#'
#' @param session The Shiny session object.
#' @param inputId The ID of the `bindableTableInput` to update.
#' @param value A \code{data.frame} or \code{data.table} of replacement data.
#' @param label Optional new label for the widget.
#'
#' @return Invisibly returns `TRUE`.
#' @export
updateBindableTableInput <- function(session, inputId, value = NULL, label = NULL) {
  if (is.null(value)) return(invisible(FALSE))
  norm_val <- normalize_bindable_table_data(value)
  session$sendCustomMessage("bindableTable_setValue", list(
    id = inputId,
    rows = norm_val
  ))
  invisible(TRUE)
}

#' Normalize Data for Bindable Table
#'
#' Ensures mandatory tracking columns (\code{_step_id}, \code{_group_id}, \code{_repeats})
#' are present and correctly typed as integer vectors.
#'
#' @param data A \code{data.frame}, \code{data.table}, or \code{NULL}.
#' @param columns Optional column specifications list used to initialize defaults if data is NULL.
#'
#' @return A validated \code{data.table}.
#' @export
normalize_bindable_table_data <- function(data, columns = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    if (is.null(columns)) {
      dt <- data.table::data.table(
        use = logical(0),
        name = character(0),
        position = numeric(0),
        speed = numeric(0),
        hold = numeric(0),
        `_step_id` = integer(0),
        `_group_id` = integer(0),
        `_repeats` = integer(0)
      )
      return(dt)
    }
    cols_init <- list()
    for (nm in names(columns)) {
      cols_init[[nm]] <- rep(columns[[nm]]$default %||% NA, 0)
    }
    cols_init[["_step_id"]] <- integer(0)
    cols_init[["_group_id"]] <- integer(0)
    cols_init[["_repeats"]] <- integer(0)
    return(data.table::as.data.table(cols_init))
  }

  dt <- data.table::copy(data.table::as.data.table(data))

  n_rows <- nrow(dt)
  if (!"_step_id" %in% names(dt)) {
    dt[, `_step_id` := seq_len(n_rows)]
  } else {
    dt[, `_step_id` := as.integer(`_step_id`)]
    if (any(is.na(dt$`_step_id`))) {
      dt[is.na(`_step_id`), `_step_id` := seq_len(.N) + 1000L]
    }
  }

  if (!"_group_id" %in% names(dt)) {
    dt[, `_group_id` := NA_integer_]
  } else {
    dt[, `_group_id` := as.integer(`_group_id`)]
    dt[is.na(`_group_id`) | `_group_id` <= 0L, `_group_id` := NA_integer_]
  }

  if (!"_repeats" %in% names(dt)) {
    dt[, `_repeats` := 1L]
  } else {
    dt[, `_repeats` := as.integer(`_repeats`)]
    dt[is.na(`_repeats`) | `_repeats` < 1L, `_repeats` := 1L]
  }

  return(dt)
}

#' Coerce Client Payload from Bindable Table
#'
#' Converts JSON or list data delivered from the browser into a strictly typed
#' \code{data.table} with integer tracking columns. Unpacks wrapped \code{list(rows = ..., _rev = ...)}
#' envelopes automatically.
#'
#' @param payload A list or data.frame returned from Shiny's input handler.
#'
#' @return A clean \code{data.table}.
#' @export
coerce_bindable_table_payload <- function(payload) {
  if (is.null(payload) || length(payload) == 0) {
    return(normalize_bindable_table_data(NULL))
  }

  if (is.data.frame(payload)) {
    return(normalize_bindable_table_data(payload))
  }

  if (is.list(payload)) {
    # If payload is wrapped in an envelope: list(rows = [...], _rev = ...)
    if ("rows" %in% names(payload)) {
      payload <- payload$rows
    }

    if (is.null(payload) || length(payload) == 0) {
      return(normalize_bindable_table_data(NULL))
    }
    if (is.data.frame(payload)) {
      return(normalize_bindable_table_data(payload))
    }

    cleaned_rows <- lapply(payload, function(row) {
      lapply(row, function(val) {
        if (is.null(val) || length(val) == 0) NA else val
      })
    })

    dt <- tryCatch({
      data.table::rbindlist(cleaned_rows, fill = TRUE, use.names = TRUE)
    }, error = function(e) {
      warning("Failed to rbind payload list: ", e$message)
      data.table::data.table()
    })
    return(normalize_bindable_table_data(dt))
  }

  return(normalize_bindable_table_data(NULL))
}

#' Dynamically Enable or Disable Grouping on a Bindable Table
#'
#' Sends a message to the client browser to show or hide multi-row grouping and repeats
#' on the specified [bindableTableInput()]. When disabled, group buttons and the repeat column
#' are hidden, and all steps are treated as single ungrouped rows.
#'
#' @param session The Shiny session object.
#' @param inputId The ID of the `bindableTableInput`.
#' @param allow_grouping Logical; `TRUE` to enable grouping, `FALSE` to disable.
#'
#' @return Invisibly returns `TRUE`.
#' @export
setTableGrouping <- function(session, inputId, allow_grouping = TRUE) {
  session$sendCustomMessage("bindableTable_setAllowGrouping", list(
    id = inputId,
    allow_grouping = isTRUE(allow_grouping)
  ))
  invisible(TRUE)
}

# Register Custom Shiny Input Handler so input[[id]] automatically delivers a data.table
if (requireNamespace("shiny", quietly = TRUE)) {
  tryCatch({
    shiny::registerInputHandler("bindableWidgets.table", function(data, ...) {
      message("[registerInputHandler bindableWidgets.table] incoming data:")
      str(data)
      coerce_bindable_table_payload(data)
    }, force = TRUE)
  }, error = function(e) NULL)
}
