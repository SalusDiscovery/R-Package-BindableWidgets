#' Convert ReactiveValues to Plain R List
#'
#' Recursively extracts reactive values into an unreactive named R list.
#' Nested \code{reactivevalues} objects are recursively traversed and converted.
#'
#' @param rv A Shiny \code{reactivevalues} object.
#'
#' @return A nested R list containing all current values.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   values <- reactiveValues(a = 1, b = "hello")
#'   plain_list <- rvsToList(values)
#' }
rvsToList <- function(rv) {
  if (!inherits(rv, "reactivevalues")) {
    return(rv)
  }
  lst <- shiny::reactiveValuesToList(rv)
  for (name in names(lst)) {
    if (inherits(lst[[name]], "reactivevalues")) {
      lst[[name]] <- rvsToList(lst[[name]])
    }
  }
  lst
}

#' Bind All Registered Settings to Reactive Values
#'
#' Establishes continuous two-way synchronization between Shiny UI input widgets
#' and a central \code{reactiveValues} container. Whenever a user interacts with a widget,
#' the reactive values container (and any designated subgroups) is updated immediately.
#'
#' @param session The Shiny session object.
#' @param input The Shiny input object.
#' @param settings A Shiny \code{reactivevalues} object storing application settings.
#' @param input_types Optional named list of input types. Defaults to \code{\link{get_registered_types}()}.
#' @param input_subgroups Optional named list of subgroups. Defaults to \code{\link{get_registered_subgroups}()}.
#'
#' @return Invisibly returns the list of bound input IDs.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   server <- function(input, output, session) {
#'     settings <- reactiveValues()
#'     bind_all_settings(session, input, settings)
#'   }
#' }
bind_all_settings <- function(session, input, settings, input_types = NULL, input_subgroups = NULL) {
  types <- get_registered_types(input_types)
  subgroups <- get_registered_subgroups(input_subgroups)

  for (input_id in names(types)) {
    type <- types[[input_id]]

    # Skip buttons or action controls
    if (type %in% c("button", "actionButton")) next

    local({
      id <- input_id
      t  <- type
      sg <- subgroups[[id]] %||% character(0)

      # Initialization observer: prime reactiveValues with initial widget values
      shiny::observe({
        # Only initialize if not already set
        if (is.null(settings[[id]])) {
          settings[[id]] <- input[[id]]
        }

        # Set in all relevant subgroup lists
        for (group_name in sg) {
          if (is.null(settings[[group_name]])) {
            settings[[group_name]] <- list()
          }
          settings[[group_name]][[id]] <- input[[id]]
        }
      })

      # Synchronization observer: keep reactiveValues synchronized with UI changes
      shiny::observeEvent(input[[id]], {
        raw_val <- input[[id]]
        val <- switch(
          t,
          "numeric"         = as.numeric(raw_val),
          "slider"          = as.numeric(raw_val),
          "checkbox"        = as.logical(raw_val),
          "text"            = as.character(raw_val),
          "select"          = as.character(raw_val),
          "select-multiple" = as.character(raw_val),
          "table"           = coerce_bindable_table_payload(raw_val),
          "bindable_table"  = coerce_bindable_table_payload(raw_val),
          raw_val           # fallback
        )

        settings[[id]] <- val
        for (group_name in sg) {
          if (is.null(settings[[group_name]])) {
            settings[[group_name]] <- list()
          }
          settings[[group_name]][[id]] <- val
        }
      }, ignoreInit = TRUE)
    })
  }

  invisible(names(types))
}

#' Save Application Settings to Disk
#'
#' Serializes the application settings stored in a \code{reactivevalues} object or
#' list to disk in either RDS or JSON format. Automatically appends audit metadata
#' such as timestamp and username.
#'
#' @param settings A Shiny \code{reactivevalues} object or list.
#' @param file_path File destination path (defaults to \code{"user_settings/current.rds"}).
#' @param format Serialization format: \code{"auto"} (inferred from file extension), \code{"rds"}, or \code{"json"}.
#' @param metadata Optional named list of custom metadata attributes to attach.
#'
#' @return The normalized file path where settings were saved.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   save_settings(settings, "my_config.json")
#' }
save_settings <- function(settings, file_path = "user_settings/current.rds",
                          format = c("auto", "rds", "json"), metadata = list()) {
  format <- match.arg(format)
  if (format == "auto") {
    format <- if (grepl("\\.json$", file_path, ignore.case = TRUE)) "json" else "rds"
  }

  # Safely extract pure list from reactivevalues
  settings_list <- if (inherits(settings, "reactivevalues")) rvsToList(settings) else as.list(settings)

  # Attach metadata
  settings_list$metadata <- utils::modifyList(
    list(
      saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      saved_by = tryCatch(Sys.info()[["user"]], error = function(e) "unknown"),
      package = "BindableWidgets",
      version = "0.1.0"
    ),
    as.list(metadata)
  )

  # Ensure target directory exists
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  if (format == "rds") {
    saveRDS(settings_list, file_path)
  } else if (format == "json") {
    jsonlite::write_json(settings_list, file_path, pretty = TRUE, auto_unbox = TRUE, null = "null", dataframe = "rows")
  }

  message("Settings saved successfully to: ", file_path)
  return(invisible(file_path))
}

#' Load Application Settings from Disk
#'
#' Reads a saved configuration file (RDS or JSON), updates the corresponding
#' \code{reactivevalues} object, and automatically pushes updated values back to
#' the registered UI widgets on the Shiny client.
#'
#' @param session The Shiny session object.
#' @param settings The Shiny \code{reactivevalues} object storing application settings.
#' @param file_path Path to the RDS or JSON settings file.
#' @param input_types Optional named list of input types. Defaults to \code{\link{get_registered_types}()}.
#'
#' @return A character vector of any registered setting IDs that were missing from the file.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   missing <- load_settings(session, settings, "my_config.json")
#' }
load_settings <- function(session, settings, file_path, input_types = NULL) {
  if (!file.exists(file_path)) {
    warning("Settings file does not exist: ", file_path)
    return(FALSE)
  }

  types <- get_registered_types(input_types)

  lst <- tryCatch({
    if (grepl("\\.json$", file_path, ignore.case = TRUE)) {
      jsonlite::read_json(file_path, simplifyVector = FALSE)
    } else {
      readRDS(file_path)
    }
  }, error = function(e) {
    warning("Failed to parse settings file: ", e$message)
    return(NULL)
  })

  if (is.null(lst)) return(FALSE)

  # Remove metadata wrapper if present
  lst$metadata <- NULL

  registered_ids <- names(types)
  missing_settings <- character(0)

  for (id in registered_ids) {
    if (!is.null(lst[[id]])) {
      val <- lst[[id]]
      settings[[id]] <- val

      type <- types[[id]] %||% "text"
      switch(
        type,
        "numeric"         = shiny::updateNumericInput(session, id, value = as.numeric(val)),
        "text"            = shiny::updateTextInput(session, id, value = as.character(val)),
        "checkbox"        = shiny::updateCheckboxInput(session, id, value = as.logical(val)),
        "select"          = shiny::updateSelectInput(session, id, selected = val),
        "select-multiple" = shiny::updateSelectInput(session, id, selected = val),
        "slider"          = shiny::updateSliderInput(session, id, value = as.numeric(val)),
        "table"           = updateBindableTableInput(session, id, value = coerce_bindable_table_payload(val)),
        "bindable_table"  = updateBindableTableInput(session, id, value = coerce_bindable_table_payload(val))
      )
    } else {
      missing_settings <- c(missing_settings, id)
      warning(paste("Missing setting in loaded configuration file:", id))
    }
  }

  return(invisible(missing_settings))
}

#' Export Settings for Shiny Download Handler
#'
#' Prepares application settings for direct user download via Shiny's \code{downloadHandler}.
#'
#' @param settings A Shiny \code{reactivevalues} object or list.
#' @param format Format to export: \code{"json"} or \code{"rds"}.
#' @param metadata Optional metadata list to attach.
#'
#' @return A function suitable for use inside \code{downloadHandler(content = ...)}.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   output$download_btn <- downloadHandler(
#'     filename = function() "settings.json",
#'     content = export_settings_handler(settings, format = "json")
#'   )
#' }
export_settings_handler <- function(settings, format = c("json", "rds"), metadata = list()) {
  format <- match.arg(format)
  function(file) {
    save_settings(settings = settings, file_path = file, format = format, metadata = metadata)
  }
}

#' Import Settings from Shiny File Input
#'
#' Convenience wrapper for \code{\link{load_settings}()} specifically designed
#' for handling uploaded files from \code{shiny::fileInput()}.
#'
#' @param session The Shiny session object.
#' @param settings The Shiny \code{reactivevalues} object.
#' @param upload_file The list or data frame provided by \code{input$file_upload}.
#' @param input_types Optional named list of input types.
#'
#' @return Missing setting names (invisibly).
#' @export
import_settings_upload <- function(session, settings, upload_file, input_types = NULL) {
  if (is.null(upload_file) || is.null(upload_file$datapath)) {
    warning("No valid file upload provided")
    return(character(0))
  }
  load_settings(session = session, settings = settings, file_path = upload_file$datapath, input_types = input_types)
}
