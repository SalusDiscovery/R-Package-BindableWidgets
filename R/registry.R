#' Internal Environment for Widget Registry
#' @noRd
.registry <- new.env(parent = emptyenv())
.registry$types <- list()
.registry$subgroups <- list()

#' Register a Bindable Widget in the Registry
#'
#' Records widget type and organizational subgroup memberships for automated
#' two-way state binding and settings persistence.
#'
#' @param inputId Character string, the input widget ID.
#' @param type Character string, type classification (e.g. "numeric", "text", "checkbox", "select", "slider", "table").
#' @param subgroups Character vector of setting subgroups this input belongs to.
#'
#' @return Invisibly returns the updated registry list.
#' @export
#'
#' @examples
#' register_input("temperature_target", "numeric", c("thermal", "hardware"))
register_input <- function(inputId, type, subgroups = character(0)) {
  .registry$types[[inputId]] <- type
  .registry$subgroups[[inputId]] <- unique(subgroups)

  # Maintain backward-compatibility with global environments (e.g. legacy POP-Interface)
  if (exists("input_types", envir = .GlobalEnv, inherits = FALSE)) {
    assign("input_types", `[[<-`(.GlobalEnv$input_types, inputId, type), envir = .GlobalEnv)
  }
  if (exists("input_subgroups", envir = .GlobalEnv, inherits = FALSE)) {
    assign("input_subgroups", `[[<-`(.GlobalEnv$input_subgroups, inputId, unique(subgroups)), envir = .GlobalEnv)
  }

  invisible(list(types = .registry$types, subgroups = .registry$subgroups))
}

#' Retrieve All Registered Input Types
#'
#' @param custom_types Optional named list of types to override or supplement package registry.
#' @return Named list of input types keyed by input ID.
#' @export
get_registered_types <- function(custom_types = NULL) {
  if (!is.null(custom_types)) return(custom_types)
  if (exists("input_types", envir = .GlobalEnv, inherits = FALSE)) {
    return(c(.registry$types, .GlobalEnv$input_types))
  }
  .registry$types
}

#' Retrieve All Registered Input Subgroups
#'
#' @param custom_subgroups Optional named list of subgroups to override or supplement package registry.
#' @return Named list of character vectors of subgroup names keyed by input ID.
#' @export
get_registered_subgroups <- function(custom_subgroups = NULL) {
  if (!is.null(custom_subgroups)) return(custom_subgroups)
  if (exists("input_subgroups", envir = .GlobalEnv, inherits = FALSE)) {
    return(c(.registry$subgroups, .GlobalEnv$input_subgroups))
  }
  .registry$subgroups
}

#' Clear the Widget Registry
#'
#' Resets the internal registry of registered widget types and subgroups.
#'
#' @export
clear_registry <- function() {
  .registry$types <- list()
  .registry$subgroups <- list()
  invisible(TRUE)
}
