#' Expand Protocol Loop Groups into Linear Execution Steps
#'
#' Unrolls a grouped sequence table from \code{\link{bindableTableInput}()} into a flat,
#' sequentially indexed execution plan suitable for automated task runners,
#' hardware controllers (such as linear actuators, pumps, heaters), or batch processors.
#'
#' @param protocol_dt A \code{data.table} or \code{data.frame} from \code{bindableTableInput}.
#' @param filter_active Logical; if \code{TRUE}, steps with \code{use == FALSE} are skipped.
#'
#' @return A \code{data.table} with expanded rows, an incremental \code{execution_step}
#'   counter, \code{cycle_index}, and \code{total_cycles}.
#' @export
#'
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   use = c(TRUE, TRUE, TRUE),
#'   name = c("Prime", "Oscillate Up", "Oscillate Down"),
#'   position = c(0, 100, 10),
#'   speed = c(100, 50, 50),
#'   hold = c(1, 2, 2),
#'   `_step_id` = 1:3,
#'   `_group_id` = c(NA_integer_, 1L, 1L),
#'   `_repeats` = c(1L, 3L, 3L)
#' )
#' expanded <- expand_protocol(dt)
#' print(expanded[, .(execution_step, name, cycle_index, total_cycles)])
expand_protocol <- function(protocol_dt, filter_active = TRUE) {
  if (is.null(protocol_dt) || nrow(protocol_dt) == 0) {
    return(data.table::data.table())
  }

  dt <- data.table::copy(data.table::as.data.table(protocol_dt))

  # Filter active rows if requested
  if (filter_active && "use" %in% names(dt)) {
    dt <- dt[dt$use == TRUE]
  }

  if (nrow(dt) == 0) {
    return(data.table::data.table())
  }

  # Build execution sequence by identifying contiguous group runs
  expanded_rows <- list()
  i <- 1L
  n <- nrow(dt)

  while (i <= n) {
    row <- dt[i]
    raw_gid <- row$`_group_id`
    is_grouped <- !is.null(raw_gid) && !is.na(raw_gid) && is.numeric(raw_gid) && raw_gid > 0

    if (!is_grouped) {
      single_row <- data.table::copy(row)
      single_row[, `:=`(cycle_index = 1L, total_cycles = 1L)]
      expanded_rows[[length(expanded_rows) + 1L]] <- single_row
      i <- i + 1L
    } else {
      # Grouped block: find extent of contiguous rows sharing the same group_id
      gid <- as.integer(raw_gid)
      grp_start <- i
      while (i <= n && !is.na(dt[i]$`_group_id`) && as.integer(dt[i]$`_group_id`) == gid) {
        i <- i + 1L
      }
      grp_end <- i - 1L
      grp_rows <- dt[grp_start:grp_end]

      repeats <- as.integer(grp_rows$`_repeats`[1])
      if (is.na(repeats) || repeats < 1L) repeats <- 1L

      for (cycle in seq_len(repeats)) {
        cycle_dt <- data.table::copy(grp_rows)
        cycle_dt[, `:=`(cycle_index = cycle, total_cycles = repeats)]
        expanded_rows[[length(expanded_rows) + 1L]] <- cycle_dt
      }
    }
  }

  res <- data.table::rbindlist(expanded_rows, use.names = TRUE, fill = TRUE)
  res[, execution_step := seq_len(.N)]
  return(res)
}
