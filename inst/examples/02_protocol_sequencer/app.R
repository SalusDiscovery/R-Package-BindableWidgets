# ==============================================================================
# BindableWidgets Example 02: Hardware Protocol Sequencer
# Demonstrates bindableTableInput, row grouping, repeat counts, and task planning
# ==============================================================================

library(shiny)
library(shinythemes)
library(data.table)
library(BindableWidgets)

# Initial protocol template
initial_protocol <- data.table(
  use = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  name = c("Home Actuator", "Raise Magnet", "Lower Magnet", "Bead Binding Wait", "Retract Magnet"),
  position = c(0, 0, 90, 90, 0),
  speed = c(100, 100, 100, 20, 100),
  hold = c(1.0, 5.0, 5.0, 15.0, 2.0),
  `_step_id` = 1:5,
  `_group_id` = c(NA_integer_, 1L, 1L, NA_integer_, NA_integer_),
  `_repeats` = c(1L, 10L, 10L, 1L, 1L)
)

cols <- list(
  use = list(name = "Use", type = logical(0), default = TRUE),
  name = list(name = "Step Name", type = character(0), default = "Step"),
  position = list(name = "Position (%)", type = numeric(0), min = 0, max = 100, default = 0),
  speed = list(name = "Speed (%)", type = numeric(0), min = 1, max = 100, default = 100),
  hold = list(name = "Hold (s)", type = numeric(0), min = 0.1, default = 1.0)
)

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  titlePanel("BindableWidgets: Hardware Protocol Sequencer"),

  fluidRow(
    column(
      width = 7,
      div(
        class = "well",
        h4("Protocol Table Sequencer"),
        p("Multi-select rows (click-drag or Shift+Click) to group and repeat. Reorder rows with drag handles."),
        bindableTableInput("protocol_steps", columns = cols, data = initial_protocol, subgroups = "actuator"),
        hr(),
        fluidRow(
          column(4, fileInput("upload_protocol", "Import Protocol (JSON)", accept = ".json")),
          column(4, br(), downloadButton("download_protocol", "Export Protocol", class = "btn-success btn-block")),
          column(4, br(), actionButton("btn_reset", "Reset Default", class = "btn-default btn-block"))
        )
      )
    ),

    column(
      width = 5,
      div(
        class = "well",
        h4("Expanded Execution Sequence"),
        p("Unrolled linear steps ready for task queue dispatch:"),
        tableOutput("expanded_table")
      ),
      div(
        class = "well",
        h4("Raw Bound data.table"),
        verbatimTextOutput("raw_dt_output")
      )
    )
  )
)

server <- function(input, output, session) {
  settings <- reactiveValues()
  bind_all_settings(session, input, settings)

  # Reactively expand protocol into flat execution plan
  expanded_plan <- reactive({
    req(input$protocol_steps)
    expand_protocol(input$protocol_steps)
  })

  output$expanded_table <- renderTable({
    plan <- expanded_plan()
    if (nrow(plan) == 0) return(NULL)
    plan[, .(Step = execution_step, Name = name, Pos = position, Speed = speed, Hold = hold, Cycle = paste0(cycle_index, "/", total_cycles))]
  })

  output$raw_dt_output <- renderPrint({
    req(settings$protocol_steps)
    print(settings$protocol_steps)
  })

  # Export protocol to JSON
  output$download_protocol <- downloadHandler(
    filename = function() paste0("protocol_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"),
    content = function(file) {
      save_settings(settings, file_path = file, format = "json")
    }
  )

  # Import protocol from JSON
  observeEvent(input$upload_protocol, {
    req(input$upload_protocol$datapath)
    import_settings_upload(session, settings, input$upload_protocol)
  })

  # Reset to default
  observeEvent(input$btn_reset, {
    updateBindableTableInput(session, "protocol_steps", value = initial_protocol)
  })
}

if (interactive()) {
  shinyApp(ui, server)
}
