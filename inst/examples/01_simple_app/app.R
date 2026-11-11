# ==============================================================================
# BindableWidgets Example 01: Simple Application State Management
# Demonstrates two-way reactive state binding, auto-syncing, and saving/loading
# ==============================================================================

library(shiny)
library(shinythemes)
library(BindableWidgets)

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  titlePanel("BindableWidgets: Application State Manager Demo"),

  sidebarLayout(
    sidebarPanel(
      h4("Settings Configuration"),
      p("All widgets below are bindable. Changing any value updates the state."),
      hr(),

      # Bindable inputs with optional subgroups
      textInput2("sample_name", "Sample ID", value = "EXP_2026_01", subgroups = "experiment"),
      numericInput2("temperature", "Target Temperature (\u00B0C)", value = 65.0, min = 20, max = 100, step = 0.5, subgroups = c("hardware", "thermal")),
      sliderInput2("stir_speed", "Mixing Speed (RPM)", min = 100, max = 2000, value = 500, step = 50, subgroups = "hardware"),
      selectInput2("lysis_buffer", "Buffer Solution", choices = c("Buffer A", "Buffer B", "Standard Lysis"), selected = "Buffer B", subgroups = "experiment"),
      checkboxInput2("enable_logging", "Verbose Telemetry Logging", value = TRUE, subgroups = "diagnostics"),

      hr(),
      h4("State Persistence"),
      fluidRow(
        column(6, actionButton("btn_save", "Save State", class = "btn-primary btn-block")),
        column(6, actionButton("btn_load", "Reload State", class = "btn-default btn-block"))
      ),
      br(),
      downloadButton("btn_download_json", "Export JSON", class = "btn-info btn-block")
    ),

    mainPanel(
      h4("Live Reactive State Inspector"),
      p("Directly reading the unreactive snapshot of settings:"),
      verbatimTextOutput("state_json"),
      hr(),
      h4("Hardware Subgroup View"),
      verbatimTextOutput("hardware_json")
    )
  )
)

server <- function(input, output, session) {
  # Initialize reactive container for application settings
  settings <- reactiveValues()

  # Bind all registered widgets to the settings reactiveValues container
  bind_all_settings(session, input, settings)

  # Display the live reactive state
  output$state_json <- renderPrint({
    lst <- rvsToList(settings)
    cat(jsonlite::toJSON(lst, pretty = TRUE, auto_unbox = TRUE))
  })

  output$hardware_json <- renderPrint({
    cat("settings$hardware:\n")
    print(settings$hardware)
  })

  # Save settings to local disk
  observeEvent(input$btn_save, {
    save_file <- save_settings(settings, file_path = "app_state.json", format = "json")
    showNotification(paste("State saved to", save_file), type = "message")
  })

  # Load settings from local disk
  observeEvent(input$btn_load, {
    if (file.exists("app_state.json")) {
      missing <- load_settings(session, settings, file_path = "app_state.json")
      showNotification("State successfully restored from disk!", type = "message")
    } else {
      showNotification("No saved state file found (click 'Save State' first)", type = "warning")
    }
  })

  # Export settings as JSON download
  output$btn_download_json <- downloadHandler(
    filename = function() paste0("demo_settings_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"),
    content = function(file) {
      save_settings(settings, file_path = file, format = "json")
    }
  )
}

if (interactive()) {
  shinyApp(ui, server)
}

