# BindableWidgets 🎛️

**Two-Way Bindable UI Widgets and Reactive App State Management for R Shiny**

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R](https://img.shields.io/badge/R-%3E%3D%204.0-brightgreen.svg)]()
[![Shiny](https://img.shields.io/badge/Shiny-1.7%2B-blue.svg)]()

`BindableWidgets` provides a complete toolkit for managing, persisting, and synchronizing application state in R Shiny. It includes drop-in replacements for standard Shiny inputs (`numericInput2`, `textInput2`, `selectInput2`, `checkboxInput2`, `sliderInput2`), an interactive tabular sequencer (`bindableTableInput`) with multi-row grouping and repeats, and a unified state engine (`bind_all_settings`, `save_settings`, `load_settings`, `export_settings_handler`, `import_settings_upload`).

---

## The Problem

In non-trivial Shiny applications—such as instrument controllers, scientific pipelines, parameter optimizers, or multi-step wizard apps—managing state is notoriously challenging:
1. **Scattered State**: Standard Shiny inputs live independently in `input[[id]]`. Passing them to background tasks or business logic requires repetitive boilerplate.
2. **One-Way UI Updates**: When loading a saved preset or configuration from disk, you must manually write dozens of `updateNumericInput`, `updateTextInput`, etc.
3. **Tabular Step Sequencers**: Hardware actuation and automation workflows (e.g., linear actuators, syringe pumps, incubation heaters) require specifying loops and wait times. Standard table packages (`DT`, `rhandsontable`) do not natively support contiguous row grouping with repeat counters, drag-and-drop reordering of grouped units, or seamless two-way binding to reactive states.

## The Solution

`BindableWidgets` bridges this gap:
- **Drop-in Widgets**: Add a `'2'` to your input names (`numericInput2`, etc.) and provide optional `subgroups`.
- **One-Line State Binding**: `bind_all_settings(session, input, settings)` creates continuous two-way synchronization between the UI and your central `reactiveValues()` container.
- **Automated Preset Loading**: `load_settings(session, settings, "config.json")` reads the file, updates your `settings` container, and automatically pushes updated values back to all UI widgets.
- **Full JSON & RDS Persistence**: Clean serialization with audit metadata (timestamp, user, version).
- **Tabular Sequencer (`bindableTableInput`)**: Interactive table with cell editing, multi-row grouping, loop repeat counts, and `expand_protocol()` compiler.

---

## Installation

Install the package directly from GitHub:

```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

devtools::install_github("jaywarrick/R-Package-BindableWidgets")
```

Or clone the repository locally and install:

```r
devtools::install("/path/to/R-Package-BindableWidgets")
```

---

## Quickstart: Application State Management

```r
library(shiny)
library(shinythemes)
library(BindableWidgets)

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  titlePanel("Instrument Controller"),

  sidebarLayout(
    sidebarPanel(
      # Drop-in bindable inputs with subgroup tagging
      numericInput2("laser_power", "Laser Power (mW)", value = 50, min = 0, max = 100, subgroups = "optical"),
      sliderInput2("exposure_ms", "Exposure Time (ms)", min = 10, max = 500, value = 100, subgroups = "optical"),
      textInput2("sample_id", "Sample Name", value = "Plate_01", subgroups = "experiment"),
      selectInput2("filter_cube", "Filter Cube", choices = c("DAPI", "FITC", "Cy5"), subgroups = "optical"),
      checkboxInput2("save_raw", "Save Raw Images", value = TRUE, subgroups = "experiment"),

      hr(),
      actionButton("btn_save", "Save Preset", class = "btn-primary"),
      actionButton("btn_load", "Load Preset", class = "btn-default"),
      downloadButton("btn_export", "Export JSON")
    ),

    mainPanel(
      h4("Central Settings Container (Reactive State)"),
      verbatimTextOutput("state_preview")
    )
  )
)

server <- function(input, output, session) {
  # 1. Create a reactiveValues container
  settings <- reactiveValues()

  # 2. Wire two-way binding in one line
  bind_all_settings(session, input, settings)

  # Inspect live state
  output$state_preview <- renderPrint({
    rvsToList(settings)
  })

  # 3. Save state (JSON or RDS)
  observeEvent(input$btn_save, {
    save_settings(settings, file_path = "preset.json", format = "json")
  })

  # 4. Load state (UI inputs update automatically!)
  observeEvent(input$btn_load, {
    load_settings(session, settings, file_path = "preset.json")
  })

  # 5. Export via downloadHandler
  output$btn_export <- downloadHandler(
    filename = function() "settings.json",
    content = export_settings_handler(settings, format = "json")
  )
}

shinyApp(ui, server)
```

---

## Tabular Sequencer: `bindableTableInput`

The `bindableTableInput` widget provides a complete interface for building multi-step protocols with repeating cycles.

```r
# Define column schema
cols <- list(
  use = list(name = "Use", type = logical(0), default = TRUE),
  name = list(name = "Step Name", type = character(0), default = "Step"),
  position = list(name = "Position (%)", type = numeric(0), min = 0, max = 100, default = 0),
  speed = list(name = "Speed (%)", type = numeric(0), min = 1, max = 100, default = 100),
  hold = list(name = "Hold (s)", type = numeric(0), min = 0.1, default = 1.0)
)

# In UI:
bindableTableInput("magnet_steps", label = "Actuation Sequence", columns = cols, data = initial_dt)
```

### Features
1. **Cell Editing**: Click to edit names, toggles, and numeric parameters with validation constraints.
2. **Contiguous Row Grouping**: Click-drag or Shift+Click across rows, then click `Group & Repeat`. The widget creates a merged repeat cell with an inline cycle counter and ungroup (`✕`) button.
3. **Smart Drag-and-Drop Handles**: Grouped rows share a single, merged drag handle. Dragging a group moves the entire loop as an atomic block and prevents illegal insertions into the middle of another group.
4. **Logical Unit Alternating Shading**: Instead of alternating single rows, background shading alternates by logical execution block (`unit-shade-a` and `unit-shade-b`). If rows 2–5 are grouped, row 1 is shade A, rows 2–5 all share shade B, row 6 is shade A, and row 7 is shade B.
5. **Theme Inheritance**: Inherits typography, colors, and borders directly from whichever Shiny/Bootstrap theme is active (`spacelab`, `flatly`, `slate`, etc.).

### Unrolling into Sequential Tasks (`expand_protocol`)

Convert your compressed loop protocol into a linear execution queue:

```r
# Given a grouped protocol table with 10x repeats on steps 2 & 3:
expanded <- expand_protocol(input$magnet_steps, filter_active = TRUE)

# Output:
# execution_step  name                   position  speed  hold  cycle_index  total_cycles
#              1  Homing / Pre-position         0    100   1.0            1             1
#              2  Raise Magnet (Top)            0    100  10.0            1            10
#              3  Lower Magnet (Bottom)        90    100   5.0            1            10
#              4  Raise Magnet (Top)            0    100  10.0            2            10
#              5  Lower Magnet (Bottom)        90    100   5.0            2            10
#            ...  ...
```

---

## State Management Architecture (POP-Interface Pattern)

`BindableWidgets` was designed from real-world laboratory instrumentation (`POP-Interface`). Here is the recommended architecture pattern:

```
[ UI Inputs (numericInput2, table, etc.) ]
                   │
                   ▼  (User Interaction)
         [ bind_all_settings() ]
                   │
                   ▼  (Two-Way Sync)
     [ settings (reactiveValues) ] ──────────────► [ Background Hardware Tasks / Queues ]
                   ▲
                   │  (load_settings() / presets)
         [ Disk / Presets JSON ]
```

### Freezing UI During Runs

Use `setWidgetState()` to prevent user modification of parameters while hardware or tasks are executing:

```r
# When starting a protocol:
setWidgetState(enabled = FALSE)

# When task finishes or aborts:
setWidgetState(enabled = TRUE)
```

---

## Included Examples

Run the bundled example applications:

```r
# 1. Simple application state management
shiny::runApp(system.file("examples/01_simple_app", package = "BindableWidgets"))

# 2. Hardware protocol sequencer with table grouping and repeats
shiny::runApp(system.file("examples/02_protocol_sequencer", package = "BindableWidgets"))
```

---

## API Summary

| Category | Function | Description |
| :--- | :--- | :--- |
| **Widgets** | `numericInput2()` | Bindable numeric input with subgroup registration |
| | `textInput2()` | Bindable text input with subgroup registration |
| | `selectInput2()` | Bindable select / multi-select input |
| | `checkboxInput2()` | Bindable boolean checkbox |
| | `sliderInput2()` | Bindable slider / range slider |
| | `bindableTableInput()` | Interactive tabular sequencer with grouping and repeats |
| | `updateBindableTableInput()` | Push updated data.table to the client table widget |
| **State Management** | `bind_all_settings()` | Establishes continuous two-way reactive synchronization |
| | `save_settings()` | Serializes state to RDS or JSON with metadata |
| | `load_settings()` | Deserializes state and updates reactiveValues + UI |
| | `export_settings_handler()` | Prepares download handler for settings export |
| | `import_settings_upload()` | Imports settings from Shiny `fileInput` |
| | `rvsToList()` | Recursively converts reactive values to pure R lists |
| | `setWidgetState()` | Enables or disables registered inputs |
| **Protocol Sequencing** | `expand_protocol()` | Unrolls grouped sequences into linear execution steps |
| | `normalize_bindable_table_data()` | Validates tracking columns (`_step_id`, `_group_id`, `_repeats`) |
| | `coerce_bindable_table_payload()` | Converts client JSON payload to strict `data.table` |

---

## License

MIT © Jay Warrick / Salus Team

