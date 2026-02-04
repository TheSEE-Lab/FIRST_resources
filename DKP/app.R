# Dragon Kill Points (DKP) - Coauthor Collaboration Tracking App
# A Shiny app for tracking author contributions to collaborative projects

library(shiny)
library(DT)
library(xml2)
library(dplyr)
library(tidyr)

# File paths
NAMES_FILE <- "names.txt"
TASKS_FILE <- "tasks.txt"

# Define task group order
TASK_GROUP_ORDER <- c("Conceptualisation", "Investigation", "Validation", 
                      "Analysis", "Writing", "Revision", "Other")

# Function to sort tasks by group order
sort_tasks_by_group <- function(tasks_list) {
  # Assign order index to each task based on group
  tasks_with_order <- lapply(tasks_list, function(task) {
    group_index <- which(TASK_GROUP_ORDER == task$group)
    if (length(group_index) == 0) {
      # If group not in predefined list, put it at the end (Other)
      group_index <- length(TASK_GROUP_ORDER)
    }
    task$group_order <- group_index
    task
  })
  
  # Sort by group order
  sorted_tasks <- tasks_with_order[order(sapply(tasks_with_order, function(t) t$group_order))]
  
  # Remove the temporary group_order field
  sorted_tasks <- lapply(sorted_tasks, function(task) {
    task$group_order <- NULL
    task
  })
  
  return(sorted_tasks)
}

# Load initial data function
load_initial_data <- function() {
  # Check if required files exist
  if (!file.exists(NAMES_FILE) || !file.exists(TASKS_FILE)) {
    return(list(
      success = FALSE,
      message = "Error: Both names.txt and tasks files are mandatory to start the app!"
    ))
  }
  
  # Load names
  names_raw <- readLines(NAMES_FILE, warn = FALSE)
  names <- names_raw[nzchar(trimws(names_raw))]
  
  if (length(names) == 0) {
    return(list(
      success = FALSE,
      message = "Error: names.txt must contain at least one name!"
    ))
  }
  
  # Load tasks
  tasks_raw <- readLines(TASKS_FILE, warn = FALSE)
  tasks_lines <- tasks_raw[nzchar(trimws(tasks_raw))]
  
  if (length(tasks_lines) == 0) {
    return(list(
      success = FALSE,
      message = "Error: tasks file must contain at least one task!"
    ))
  }
  
  # Parse tasks (format: TaskGroup:Task:Type)
  tasks_list <- list()
  for (line in tasks_lines) {
    parts <- strsplit(line, ":", fixed = TRUE)[[1]]
    if (length(parts) == 3) {
      tasks_list[[length(tasks_list) + 1]] <- list(
        group = parts[1],
        task = parts[2],
        type = parts[3]
      )
    }
  }
  
  if (length(tasks_list) == 0) {
    return(list(
      success = FALSE,
      message = "Error: tasks file must contain valid tasks in format TaskGroup:Task:Type!"
    ))
  }
  
  # Sort tasks by group order
  tasks_list <- sort_tasks_by_group(tasks_list)
  
  # Create initial data frame
  df <- data.frame(Author = names, stringsAsFactors = FALSE)
  
  # Add task columns (use only task name as column name)
  for (i in seq_along(tasks_list)) {
    task_info <- tasks_list[[i]]
    col_name <- task_info$task  # Only use task name
    df[[col_name]] <- ""
  }
  
  # Add special columns
  df$Senior <- ""
  df$Equal <- ""
  df$Exclude <- ""
  df$Total <- 0
  
  return(list(
    success = TRUE,
    data = df,
    tasks = tasks_list,
    names = names
  ))
}

# Save data to XML
save_to_xml <- function(df, tasks_list, filename, project_title = "Untitled Project", threshold = NA) {
  root <- xml_new_root("CoauthorData")
  
  # Save project metadata
  metadata_node <- xml_add_child(root, "Metadata")
  xml_add_child(metadata_node, "ProjectTitle", project_title)
  xml_add_child(metadata_node, "Threshold", as.character(threshold))
  xml_add_child(metadata_node, "SaveDate", as.character(Sys.Date()))
  
  # Save tasks
  tasks_node <- xml_add_child(root, "Tasks")
  for (task in tasks_list) {
    task_node <- xml_add_child(tasks_node, "Task")
    xml_add_child(task_node, "Group", task$group)
    xml_add_child(task_node, "Name", task$task)
    xml_add_child(task_node, "Type", task$type)
  }
  
  # Save authors and their data
  authors_node <- xml_add_child(root, "Authors")
  for (i in 1:nrow(df)) {
    author_node <- xml_add_child(authors_node, "Author")
    xml_add_child(author_node, "Name", as.character(df$Author[i]))
    
    # Add points for each task
    points_node <- xml_add_child(author_node, "Points")
    for (task in tasks_list) {
      col_name <- task$task  # Only use task name
      if (col_name %in% names(df)) {
        point_node <- xml_add_child(points_node, "Point")
        xml_set_attr(point_node, "task", col_name)
        xml_text(point_node) <- as.character(df[[col_name]][i])
      }
    }
    
    # Add special attributes
    xml_add_child(author_node, "Senior", as.character(df$Senior[i]))
    xml_add_child(author_node, "Equal", as.character(df$Equal[i]))
    xml_add_child(author_node, "Exclude", as.character(df$Exclude[i]))
  }
  
  write_xml(root, filename)
}

# Load data from XML
load_from_xml <- function(filename) {
  if (!file.exists(filename)) {
    return(list(success = FALSE, message = "XML file not found!"))
  }
  
  tryCatch({
    doc <- read_xml(filename)
    
    # Load project metadata
    project_title <- "Untitled Project"
    threshold <- NA
    
    metadata_node <- xml_find_first(doc, "//Metadata")
    if (!is.na(metadata_node)) {
      title_node <- xml_find_first(metadata_node, "./ProjectTitle")
      if (!is.na(title_node)) {
        project_title <- xml_text(title_node)
      }
      
      threshold_node <- xml_find_first(metadata_node, "./Threshold")
      if (!is.na(threshold_node)) {
        threshold_text <- xml_text(threshold_node)
        if (threshold_text != "NA" && threshold_text != "") {
          threshold <- as.numeric(threshold_text)
        }
      }
    }
    
    # Load tasks
    tasks_list <- list()
    task_nodes <- xml_find_all(doc, "//Task")
    for (node in task_nodes) {
      group <- xml_text(xml_find_first(node, "./Group"))
      name <- xml_text(xml_find_first(node, "./Name"))
      type <- xml_text(xml_find_first(node, "./Type"))
      tasks_list[[length(tasks_list) + 1]] <- list(group = group, task = name, type = type)
    }
    
    # Sort tasks by group order
    tasks_list <- sort_tasks_by_group(tasks_list)
    
    # Load authors
    author_nodes <- xml_find_all(doc, "//Author")
    names <- character()
    data_list <- list()
    
    for (node in author_nodes) {
      author_name <- xml_text(xml_find_first(node, "./Name"))
      names <- c(names, author_name)
      
      # Get points
      point_nodes <- xml_find_all(node, ".//Point")
      points_data <- list()
      for (pt_node in point_nodes) {
        task_name <- xml_attr(pt_node, "task")
        points_data[[task_name]] <- xml_text(pt_node)
      }
      
      # Get special attributes
      senior <- xml_text(xml_find_first(node, "./Senior"))
      equal <- xml_text(xml_find_first(node, "./Equal"))
      exclude <- xml_text(xml_find_first(node, "./Exclude"))
      
      data_list[[author_name]] <- list(
        points = points_data,
        senior = senior,
        equal = equal,
        exclude = exclude
      )
    }
    
    # Reconstruct data frame
    df <- data.frame(Author = names, stringsAsFactors = FALSE)
    
    # Add task columns (use only task name)
    for (task in tasks_list) {
      col_name <- task$task  # Only use task name
      df[[col_name]] <- ""
    }
    
    # Fill in data
    for (i in 1:nrow(df)) {
      author_name <- df$Author[i]
      if (author_name %in% names(data_list)) {
        author_data <- data_list[[author_name]]
        
        # Fill points
        for (col_name in names(author_data$points)) {
          if (col_name %in% names(df)) {
            df[[col_name]][i] <- author_data$points[[col_name]]
          }
        }
        
        # Fill special columns
        df$Senior[i] <- author_data$senior
        df$Equal[i] <- author_data$equal
        df$Exclude[i] <- author_data$exclude
      }
    }
    
    # Add special columns and Total
    if (!"Senior" %in% names(df)) df$Senior <- ""
    if (!"Equal" %in% names(df)) df$Equal <- ""
    if (!"Exclude" %in% names(df)) df$Exclude <- ""
    df$Total <- 0
    
    # Update files if they don't exist
    if (!file.exists(NAMES_FILE)) {
      writeLines(names, NAMES_FILE)
    }
    
    if (!file.exists(TASKS_FILE)) {
      task_lines <- sapply(tasks_list, function(t) {
        paste(t$group, t$task, t$type, sep = ":")
      })
      writeLines(task_lines, TASKS_FILE)
    }
    
    return(list(
      success = TRUE,
      data = df,
      tasks = tasks_list,
      names = names,
      project_title = project_title,
      threshold = threshold
    ))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error loading XML:", e$message)))
  })
}

# UI
ui <- fluidPage(
  titlePanel("Dragon Kill Points - Coauthor Collaboration Tracker"),
  
  # Add CSS for table borders
  tags$head(
    tags$style(HTML("
      .dataTables_wrapper table.dataTable {
        border-collapse: collapse !important;
        font-size: 9pt !important;
      }
      .dataTables_wrapper table.dataTable thead th,
      .dataTables_wrapper table.dataTable thead td {
        border: 1px solid #ddd !important;
        padding: 8px !important;
        font-size: 9pt !important;
      }
      .dataTables_wrapper table.dataTable tbody td {
        border: 1px solid #ddd !important;
        padding: 8px !important;
        font-size: 9pt !important;
      }
      .dataTables_wrapper table.dataTable tbody tr:hover {
        background-color: #f5f5f5 !important;
      }
      /* Hide tfoot if it exists */
      .dataTables_wrapper table.dataTable tfoot {
        display: none !important;
      }
    "))
  ),
  
  mainPanel(
    width = 12,
    
    # Error/info messages
    uiOutput("message_box"),
    
    # Project title and color randomizer
    fluidRow(
      column(6,
             textInput("project_title", "Project Title:", value = "Untitled Project", width = "100%")
      ),
      column(6,
             br(),
             actionButton("randomize_colors_btn", "Randomize Task Category Colors", class = "btn-info", icon = icon("palette"))
      )
    ),
    
    br(),
    
    # Main table
    DTOutput("data_table"),
    
    br(),
    
    # Controls row 1: Add task and author
    fluidRow(
      column(3,
             textInput("new_task", "Add Task (TaskGroup:Task:Type):", ""),
             actionButton("add_task_btn", "Add Task", class = "btn-primary")
      ),
      column(3,
             textInput("new_author", "Add Author:", ""),
             actionButton("add_author_btn", "Add Author", class = "btn-primary")
      ),
      column(3,
             numericInput("threshold", "Minimum Score Threshold:", value = NA, min = 0, step = 0.1),
             helpText("Leave empty for no threshold")
      ),
      column(3,
             br(),
             actionButton("calculate_btn", "Calculate Totals", class = "btn-success btn-lg", style = "width: 100%;")
      )
    ),
    
    br(),
    
    # Author list output
    wellPanel(
      h4("Author List (in order):"),
      verbatimTextOutput("author_list")
    ),
    
    # XML and CSV save controls
    fluidRow(
      column(4,
             br(),
             actionButton("save_xml_btn", "Save to XML", class = "btn-info"),
             actionButton("load_xml_btn", "Load from XML", class = "btn-warning")
      ),
      column(4,
             br(),
             actionButton("save_csv_btn", "Save to CSV", class = "btn-success")
      )
    ),
    
    br(),
    
    # Status messages
    textOutput("status_message")
  )
)

# Server
server <- function(input, output, session) {
  
  # Function to generate random pastel colors
  generate_pastel_colors <- function(n) {
    colors <- character(n)
    for (i in 1:n) {
      # Generate random hue (0-360)
      hue <- runif(1, 0, 360)
      # Pastel colors: high lightness (75-90%) and low-medium saturation (30-60%)
      saturation <- runif(1, 30, 60)
      lightness <- runif(1, 75, 90)
      
      # Convert HSL to hex color
      colors[i] <- hsl_to_hex(hue, saturation, lightness)
    }
    return(colors)
  }
  
  # Helper function to convert HSL to hex
  hsl_to_hex <- function(h, s, l) {
    s <- s / 100
    l <- l / 100
    
    c <- (1 - abs(2 * l - 1)) * s
    x <- c * (1 - abs((h / 60) %% 2 - 1))
    m <- l - c / 2
    
    if (h < 60) {
      r <- c; g <- x; b <- 0
    } else if (h < 120) {
      r <- x; g <- c; b <- 0
    } else if (h < 180) {
      r <- 0; g <- c; b <- x
    } else if (h < 240) {
      r <- 0; g <- x; b <- c
    } else if (h < 300) {
      r <- x; g <- 0; b <- c
    } else {
      r <- c; g <- 0; b <- x
    }
    
    r <- round((r + m) * 255)
    g <- round((g + m) * 255)
    b <- round((b + m) * 255)
    
    return(sprintf("#%02X%02X%02X", r, g, b))
  }
  
  # Reactive values to store data
  rv <- reactiveValues(
    data = NULL,
    tasks = NULL,
    names = NULL,
    initialized = FALSE,
    message = NULL,
    group_colors = NULL  # Store custom colors
  )
  
  # Initialize data on startup
  observe({
    if (!rv$initialized) {
      # Show modal dialog to choose: load XML or start fresh
      showModal(modalDialog(
        title = "Welcome to Dragon Kill Points",
        p("Would you like to load an existing XML file or start with fresh data?"),
        fileInput("startup_xml_file", "Choose XML File to Load:", 
                  accept = c(".xml", "text/xml", "application/xml"),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"),
        footer = tagList(
          actionButton("modal_start_fresh", "Start Fresh (use names.txt & tasks.txt)", class = "btn-success")
        ),
        easyClose = FALSE
      ))
    }
  })
  
  # Handle XML file selection from startup modal
  observeEvent(input$startup_xml_file, {
    req(input$startup_xml_file)
    
    filename <- input$startup_xml_file$datapath
    original_name <- input$startup_xml_file$name
    
    result <- load_from_xml(filename)
    
    if (result$success) {
      rv$data <- result$data
      rv$tasks <- result$tasks
      rv$names <- result$names
      rv$initialized <- TRUE
      rv$message <- NULL
      
      # Restore project title and threshold
      updateTextInput(session, "project_title", value = result$project_title)
      updateNumericInput(session, "threshold", value = result$threshold)
      
      # Calculate totals automatically after loading
      calculate_totals(silent = TRUE)
      
      removeModal()
      showNotification(paste("Data loaded from", original_name), type = "message")
    } else {
      showNotification(result$message, type = "error")
    }
  })
  
  # Handle "Start Fresh" from startup modal
  observeEvent(input$modal_start_fresh, {
    removeModal()
    
    init_data <- load_initial_data()
    
    if (init_data$success) {
      rv$data <- init_data$data
      rv$tasks <- init_data$tasks
      rv$names <- init_data$names
      rv$initialized <- TRUE
      rv$message <- NULL
      showNotification("Started with fresh data from names.txt and tasks.txt", type = "message")
    } else {
      rv$message <- init_data$message
      showNotification(init_data$message, type = "error")
    }
  })
  
  # Display message if initialization failed
  output$message_box <- renderUI({
    if (!is.null(rv$message)) {
      wellPanel(
        style = "background-color: #f8d7da; border-color: #f5c6cb; color: #721c24;",
        h4("Error"),
        p(rv$message)
      )
    }
  })
  
  # Render the editable table
  output$data_table <- renderDT({
    req(rv$initialized)
    req(rv$data)
    req(rv$tasks)
    
    # Generate color palette for task groups
    unique_groups <- unique(sapply(rv$tasks, function(t) t$group))
    n_groups <- length(unique_groups)
    
    # Use custom colors if available, otherwise use default palette
    if (!is.null(rv$group_colors) && length(rv$group_colors) == n_groups) {
      group_colors <- rv$group_colors
    } else {
      # Default color palette for task groups (pastel colors for better readability)
      group_colors <- c(
        "#FFE5B4", "#E0BBE4", "#B4E5FF", "#B4FFB4", "#FFB4B4",
        "#FFFFB4", "#FFB4E5", "#E5B4FF", "#B4FFE5", "#FFD4B4"
      )
    }
    
    # Assign colors to groups
    group_color_map <- setNames(
      group_colors[1:min(n_groups, length(group_colors))],
      unique_groups
    )
    
    # Prepare data for group header row (first row with colspan)
    group_header_data <- list()
    
    # Author column in group header
    group_header_data[[length(group_header_data) + 1]] <- list(
      text = "",
      rowspan = 2,
      style = "background-color: #f0f0f0; font-weight: bold; vertical-align: middle;"
    )
    
    # Task group headers with colspan
    current_group <- NULL
    group_span <- 0
    
    for (col_name in names(rv$data)[-1]) {  # Skip Author column
      if (col_name %in% c("Senior", "Equal", "Exclude", "Total")) {
        # Special columns - add any pending group first
        if (!is.null(current_group) && group_span > 0) {
          bg_color <- group_color_map[[current_group]]
          group_header_data[[length(group_header_data) + 1]] <- list(
            text = current_group,
            colspan = group_span,
            style = paste0("background-color: ", bg_color, "; font-weight: bold; text-align: center; border: 1px solid #ddd;")
          )
          current_group <- NULL
          group_span <- 0
        }
        # Don't add these to group header yet
      } else {
        # Find task group
        matching_task <- NULL
        for (task in rv$tasks) {
          if (task$task == col_name) {
            matching_task <- task
            break
          }
        }
        
        if (!is.null(matching_task)) {
          if (is.null(current_group)) {
            # Start new group
            current_group <- matching_task$group
            group_span <- 1
          } else if (current_group == matching_task$group) {
            # Continue current group
            group_span <- group_span + 1
          } else {
            # New group, add previous group header
            bg_color <- group_color_map[[current_group]]
            group_header_data[[length(group_header_data) + 1]] <- list(
              text = current_group,
              colspan = group_span,
              style = paste0("background-color: ", bg_color, "; font-weight: bold; text-align: center; border: 1px solid #ddd;")
            )
            
            # Start new group
            current_group <- matching_task$group
            group_span <- 1
          }
        }
      }
    }
    
    # Add last group if any
    if (!is.null(current_group) && group_span > 0) {
      bg_color <- group_color_map[[current_group]]
      group_header_data[[length(group_header_data) + 1]] <- list(
        text = current_group,
        colspan = group_span,
        style = paste0("background-color: ", bg_color, "; font-weight: bold; text-align: center; border: 1px solid #ddd;")
      )
    }
    
    # Add special columns to group header (with rowspan=2 to span both header rows)
    group_header_data[[length(group_header_data) + 1]] <- list(
      text = "Senior",
      rowspan = 2,
      style = "background-color: #FF9800; color: white; font-weight: bold; vertical-align: middle; border: 1px solid #ddd;"
    )
    group_header_data[[length(group_header_data) + 1]] <- list(
      text = "Equal",
      rowspan = 2,
      style = "background-color: #FF9800; color: white; font-weight: bold; vertical-align: middle; border: 1px solid #ddd;"
    )
    group_header_data[[length(group_header_data) + 1]] <- list(
      text = "Exclude",
      rowspan = 2,
      style = "background-color: #FF9800; color: white; font-weight: bold; vertical-align: middle; border: 1px solid #ddd;"
    )
    group_header_data[[length(group_header_data) + 1]] <- list(
      text = "Total",
      rowspan = 2,
      style = "background-color: #4CAF50; color: white; font-weight: bold; vertical-align: middle; border: 1px solid #ddd;"
    )
    
    # Prepare data for second header row (individual task names)
    task_header_data <- list()
    
    for (col_name in names(rv$data)) {
      if (col_name == "Author" || col_name %in% c("Senior", "Equal", "Exclude", "Total")) {
        # Skip - these use rowspan=2 in first row
        next
      }
      
      # Find which task group this belongs to
      matching_task <- NULL
      for (task in rv$tasks) {
        if (task$task == col_name) {
          matching_task <- task
          break
        }
      }
      
      if (!is.null(matching_task)) {
        bg_color <- group_color_map[[matching_task$group]]
        style_str <- paste0("background-color: ", bg_color, "; font-weight: bold; border: 1px solid #ddd;")
      } else {
        style_str <- "background-color: #f0f0f0; border: 1px solid #ddd;"
      }
      
      task_header_data[[length(task_header_data) + 1]] <- list(
        text = col_name,
        style = style_str
      )
    }
    
    # Create header with two rows
    sketch <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          lapply(group_header_data, function(cell) {
            if (!is.null(cell$colspan)) {
              th(cell$text, colspan = as.character(cell$colspan), style = cell$style)
            } else if (!is.null(cell$rowspan)) {
              th(cell$text, rowspan = as.character(cell$rowspan), style = cell$style)
            } else {
              th(cell$text, style = cell$style)
            }
          })
        ),
        tr(
          lapply(task_header_data, function(cell) {
            th(cell$text, style = cell$style)
          })
        )
      ),
      tbody()  # Add explicit empty tbody
    ))
    
    datatable(
      rv$data,
      container = sketch,
      editable = list(target = "cell", disable = list(columns = c(0, ncol(rv$data) - 1))),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 't',
        ordering = FALSE,
        searching = FALSE,
        orderClasses = FALSE,
        drawCallback = JS(
          "function(settings) {",
          "  var api = this.api();",
          "  var table = $(api.table().node());",
          "  ",
          "  // Remove any extra rows from thead (keep only first 2 rows)",
          "  var theadRows = table.find('thead tr');",
          "  if (theadRows.length > 2) {",
          "    theadRows.slice(2).remove();",
          "  }",
          "  ",
          "  // Remove any tbody rows that contain th elements instead of td",
          "  table.find('tbody tr').each(function() {",
          "    if ($(this).find('th').length > 0) {",
          "      $(this).remove();",
          "    }",
          "  });",
          "  ",
          "  // Remove any completely empty rows from tbody",
          "  table.find('tbody tr').each(function() {",
          "    var row = $(this);",
          "    var cells = row.find('td');",
          "    var allEmpty = true;",
          "    ",
          "    cells.each(function() {",
          "      if ($(this).text().trim() !== '') {",
          "        allEmpty = false;",
          "        return false;",
          "      }",
          "    });",
          "    ",
          "    if (allEmpty && cells.length > 0) {",
          "      row.remove();",
          "    }",
          "  });",
          "}"
        )
      ),
      rownames = FALSE,
      filter = 'none'
    )
  })
  
  # Handle cell edits
  observeEvent(input$data_table_cell_edit, {
    info <- input$data_table_cell_edit
    rv$data <- editData(rv$data, info, rownames = FALSE)
  })
  
  # Add new task
  observeEvent(input$add_task_btn, {
    req(rv$initialized)
    
    task_text <- trimws(input$new_task)
    if (task_text == "") return()
    
    parts <- strsplit(task_text, ":", fixed = TRUE)[[1]]
    if (length(parts) != 3) {
      showNotification("Task must be in format TaskGroup:Task:Type", type = "error")
      return()
    }
    
    group <- parts[1]
    task_name <- parts[2]
    task_type <- parts[3]
    
    if (!task_type %in% c("Binary", "Quant")) {
      showNotification("Type must be either 'Binary' or 'Quant'", type = "error")
      return()
    }
    
    # Add to tasks list and sort by group order
    rv$tasks[[length(rv$tasks) + 1]] <- list(group = group, task = task_name, type = task_type)
    rv$tasks <- sort_tasks_by_group(rv$tasks)
    
    # Rebuild data frame with columns in correct order
    # Save special columns and Total
    special_cols <- rv$data[, c("Senior", "Equal", "Exclude", "Total"), drop = FALSE]
    
    # Start with Author column
    new_df <- data.frame(Author = rv$data$Author, stringsAsFactors = FALSE)
    
    # Add task columns in sorted order
    for (task in rv$tasks) {
      col_name <- task$task
      if (col_name %in% names(rv$data)) {
        # Keep existing data
        new_df[[col_name]] <- rv$data[[col_name]]
      } else {
        # New column, fill with empty strings
        new_df[[col_name]] <- ""
      }
    }
    
    # Add back special columns
    new_df <- cbind(new_df, special_cols)
    
    rv$data <- new_df
    
    # Update tasks file
    task_lines <- sapply(rv$tasks, function(t) paste(t$group, t$task, t$type, sep = ":"))
    writeLines(task_lines, TASKS_FILE)
    
    updateTextInput(session, "new_task", value = "")
    showNotification("Task added successfully!", type = "message")
  })
  
  # Add new author
  observeEvent(input$add_author_btn, {
    req(rv$initialized)
    
    author_name <- trimws(input$new_author)
    if (author_name == "") return()
    
    if (author_name %in% rv$data$Author) {
      showNotification("Author already exists!", type = "error")
      return()
    }
    
    # Create new row with empty cells
    new_row <- data.frame(Author = author_name, stringsAsFactors = FALSE)
    for (col in names(rv$data)[-1]) {
      new_row[[col]] <- if (col == "Total") 0 else ""
    }
    
    rv$data <- rbind(rv$data, new_row)
    rv$names <- c(rv$names, author_name)
    
    # Update names file
    writeLines(rv$names, NAMES_FILE)
    
    updateTextInput(session, "new_author", value = "")
    showNotification("Author added successfully!", type = "message")
  })
  
  # Randomize task category colors
  observeEvent(input$randomize_colors_btn, {
    req(rv$initialized)
    req(rv$tasks)
    
    # Get unique task groups
    unique_groups <- unique(sapply(rv$tasks, function(t) t$group))
    n_groups <- length(unique_groups)
    
    # Generate new random pastel colors
    rv$group_colors <- generate_pastel_colors(n_groups)
    
    showNotification("Task category colors randomized!", type = "message")
  })
  
  # Helper function to calculate totals
  calculate_totals <- function(silent = FALSE) {
    req(rv$initialized)
    req(rv$data)
    req(rv$tasks)
    
    df <- rv$data
    
    # Calculate totals for each author
    for (i in 1:nrow(df)) {
      total <- 0
      
      for (task in rv$tasks) {
        col_name <- task$task  # Only use task name
        if (col_name %in% names(df)) {
          val <- df[[col_name]][i]
          
          # Convert empty to 0
          if (is.na(val) || val == "" || trimws(val) == "") {
            numeric_val <- 0
          } else {
            numeric_val <- suppressWarnings(as.numeric(val))
            if (is.na(numeric_val)) numeric_val <- 0
          }
          
          if (task$type == "Binary") {
            # Binary: just add the value (0 or 1)
            total <- total + numeric_val
          } else if (task$type == "Quant") {
            # Quant: calculate relative value
            # First, get sum for this task across all authors
            task_sum <- 0
            for (j in 1:nrow(df)) {
              task_val <- df[[col_name]][j]
              if (is.na(task_val) || task_val == "" || trimws(task_val) == "") {
                task_numeric <- 0
              } else {
                task_numeric <- suppressWarnings(as.numeric(task_val))
                if (is.na(task_numeric)) task_numeric <- 0
              }
              task_sum <- task_sum + task_numeric
            }
            
            # Add relative value
            if (task_sum > 0) {
              total <- total + (numeric_val / task_sum)
            }
          }
        }
      }
      
      df$Total[i] <- round(total, 2)
    }
    
    rv$data <- df
    
    if (!silent) {
      showNotification("Totals calculated!", type = "message")
    }
  }
  
  # Calculate totals and generate author list
  observeEvent(input$calculate_btn, {
    calculate_totals(silent = FALSE)
  })
  
  # Generate author list
  output$author_list <- renderText({
    req(rv$initialized)
    req(rv$data)
    
    df <- rv$data
    
    # Apply threshold if provided
    threshold <- input$threshold
    if (!is.na(threshold)) {
      df <- df[df$Total >= threshold, ]
    }
    
    # Separate authors by category
    senior_authors <- df[!is.na(df$Senior) & df$Senior != "" & df$Senior != "0", ]
    excluded_authors <- df[!is.na(df$Exclude) & df$Exclude != "" & df$Exclude != "0", ]
    
    # Remove senior and excluded from main pool
    main_pool <- df[!(df$Author %in% senior_authors$Author) & !(df$Author %in% excluded_authors$Author), ]
    
    # Handle equal groups
    equal_groups <- list()
    equal_processed <- character()
    
    for (i in 1:nrow(main_pool)) {
      author <- main_pool$Author[i]
      if (author %in% equal_processed) next
      
      equal_val <- main_pool$Equal[i]
      if (!is.na(equal_val) && equal_val != "" && equal_val != "0") {
        # Find all authors with same equal value
        equal_group <- main_pool[!is.na(main_pool$Equal) & main_pool$Equal == equal_val, ]
        if (nrow(equal_group) > 0) {
          equal_groups[[length(equal_groups) + 1]] <- equal_group
          equal_processed <- c(equal_processed, equal_group$Author)
        }
      }
    }
    
    # Remove equal group members from main pool
    main_pool <- main_pool[!(main_pool$Author %in% equal_processed), ]
    
    # Sort main pool by Total (descending)
    main_pool <- main_pool[order(-main_pool$Total), ]
    
    # Build author list
    author_list <- character()
    
    # Add main pool authors
    for (i in 1:nrow(main_pool)) {
      author_list <- c(author_list, main_pool$Author[i])
    }
    
    # Insert equal groups at appropriate positions
    for (group in equal_groups) {
      # Find highest Total in group
      max_total <- max(group$Total)
      
      # Find position where this total would be inserted
      insert_pos <- 1
      for (i in seq_along(author_list)) {
        author_in_list <- author_list[i]
        author_total <- main_pool$Total[main_pool$Author == author_in_list]
        if (length(author_total) > 0 && author_total[1] >= max_total) {
          insert_pos <- i + 1
        } else {
          break
        }
      }
      
      # Sort group alphabetically by last name
      group <- group[order(sapply(strsplit(group$Author, " "), function(x) x[length(x)])), ]
      
      # Add superscript #
      group_names <- paste0(group$Author, "^#")
      
      # Insert group
      if (insert_pos <= length(author_list)) {
        author_list <- c(author_list[1:(insert_pos - 1)], group_names, author_list[insert_pos:length(author_list)])
      } else {
        author_list <- c(author_list, group_names)
      }
    }
    
    # Add senior authors at the end (alphabetically by surname)
    if (nrow(senior_authors) > 0) {
      senior_authors <- senior_authors[order(sapply(strsplit(senior_authors$Author, " "), function(x) x[length(x)])), ]
      senior_names <- paste0(senior_authors$Author, "*")
      author_list <- c(author_list, senior_names)
    }
    
    # Format output
    if (length(author_list) == 0) {
      return("No authors meet the criteria.")
    }
    
    paste(author_list, collapse = ", ")
  })
  
  # Save to XML
  observeEvent(input$save_xml_btn, {
    req(rv$initialized)
    req(rv$data)
    req(rv$tasks)
    
    # Generate filename based on project title and date
    project_name <- gsub("[^A-Za-z0-9_-]", "_", input$project_title)  # Sanitize project name
    date_str <- format(Sys.Date(), "%Y%m%d")
    filename <- paste0(project_name, "_DKP_", date_str, ".xml")
    
    tryCatch({
      save_to_xml(rv$data, rv$tasks, filename, input$project_title, input$threshold)
      output$status_message <- renderText(paste("Data saved to", filename))
      showNotification(paste("Data saved to", filename), type = "message")
    }, error = function(e) {
      showNotification(paste("Error saving:", e$message), type = "error")
    })
  })
  
  # Load from XML
  observeEvent(input$load_xml_btn, {
    # Open file dialog to select XML file
    tryCatch({
      filename <- file.choose()
      if (is.null(filename) || filename == "") {
        showNotification("No file selected", type = "error")
        return()
      }
      
      result <- load_from_xml(filename)
      
      if (result$success) {
        rv$data <- result$data
        rv$tasks <- result$tasks
        rv$names <- result$names
        rv$initialized <- TRUE
        rv$message <- NULL
        
        # Restore project title and threshold
        updateTextInput(session, "project_title", value = result$project_title)
        updateNumericInput(session, "threshold", value = result$threshold)
        
        # Calculate totals automatically after loading
        calculate_totals(silent = TRUE)
        
        output$status_message <- renderText(paste("Data loaded from", basename(filename)))
        showNotification("Data loaded successfully!", type = "message")
      } else {
        showNotification(result$message, type = "error")
      }
    }, error = function(e) {
      # User cancelled file selection
      if (!grepl("cannot open the connection", e$message)) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    })
  })
  
  # Save to CSV
  observeEvent(input$save_csv_btn, {
    req(rv$initialized)
    req(rv$data)
    
    # Generate filename based on project title and date
    project_name <- gsub("[^A-Za-z0-9_-]", "_", input$project_title)  # Sanitize project name
    date_str <- format(Sys.Date(), "%Y%m%d")
    filename <- paste0(project_name, "_DKP_", date_str, ".csv")
    
    tryCatch({
      write.csv(rv$data, filename, row.names = FALSE)
      output$status_message <- renderText(paste("Data saved to", filename))
      showNotification(paste("Data saved to", filename), type = "message")
    }, error = function(e) {
      showNotification(paste("Error saving CSV:", e$message), type = "error")
    })
  })
  
  output$status_message <- renderText("")
}

# Run the app
shinyApp(ui = ui, server = server)
