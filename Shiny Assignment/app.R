# Load necessary libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(dplyr)
library(ggplot2)
library(stringr)
library(rvest)
library(readr)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "US Stadiums Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Data Table", tabName = "data_table", icon = icon("table")),
      menuItem("Summary", tabName = "summary", icon = icon("chart-bar")),
      menuItem("Statistics", tabName = "stats", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
          color: #ffffff;
          border-bottom: 0 solid transparent;
        }
        .skin-blue .main-header .navbar {
          background-color: #3c8dbc;
          color: #ffffff;
        }
        .skin-blue .main-sidebar {
          background-color: #1c1c1c;
        }
        .skin-blue .main-sidebar .sidebar {
          padding: 0;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
          background-color: #3c8dbc;
          color: #ffffff;
        }
        .content-wrapper {
          background-color: #f9fafc;
        }
        .main-footer {
          background-color: #f9fafc;
          border-top: 1px solid #d2d6de;
        }
        .box {
          border-top: 3px solid #3c8dbc;
        }
        .box-header {
          color: #3c8dbc;
        }
        .box-title {
          font-weight: bold;
        }
        .btn {
          background-color: #3c8dbc;
          color: #ffffff;
          border: none;
        }
        .btn:hover {
          background-color: #367fa9;
        }
        .form-control {
          border-radius: 0;
          box-shadow: none;
          border-color: #d2d6de;
        }
        .form-control:focus {
          border-color: #3c8dbc;
        }
        .table > tbody > tr:hover {
          background-color: #f5f5f5;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(width = 12, leafletOutput("stadiumMap", height = 500))
              ),
              fluidRow(
                box(width = 12, div(
                  HTML('<div style="text-align: center; padding: 10px;">
                         <span style="background-color: red; width: 15px; height: 15px; display: inline-block"></span> MLB
                         <span style="background-color: green; width: 15px; height: 15px; display: inline-block; margin-left: 10px;"></span> MLS
                         <span style="background-color: blue; width: 15px; height: 15px; display: inline-block; margin-left: 10px; margin-right: 5px;"></span> NFL
                       </div>')
                ))
              ),
              fluidRow(
                box(width = 6, selectInput("league_map", "Select League:", choices = NULL, selected = "All Leagues")),
                box(width = 6, selectInput("team_map", "Select Team:", choices = c("All Teams"), selected = "All Teams")),
                box(width = 12, sliderInput("capacity_map", "Select Stadium Capacity Range:", min = 10000, max = 90000, value = c(10000, 90000))),
                box(width = 12, actionButton("reset_map", "Reset Filters"))
              )
      ),
      tabItem(tabName = "data_table",
              fluidRow(
                box(width = 12, DTOutput("stadiumTable"))
              ),
              fluidRow(
                box(width = 6, selectInput("league_table", "Select League:", choices = NULL, selected = "All Leagues")),
                box(width = 6, selectInput("team_table", "Select Team:", choices = c("All Teams"), selected = "All Teams")),
                box(width = 12, sliderInput("capacity_table", "Select Stadium Capacity Range:", min = 10000, max = 90000, value = c(10000, 90000))),
                box(width = 12, actionButton("reset_table", "Reset Filters"))
              )
      ),
      tabItem(tabName = "summary",
              fluidRow(
                box(width = 12, plotOutput("capacityPlot", height = 600))
              ),
              fluidRow(
                box(width = 6, selectInput("league_summary", "Select League:", choices = NULL, selected = "All Leagues")),
                box(width = 6, selectInput("team_summary", "Select Team:", choices = c("All Teams"), selected = "All Teams")),
                box(width = 12, sliderInput("capacity_summary", "Select Stadium Capacity Range:", min = 10000, max = 90000, value = c(10000, 90000))),
                box(width = 12, actionButton("reset_summary", "Reset Filters"))
              )
      ),
      tabItem(tabName = "stats",
              fluidRow(
                box(width = 12, div(htmlOutput("summaryStats"), style = "text-align: center;")),
                box(width = 6, plotOutput("averageCapacityPlot")),
                box(width = 6, plotOutput("stadiumCountPlot")),
                box(width = 6, plotOutput("trendPlot")),
                box(width = 6, plotOutput("pieChart"))
      

              ),
              fluidRow(
                box(width = 6, selectInput("league_stats", "Select League:", choices = NULL, selected = "All Leagues")),
                box(width = 6, selectInput("team_stats", "Select Team:", choices = c("All Teams"), selected = "All Teams")),
                box(width = 12, sliderInput("capacity_stats", "Select Stadium Capacity Range:", min = 10000, max = 90000, value = c(10000, 90000))),
                box(width = 12, actionButton("reset_stats", "Reset Filters"))
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load and scrape data
  url <- "https://en.wikipedia.org/wiki/List_of_U.S._stadiums_by_capacity"
  
  tryCatch({
    webpage <- read_html(url)
    stadium_table <- html_table(html_nodes(webpage, "table")[[1]], fill = TRUE)
    stadium_data <- as.data.frame(stadium_table)
    
    # Data cleaning steps
    stadium_data <- stadium_data %>%
      mutate(Tenant = str_extract(Tenant, "^[^()]+")) %>%
      mutate(Tenant = str_trim(Tenant)) %>%
      mutate(Capacity = as.numeric(gsub("[^0-9]", "", Capacity)))
    
    stadium_data <- stadium_data %>% filter(!is.na(Capacity))
  }, error = function(e) {
    showNotification("Error loading data from Wikipedia", type = "error")
    stadium_data <- data.frame()  # Fallback to an empty data frame
  })
  
  # Load CSV file with error handling
  tryCatch({
    stadium_locations <- read_csv("stadiums.csv", show_col_types = FALSE)
  }, error = function(e) {
    showNotification("Error loading stadiums.csv", type = "error")
    stadium_locations <- data.frame()  # Fallback to an empty data frame
  })
  
  # Merge the dataframes by matching teams
  merged_data <- stadium_data %>%
    filter(sapply(Tenant, function(tenant) any(str_detect(tenant, paste0(stadium_locations$Team, collapse = "|"))))) %>%
    rowwise() %>%
    mutate(Team = stadium_locations$Team[which.max(str_detect(Tenant, stadium_locations$Team))]) %>%
    inner_join(stadium_locations, by = c("Team" = "Team")) %>%
    select(-Type, -Tenant, -Image, -Division)
  
  # Initialize filter options
  leagues <- c("All Leagues", unique(merged_data$League))
  teams <- c("All Teams", unique(merged_data$Team))
  
  # Update selectInput choices for league and team in all tabs
  updateSelectInput(session, "league_map", choices = leagues, selected = "All Leagues")
  updateSelectInput(session, "league_table", choices = leagues, selected = "All Leagues")
  updateSelectInput(session, "league_summary", choices = leagues, selected = "All Leagues")
  updateSelectInput(session, "league_stats", choices = leagues, selected = "All Leagues")
  updateSelectInput(session, "team_map", choices = teams, selected = "All Teams")
  updateSelectInput(session, "team_table", choices = teams, selected = "All Teams")
  updateSelectInput(session, "team_summary", choices = teams, selected = "All Teams")
  updateSelectInput(session, "team_stats", choices = teams, selected = "All Teams")
  
  observeEvent(input$league_map, {
    if (input$league_map == "All Leagues") {
      updateSelectInput(session, "team_map", choices = c("All Teams"), selected = "All Teams")
    } else {
      updateSelectInput(session, "team_map", choices = c("All Teams", unique(merged_data$Team[merged_data$League == input$league_map])))
    }
  })
  
  observeEvent(input$league_table, {
    if (input$league_table == "All Leagues") {
      updateSelectInput(session, "team_table", choices = c("All Teams"), selected = "All Teams")
    } else {
      updateSelectInput(session, "team_table", choices = c("All Teams", unique(merged_data$Team[merged_data$League == input$league_table])))
    }
  })
  
  observeEvent(input$league_summary, {
    if (input$league_summary == "All Leagues") {
      updateSelectInput(session, "team_summary", choices = c("All Teams"), selected = "All Teams")
    } else {
      updateSelectInput(session, "team_summary", choices = c("All Teams", unique(merged_data$Team[merged_data$League == input$league_summary])))
    }
  })
  
  observeEvent(input$league_stats, {
    if (input$league_stats == "All Leagues") {
      updateSelectInput(session, "team_stats", choices = c("All Teams"), selected = "All Teams")
    } else {
      updateSelectInput(session, "team_stats", choices = c("All Teams", unique(merged_data$Team[merged_data$League == input$league_stats])))
    }
  })
  
  # Filtered data reactive expressions for each tab
  filtered_data_map <- reactive({
    data <- merged_data
    if (input$league_map != "All Leagues") {
      data <- data %>% filter(League == input$league_map)
    }
    if (input$team_map != "All Teams") {
      data <- data %>% filter(Team == input$team_map)
    }
    data <- data %>% filter(Capacity >= input$capacity_map[1] & Capacity <= input$capacity_map[2])
    data
  })
  
  filtered_data_table <- reactive({
    data <- merged_data
    if (input$league_table != "All Leagues") {
      data <- data %>% filter(League == input$league_table)
    }
    if (input$team_table != "All Teams") {
      data <- data %>% filter(Team == input$team_table)
    }
    data <- data %>% filter(Capacity >= input$capacity_table[1] & Capacity <= input$capacity_table[2])
    data
  })
  
  filtered_data_summary <- reactive({
    data <- merged_data
    if (input$league_summary != "All Leagues") {
      data <- data %>% filter(League == input$league_summary)
    }
    if (input$team_summary != "All Teams") {
      data <- data %>% filter(Team == input$team_summary)
    }
    data <- data %>% filter(Capacity >= input$capacity_summary[1] & Capacity <= input$capacity_summary[2])
    data
  })
  
  filtered_data_stats <- reactive({
    data <- merged_data
    if (input$league_stats != "All Leagues") {
      data <- data %>% filter(League == input$league_stats)
    }
    if (input$team_stats != "All Teams") {
      data <- data %>% filter(Team == input$team_stats)
    }
    data <- data %>% filter(Capacity >= input$capacity_stats[1] & Capacity <= input$capacity_stats[2])
    data
  })
  
  # Reset filters for each tab
  observeEvent(input$reset_map, {
    updateSelectInput(session, "league_map", selected = "All Leagues")
    updateSelectInput(session, "team_map", selected = "All Teams")
    updateSliderInput(session, "capacity_map", value = c(10000, 90000))
  })
  
  observeEvent(input$reset_table, {
    updateSelectInput(session, "league_table", selected = "All Leagues")
    updateSelectInput(session, "team_table", selected = "All Teams")
    updateSliderInput(session, "capacity_table", value = c(10000, 90000))
  })
  
  observeEvent(input$reset_summary, {
    updateSelectInput(session, "league_summary", selected = "All Leagues")
    updateSelectInput(session, "team_summary", selected = "All Teams")
    updateSliderInput(session, "capacity_summary", value = c(10000, 90000))
  })
  
  observeEvent(input$reset_stats, {
    updateSelectInput(session, "league_stats", selected = "All Leagues")
    updateSelectInput(session, "team_stats", selected = "All Teams")
    updateSliderInput(session, "capacity_stats", value = c(10000, 90000))
  })
  
  # Render Leaflet map
  output$stadiumMap <- renderLeaflet({
    leaflet(filtered_data_map()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Long, lat = ~Lat, 
        popup = ~paste(
          "<b>Stadium:</b> ", Stadium, "<br>",
          "<b>Team:</b> ", Team, "<br>",
          "<b>Capacity:</b> ", Capacity, "<br>",
          "<b>Opened:</b> ", `Year opened`
        ),
        radius = 5, 
        color = ~case_when(
          League == "MLS" ~ "green",
          League == "MLB" ~ "red",
          League == "NFL" ~ "blue",
          TRUE ~ "gray"
        )
      )
  })
  
  # Render data table
  output$stadiumTable <- renderDT({
    datatable(filtered_data_table(), options = list(pageLength = 10))
  })
  
  # Render capacity plot
  output$capacityPlot <- renderPlot({
    ggplot(filtered_data_summary(), aes(x = reorder(Team, Capacity), y = Capacity, fill = League)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Capacity), hjust = -0.1) +
      coord_flip() +
      labs(title = "Stadium Capacity by Team", x = "Team", y = "Capacity") +
      theme_minimal() +
      theme(legend.position = "right")
  })
  
  # Additional statistics plots
  output$averageCapacityPlot <- renderPlot({
    avg_capacity <- filtered_data_stats() %>%
      group_by(League) %>%
      summarize(avg_capacity = mean(Capacity, na.rm = TRUE))
    
    ggplot(avg_capacity, aes(x = League, y = avg_capacity, fill = League)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(avg_capacity, 1)), vjust = -0.3) +
      labs(title = "Average Stadium Capacity by League", x = "League", y = "Average Capacity") +
      theme_minimal()
  })
  
  output$stadiumCountPlot <- renderPlot({
    stadium_count <- filtered_data_stats() %>%
      group_by(League) %>%
      summarize(count = n())
    
    ggplot(stadium_count, aes(x = League, y = count, fill = League)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = count), vjust = -0.3) +
      labs(title = "Number of Stadiums by League", x = "League", y = "Number of Stadiums") +
      theme_minimal()
  })
  
  # Render trend plot
  output$trendPlot <- renderPlot({
    trend_data <- filtered_data_stats() %>%
      group_by(`Year opened`) %>%
      summarize(avg_capacity = mean(Capacity, na.rm = TRUE))
    
    ggplot(trend_data, aes(x = `Year opened`, y = avg_capacity)) +
      geom_line(color = "blue") +
      geom_point() +
      labs(title = "Trend of Average Stadium Capacity Over Years", x = "Year Opened", y = "Average Capacity") +
      theme_minimal()
  })
  
  # Render pie chart
  output$pieChart <- renderPlot({
    league_distribution <- filtered_data_stats() %>%
      group_by(League) %>%
      summarize(count = n())
    
    ggplot(league_distribution, aes(x = "", y = count, fill = League)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Proportion of Stadiums by League") +
      theme_void() +
      theme(legend.position = "bottom")
  })
  
  # Render summary statistics
  output$summaryStats <- renderUI({
    total_stadiums <- nrow(filtered_data_stats())
    average_capacity <- mean(filtered_data_stats()$Capacity, na.rm = TRUE)
    min_capacity <- min(filtered_data_stats()$Capacity, na.rm = TRUE)
    max_capacity <- max(filtered_data_stats()$Capacity, na.rm = TRUE)
    
    HTML(paste(
      "<b>Total Number of Stadiums:</b> ", total_stadiums, "<br>",
      "<b>Average Stadium Capacity:</b> ", round(average_capacity, 1), "<br>",
      "<b>Minimum Stadium Capacity:</b> ", min_capacity, "<br>",
      "<b>Maximum Stadium Capacity:</b> ", max_capacity
    ))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
