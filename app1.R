library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(bslib)
library(leafpop)
library(mapview)

# Define theme
my_theme <- bs_theme(
  version = 5,
  bootswatch = "cerulean",
  primary = "#0466c8",
  "navbar-bg" = "#0466c8"
)

# Data preprocessing function
preprocess_data <- function(yearly_data) {
  # Get full range of years from data
  year_range <- range(yearly_data$year)
  
  # Create complete year sequence for each county
  all_counties <- unique(yearly_data$County)
  complete_data <- expand.grid(
    County = all_counties,
    year = seq(year_range[1], year_range[2])
  )
  
  # Join with actual data
  processed_data <- left_join(complete_data, yearly_data, by = c("County", "year")) %>%
    mutate(
      total = ifelse(is.na(total), 0, total),
      has_data = !is.na(total)
    )
  
  return(processed_data)
}

# UI
ui <- navbarPage(
  theme = my_theme,
  title = "Arizona Agricultural Pesticide Usage Dashboard",
  id = "nav",
  
  # Data Explorer Page
  tabPanel(
    "Data Explorer",
    fluidPage(
      # Sidebar
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            "county",
            "Select County",
            choices = c("APACHE", "COCHISE", "COCONINO", "GILA", "GRAHAM", 
                        "GREENLEE", "LA PAZ", "MARICOPA", "MOHAVE", "NAVAJO", 
                        "PIMA", "PINAL", "SANTA CRUZ", "YAVAPAI", "YUMA"),
            selected = "YUMA"
          ),
          selectInput(
            "crop",
            "Select Crop",
            choices = NULL  # Will be updated in server
          ),
          selectInput(
            "year",
            "Select Year",
            choices = 1992:2016,
            selected = 2006
          ),
          br(),
          # Crop summary card
          uiOutput("cropSummaryCard"),
          br(),
          # Data availability card
          uiOutput("dataAvailabilityCard")
        ),
        
        # Main Panel
        mainPanel(
          width = 9,
          fluidRow(
            column(
              12,
              card(
                height = "600px",
                card_header("Geographic Distribution of Pesticide Usage"),
                leafletOutput("map", height = "500px"),
                uiOutput("mapWarning")
              )
            )
          ),
          br(),
          fluidRow(
            column(
              12,
              card(
                card_header("Yearly Total Usage by Crop"),
                plotlyOutput("yearlyPlot", height = "400px")
              )
            )
          ),
          br(),
          fluidRow(
            column(
              12,
              card(
                card_header("Pesticide Types Used"),
                plotlyOutput("pesticideBreakdown", height = "300px")
              )
            )
          )
        )
      )
    )
  ),
  
  # About Page
  tabPanel(
    "About",
    fluidPage(
      fluidRow(
        column(
          8,
          offset = 2,
          card(
            card_header(
              h2("About This Project", class = "text-center")
            ),
            card_body(
              h3("Overview"),
              p("This application visualizes agricultural pesticide usage across Arizona 
                counties by crop type, supporting research on environmental exposure 
                to agricultural chemicals."),
              
              h3("Data Sources"),
              p("The data comes from the Arizona Department of Agriculture's 
                Pesticide Use Registry. It includes commercial agricultural 
                pesticide applications, including:"),
              tags$ul(
                tags$li("All aerial applications"),
                tags$li("Soil-applied pesticides on the ADEQ ground water 
                        protection list"),
                tags$li("Certain odiforous compounds")
              ),
              
              h3("Data Coverage"),
              p("The dashboard shows pesticide applications by crop type, allowing users to:"),
              tags$ul(
                tags$li("View geographic distribution of pesticide applications for specific crops"),
                tags$li("Track yearly usage trends by crop type"),
                tags$li("Examine the types of pesticides used on each crop")
              ),
              p("Note: Not all counties have data for all years, and crop patterns 
                vary by region."),
              
              hr(),
              
              div(
                class = "text-center",
                p(
                  "Developed by ",
                  tags$a(href = "https://tuo-liu.netlify.app", "Tuo Liu"), 
                  " and ",
                  tags$a(
                    href = "https://live-azs-furlonglab.pantheonsite.io",
                    "Melissa Furlong"
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Load initial data
  pesticide_data <- reactive({
    data <- readRDS("./data/cleaned_pesticides.rds")
    return(data)
  })
  
  # Update crop choices based on county
  observe({
    req(pesticide_data())
    crops <- pesticide_data() %>%
      filter(County == input$county) %>%
      pull(Crop.Name) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "crop",
                      choices = crops,
                      selected = crops[1]
    )
  })
  
  # Filtered data for map
  map_data <- reactive({
    req(pesticide_data())
    data <- pesticide_data() %>%
      filter(
        County == input$county,
        year == input$year,
        Crop.Name == input$crop
      )
    
    if (nrow(data) == 0) return(NULL)
    data
  })
  
  # Yearly summary data
  yearly_summary <- reactive({
    req(pesticide_data())
    pesticide_data() %>%
      filter(County == input$county, Crop.Name == input$crop) %>%
      group_by(year) %>%
      summarise(
        total = sum(AI_amount, na.rm = TRUE),
        unique_pesticides = n_distinct(Active.Ingredient.new)
      ) %>%
      mutate(has_data = total > 0)
  })
  
  # Pesticide breakdown
  pesticide_breakdown <- reactive({
    req(map_data())
    map_data() %>%
      group_by(Active.Ingredient.new) %>%
      summarise(
        total = sum(AI_amount, na.rm = TRUE),
        pct = total / sum(total) * 100
      ) %>%
      arrange(desc(total))
  })
  
  # Crop summary card
  output$cropSummaryCard <- renderUI({
    req(map_data())
    
    total_amount <- sum(map_data()$AI_amount, na.rm = TRUE)
    unique_pesticides <- n_distinct(map_data()$Active.Ingredient.new)
    
    card(
      card_header("Crop Summary"),
      card_body(
        p(strong("Selected Year: "), input$year),
        p(strong("Total Pesticide Usage: "), sprintf("%.2f lbs", total_amount)),
        p(strong("Unique Pesticides: "), unique_pesticides)
      )
    )
  })
  
  # Data availability card
  output$dataAvailabilityCard <- renderUI({
    req(yearly_summary())
    
    total_years <- nrow(yearly_summary())
    years_with_data <- sum(yearly_summary()$has_data)
    
    card(
      card_header("Data Availability"),
      card_body(
        p(
          sprintf(
            "Years with data: %d of %d (%g%%)",
            years_with_data,
            total_years,
            round(years_with_data / total_years * 100, 1)
          )
        ),
        if (years_with_data < total_years) {
          div(
            class = "alert alert-warning",
            "Some years have no recorded data for this crop."
          )
        }
      )
    )
  })
  
  # Map warning
  output$mapWarning <- renderUI({
    if (is.null(map_data())) {
      div(
        class = "alert alert-warning m-3",
        "No geographic data available for the selected combination."
      )
    }
  })
  
  # Map output
  output$map <- renderLeaflet({
    if (is.null(map_data())) {
      # Return empty map centered on Arizona
      leaflet() %>%
        addTiles() %>%
        setView(lng = -111.6513, lat = 34.0489, zoom = 6)
    } else {
      m <- mapview(
        map_data(),
        crs = 4269,
        grid = FALSE,
        zcol = "AI_amount",
        popup = popupTable(
          map_data(),
          zcol = c(
            "year",
            "County",
            "Crop.Name",
            "Active.Ingredient.new",
            "AI_amount"
          )
        )
      )
      m@map
    }
  })
  
  # Yearly plot output
  output$yearlyPlot <- renderPlotly({
    req(yearly_summary())
    
    p <- ggplot(
      yearly_summary(),
      aes(
        x = as.factor(year),
        y = total,
        text = paste(
          "Year:", year,
          "<br>Total:", round(total, 2), "lbs",
          "<br>Unique Pesticides:", unique_pesticides,
          "<br>Status:", ifelse(has_data, "Reported", "No Data")
        )
      )
    ) +
      geom_point(aes(color = has_data)) +
      geom_line(group = 1, color = "#0466c8", alpha = 0.5) +
      scale_color_manual(
        values = c("TRUE" = "#0466c8", "FALSE" = "#dc3545"),
        guide = "none"
      ) +
      theme_minimal() +
      labs(
        x = "Year",
        y = "Total Pesticide Usage (lbs)",
        title = paste("Pesticide Usage for", input$crop, "in", input$county, "County")
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(b = 100)
      )
  })
  
  # Pesticide breakdown plot
  output$pesticideBreakdown <- renderPlotly({
    req(pesticide_breakdown())
    
    p <- ggplot(
      pesticide_breakdown(),
      aes(
        x = reorder(Active.Ingredient.new, -total),
        y = total,
        text = paste(
          "Pesticide:", Active.Ingredient.new,
          "<br>Amount:", round(total, 2), "lbs",
          "<br>Percentage:", round(pct, 1), "%"
        )
      )
    ) +
      geom_bar(stat = "identity", fill = "#0466c8") +
      theme_minimal() +
      labs(
        x = "Active Ingredient",
        y = "Amount (lbs)",
        title = paste("Pesticide Usage Breakdown for", input$crop)
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(b = 100)
      )
  })
}

# Run the app
shinyApp(ui, server)