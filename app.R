

# install.packages("shiny")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("here")
# install.packages("tidyr")
# install.packages("ggrepel")
# install.packages("wesanderson")
# install.packages("DT")
# install.packages("scales")
# install.packages("shinydashboard")
# install.packages('rsconnect')

library(shiny)
library(dplyr)
library(ggplot2)
library(here)
library(tidyr)
library(ggrepel)
library(wesanderson)
library(DT)
library(scales)
library(shinydashboard)
library(rsconnect)

# Loading data
load(here("Data", "shiny_data.RData"))
load(here("Data", "built_ucdb.RData"))



## UI


ui <- dashboardPage(
  dashboardHeader(title = "Urban Definitions Data Shiny Web App"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenu",
      menuItem("eUFA", tabName = "eUFA", icon = icon("chart-bar")),
      menuItem("Urban Centres", tabName = "UrbanCentres", icon = icon("city"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "eUFA",
              sidebarLayout(
                sidebarPanel(width = 2,
                             selectInput("year", "Select Year:", choices = unique(oe_data_shi$Year)),
                             selectInput("location", "Select City:", choices = unique(oe_data_shi$Location)),
                             selectInput("region2", "Select Comparison Group:", choices = unique(oe_data_shi$Region2), selected = "MENA"),
                             radioButtons("section", "Select:", choices = c("GDP per capita (PPP)", "Employment structure"))
                ),
                mainPanel(width = 10,
                          uiOutput("dynamicPlot"),
                          DT::dataTableOutput("dataTable")
                )
              )
      ),
      tabItem(tabName = "UrbanCentres",
              sidebarLayout(
                sidebarPanel(width = 2,
                             h4("Select ONE of the options below:"),
                             selectInput("urbanCentre", "Select City:", choices = c("", unique(built_ucdb$Urban_centre))),
                             selectInput("region", "Select Region:", choices = c("", unique(built_ucdb$Region))),
                             selectInput("country", "Select Country:", choices = c("", unique(built_ucdb$Country))),
                             actionButton("reset", "Reset Selection")
                ),
                mainPanel(width = 10,
                          uiOutput("urbanOutput"),
                          DT::dataTableOutput("urbanDataTable")
                )
              )
      )
    )
  )
)



## Server

server <- function(input, output, session) {
  
  output$dynamicPlot <- renderUI({
    if (input$sidebarMenu == "eUFA") {
      if (input$section == "GDP per capita (PPP)") {
        plotOutput("plotOutput_eUFA")
      } else if (input$section == "Employment structure") {
        plotOutput("employmentPlot_eUFA")
      }
    }
  })
  
  filteredData_eUFA <- reactive({
    req(input$location)
    oe_data_shi %>%
      filter(Location == input$location)
  })
  
  filteredDataByYear_eUFA <- reactive({
    req(input$year, input$location)
    oe_data_shi %>%
      filter(Year == input$year & Location == input$location)
  })
  
  comparisonGroupData_eUFA <- reactive({
    req(input$region2)
    oe_data_shi %>%
      filter(Region2 == input$region2 & Location != input$location)
  })
  
  comparisonGroupDataByYear_eUFA <- reactive({
    req(input$year, input$region2)
    oe_data_shi %>%
      filter(Year == input$year & Region2 == input$region2 & Location != input$location)
  })
  
  output$dataTable <- DT::renderDataTable({
    if (input$sidebarMenu == "eUFA") {
      if (input$section == "GDP per capita (PPP)") {
        data <- filteredDataByYear_eUFA() %>%
          select(-ends_with("Emp_Pct"))
        
        comparison_data <- comparisonGroupDataByYear_eUFA() %>%
          select(-ends_with("Emp_Pct"))
        
        combined_data <- bind_rows(data, comparison_data)
        combined_data <- combined_data %>%
          mutate(GDP_per_capita_PPP = scales::comma(GDP_per_capita_PPP * 1000, accuracy = 0.1))
        
        datatable(combined_data)
        
      } else if (input$section == "Employment structure") {
        data <- filteredDataByYear_eUFA() %>%
          select(-ends_with("Emp_Pct"))
        
        comparison_data <- comparisonGroupDataByYear_eUFA() %>%
          select(-ends_with("Emp_Pct"))
        
        combined_data <- bind_rows(data, comparison_data)
        combined_data <- combined_data %>%
          mutate(GDP_per_capita_PPP = scales::comma(GDP_per_capita_PPP * 1000, accuracy = 0.1))
        
        datatable(combined_data)
      }
    }
  })
  
  output$plotOutput_eUFA <- renderPlot({
    if (input$sidebarMenu == "eUFA" && input$section == "GDP per capita (PPP)") {
      
      ggplot() +
        geom_line(data = comparisonGroupData_eUFA(), 
                  aes(x = Year, y = GDP_per_capita_PPP, group = Location), 
                  color = "gray80", 
                  alpha = 0.6) +
        geom_line(data = filteredData_eUFA(), 
                  aes(x = Year, y = GDP_per_capita_PPP, group = Location), 
                  color = "red", 
                  linewidth = 1.2) +
        geom_text_repel(data = comparisonGroupData_eUFA() %>% filter(Year == max(oe_data_shi$Year)),
                        aes(x = Year, y = GDP_per_capita_PPP, label = Location),
                        color = "gray40",
                        size = 3,
                        direction = "y",
                        hjust = -0.2,
                        segment.size = 0.3,
                        force = 0.5,
                        max.overlaps = 20) + 
        geom_text_repel(data = filteredData_eUFA() %>% filter(Year == max(oe_data_shi$Year)),
                        aes(x = Year, y = GDP_per_capita_PPP, label = Location),
                        color = "red",
                        size = 5,
                        fontface = "bold",
                        direction = "y",
                        hjust = -0.9,  # Push farther
                        segment.size = 0.5,
                        force = 0.5,
                        max.overlaps = 20) + 
        labs(title = "GDP per Capita PPP over Years",
             x = "Year", 
             y = "GDP per Capita PPP") +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10),  
                           labels = scales::label_number(scale = 1, suffix = "K")) +       
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.y = element_text(size = 10),
              axis.ticks.y = element_line(linewidth = 0.5)
        )
    }
  })
  
  output$employmentPlot_eUFA <- renderPlot({
    if (input$sidebarMenu == "eUFA" && input$section == "Employment structure") {
      columns_to_pivot_Emp_Pct <- c("Public_Services_Emp_Pct",
                                    "Industry_Emp_Pct",
                                    "Financial_Busines_Services_Emp_Pct",
                                    "Consumer_Services_Emp_Pct",
                                    "Agriculture_Emp_Pct",
                                    "Transport_Information_Communic_Services_Emp_Pct")
      
      pie_data3 <- filteredDataByYear_eUFA() %>%
        pivot_longer(cols = all_of(columns_to_pivot_Emp_Pct), names_to = "Employment_sector", values_to = "Percentage") %>%
        mutate(Employment_sector = gsub("(_Emp_Pct|_)", " ", Employment_sector))
      
      ggplot(pie_data3, aes(x = Location, y = Percentage, fill = Employment_sector)) +
        geom_bar(stat = "identity", position = "fill") +
        scale_fill_manual(values = wes_palette("Zissou1", n = length(unique(pie_data3$Employment_sector)), type = "continuous")) + 
        labs(title = paste("Employment Sector Contribution, ", input$location),
             x = "Location") +
        theme_minimal() +
        scale_y_continuous(labels = scales::percent_format(), name = NULL) + 
        scale_x_discrete(name = NULL, breaks = NULL) + 
        theme(legend.title = element_blank(), legend.text = element_text(size = 11)) +
        geom_text(aes(label = scales::percent(Percentage, accuracy = 0.1)),
                  position = position_fill(vjust = 0.5), size = 4)
    }
  })
  
  # Urban Centres
  output$urbanOutput <- renderUI({
    if (!is.null(input$urbanCentre) && input$urbanCentre != "") {
      plotOutput("urbanPlotCity")
    } else if (!is.null(input$region) && input$region != "") {
      plotOutput("urbanPlotRegion")
    } else if (!is.null(input$country) && input$country != "") {
      plotOutput("urbanPlotCountry")
    }
  })
  
  urbanDataCity <- reactive({
    req(input$urbanCentre)
    built_ucdb %>%
      filter(Urban_centre == input$urbanCentre)
  })
  
  urbanDataRegion <- reactive({
    req(input$region)
    built_ucdb %>%
      filter(Region == input$region)
  })
  
  urbanDataCountry <- reactive({
    req(input$country)
    built_ucdb %>%
      filter(Country == input$country)
  })
  
  output$urbanDataTable <- DT::renderDataTable({
    if (!is.null(input$urbanCentre) && input$urbanCentre != "") {
      city_data <- urbanDataCity()
      city_summary <- city_data %>%
        filter(!is.na(Built_rel_change)) %>%
        summarise(
          Value = unique(Built_rel_change)
        )
      datatable(city_summary, options = list(pageLength = 5), rownames = FALSE)
    } else if (!is.null(input$region) && input$region != "") {
      region_data <- urbanDataRegion()
      region_summary <- region_data %>%
        filter(!is.na(Built_rel_change)) %>%
        summarise(
          Average = mean(Built_rel_change, na.rm = TRUE),
          Minimum = min(Built_rel_change, na.rm = TRUE),
          Maximum = max(Built_rel_change, na.rm = TRUE)
        )
      datatable(region_summary, options = list(pageLength = 5), rownames = FALSE)
    } else if (!is.null(input$country) && input$country != "") {
      country_data <- urbanDataCountry()
      country_summary <- country_data %>%
        filter(!is.na(Built_rel_change)) %>%
        summarise(
          Average = mean(Built_rel_change, na.rm = TRUE),
          Minimum = min(Built_rel_change, na.rm = TRUE),
          Maximum = max(Built_rel_change, na.rm = TRUE)
        )
      datatable(country_summary, options = list(pageLength = 5), rownames = FALSE)
    }
  })
  
  output$urbanPlotCity <- renderPlot({
    city_data <- urbanDataCity()
    city_value <- unique(city_data$Built_rel_change[!is.na(city_data$Built_rel_change)])
    
    ggplot() +
      geom_point(aes(x = 1, y = 1, size = city_value), shape = 21, fill = "dodgerblue", alpha = 0.7) +
      geom_point(aes(x = 1, y = 1), shape = 21, size = 5, fill = "gray80", alpha = 0.4) +  # Unit circle
      scale_size_continuous(range = c(5, 20)) +
      labs(title = paste("Built-up Surface Relative Change for", input$urbanCentre),
           x = NULL, y = NULL) +
      theme_void() +
      theme(legend.position = "none")
  })
  
  output$urbanPlotRegion <- renderPlot({
    region_data <- urbanDataRegion()
    region_avg <- mean(region_data$Built_rel_change, na.rm = TRUE)
    
    ggplot() +
      geom_point(aes(x = 1, y = 1, size = region_avg), shape = 21, fill = "dodgerblue", alpha = 0.7) +
      geom_point(aes(x = 1, y = 1), shape = 21, size = 5, fill = "gray80", alpha = 0.4) +  # Unit circle
      scale_size_continuous(range = c(5, 20)) +
      labs(title = paste("Average Built-up Surface Relative Change in", input$region),
           x = NULL, y = NULL) +
      theme_void() +
      theme(legend.position = "none")
  })
  
  output$urbanPlotCountry <- renderPlot({
    country_data <- urbanDataCountry()
    country_avg <- mean(country_data$Built_rel_change, na.rm = TRUE)
    
    ggplot() +
      geom_point(aes(x = 1, y = 1, size = country_avg), shape = 21, fill = "dodgerblue", alpha = 0.7) +
      geom_point(aes(x = 1, y = 1), shape = 21, size = 5, fill = "gray80", alpha = 0.4) +  # Unit circle
      scale_size_continuous(range = c(5, 20)) +
      labs(title = paste("Average Built-up Surface Relative Change in", input$country),
           x = NULL, y = NULL) +
      theme_void() +
      theme(legend.position = "none")
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "urbanCentre", selected = "")
    updateSelectInput(session, "region", selected = "")
    updateSelectInput(session, "country", selected = "")
  })
}



