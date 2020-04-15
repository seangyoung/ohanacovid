library(tidyverse)
library(shinythemes)

source("datagraber.R")
date_range <- unique(cleandata$date)
model_date <- max(date_range)
cols <- c("AR" = "darkgreen", "UT" = "blue", "HI" = "orange")
var.labels = c("Confirmed Cases", "Deaths")

ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  # tags$style("* { font-family: Arial; }"),
  
  titlePanel("COVID-19 Tracking for Young Ohana"),
  
  sidebarLayout(
    sidebarPanel(
      tableOutput("table2"),
      hr(),
      selectInput(inputId = "type",
                  label = h4("Select Data to Graph"),
                  choices = c("Confirmed Cases" = "positive",
                              "Deaths" = "death",
                              "Recoveries" = "recovered",
                              "Hospitalizations" = "hospitalized",
                              "Total Tests Performed" = "totalTestResults"),
                  selected = "positive"),
      hr(),
      h4("Details"),
      p("Contact for source code for this app: ", a("Sean Young", target="_blank", href="mailto:dabriase@gmail.com"))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Graph", plotOutput("plot")),
                  tabPanel("Raw Data Table", tableOutput("table")))
    )
  )
)

server <- function(input, output, session) {

  plot_input <- reactive({
    
    column_I_want = input$type
    
    ggplot(data = cleandata, aes(x = date, y = !!as.name(column_I_want), group = state)) +
      geom_line(aes(color = state), size = 1) + 
      labs(
        title   = "COVID-19 Stats for Young Ohana States",
        x       = "",
        y       = "Number of Individuals",
        color   = "",
        caption = paste0("Data from covidtracking.com, as of: ", model_date)
      ) +
      scale_colour_manual(values = cols) +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"),
            axis.text=element_text(size=10),
            axis.title=element_text(size=12),
            legend.position = "bottom")
    
  })
  
  
  output$plot <- renderPlot({ plot_input() })
  
  output$table <- renderTable({
    cleandata
  })
  
  output$table2 <- renderTable({
    slimtable
  })

}

shinyApp(ui, server)