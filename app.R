
library(shiny)
library(dplyr)
library(ggplot2)

data <- read.csv("dates.csv", stringsAsFactors = FALSE) %>%
  rename(
    country = Country,
    year = Year
  )

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .irs--shiny .irs-bar {
        background: #FF6F91;
        border-top: 1px solid #FF6F91;
        border-bottom: 1px solid #FF6F91;
      }
      .irs--shiny .irs-single {
        background: #FF6F91;
      }
      .irs--shiny .irs-handle > i:first-child {
        background: #FF6F91;
      }
    "))
  ),
  
  titlePanel("Age at First Birth (2012–2023)"),
  
  plotOutput("age_plot", height = "600px"),
  
  fluidRow(
    column(
      width = 8,
      offset = 2,
      sliderInput(
        "year",
        "Select year:",
        min = min(data$year, na.rm = TRUE),
        max = max(data$year, na.rm = TRUE),
        value = min(data$year, na.rm = TRUE),
        step = 1,
        sep = "",
        animate = animationOptions(interval = 1000),
        width = "100%"
      )
    )
  )
)

server <- function(input, output) {
  
  output$age_plot <- renderPlot({
    
    data_year <- data %>%
      filter(year == input$year) %>%
      filter(!is.na(Women)) %>%
      arrange(desc(Women))
    
    ggplot(
      data_year,
      aes(
        x = reorder(country, Women),
        y = Women
      )
    ) +
      geom_point(
        size = 3.6,
        color = "lightcoral"
      ) +
      labs(
        title = paste("Year:", input$year),
        x = "",
        y = "Age at First Birth (Women)"
      ) +
      theme_minimal(base_size = 15) +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 10,
          family = "serif"
        ),
        axis.text.y = element_text(
          size = 10,
          family = "serif"
        )
      ) +
      scale_y_continuous(breaks = seq(15, 45, by = 1)) +
      coord_cartesian(clip = "off")
    
  })
}

shinyApp(ui = ui, server = server)


