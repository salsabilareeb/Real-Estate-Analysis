library(shiny)
library(dplyr)
library(ggplot2)

# Read the CSV file
data <- read.csv("F:/NEU/2nd Quarter/ALY 6070/Assignment 3 Group/th_new.csv")

# Define UI with a background image
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "body {
          background-image: url('https://resources.pollfish.com/wp-content/uploads/2020/11/MARKET_RESEARCH_FOR_REAL_ESTATE_IN_CONTENT_1.png');
          background-size: cover;
          background-repeat: no-repeat;
          background-attachment: fixed;
          background-position: center;
        }
        #contentArea {
          position: relative;
          background-color: rgba(255, 255, 255, 0.75);
        }"
      )
    )
  ),
  
  # Application title
  titlePanel("PortfolioFrontier: Exploring the Landscape of Investment"),
  
  # Create tabs with an ID for the content area
  div(id = "contentArea", 
      tabsetPanel(
        tabPanel("Tab 1", tableOutput("csvTable")),
        tabPanel("Tab 2", plotOutput("barPlot1")),
        tabPanel("Tab 3", plotOutput("barPlot2"))
      )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Output the CSV data as a table
  output$csvTable <- renderTable({
    data
  })
  
  # Plot the data for number of listings by city
  output$barPlot1 <- renderPlot({
    data_by_city_1 <- data %>%
      group_by(city) %>%
      summarise(listings = sum(listings, na.rm = TRUE)) %>%
      arrange(desc(listings)) # Arrange by listings in descending order
    
    # Reorder the city factor levels based on listings
    data_by_city_1$city <- factor(data_by_city_1$city, levels = data_by_city_1$city)
    
    # Create a bar chart for 'listings'
    ggplot(data_by_city_1, aes(x=city, y=listings)) +
      geom_bar(stat="identity", fill="deepskyblue4") +
      labs(x = "City", 
           y = "Number of Listings",
           title = "Bar Chart of Number of Listings by City",
           subtitle = "Comparison Across Cities",
           caption = "Data: ALY-6070 dataset") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, 
                                       vjust = 1,
                                       hjust = 1,
                                       size = 9)) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1))
  })
  
  # Plot the data for average median prices by city
  output$barPlot2 <- renderPlot({
    data_by_city_2 <- data %>%
      group_by(city) %>%
      summarise(median_price = mean(median, na.rm = TRUE))
    
    # Create a bar chart for 'median_price'
    ggplot(data_by_city_2, aes(x=reorder(city, -median_price), y=median_price)) +
      geom_bar(stat="identity", fill="deepskyblue4") +
      labs(x = "City", 
           y = "Average Median Price",
           title = "Bar Chart of Average Median Prices",
           subtitle = "Comparison Across Cities",
           caption = "Data: ALY-6070 dataset") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, 
                                       vjust = 1,
                                       hjust = 1,
                                       size = 9)) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
