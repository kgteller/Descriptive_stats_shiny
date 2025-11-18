library(shiny)
library(ggplot2)
library(bslib) # Optional, for a modern theme

# Define the UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "cerulean"), # Use the cerulean theme
  titlePanel("Descriptive Statistics and Histogram App"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Enter Your Data"),
      textAreaInput("user_data", "Enter data (comma or space separated):",
                    value = "1, 2, 3, 4, 5, 6, 7, 8, 9, 10"),
      
      h4("2. Configure Display Options"),
      radioButtons("type", "Type of Statistics:",
                   choices = list("Sample" = "sample", "Population" = "population"),
                   selected = "sample"),
      
      numericInput("decimals", "Number of Decimal Places:",
                   value = 2, min = 0, max = 10, step = 1),
      
      h4("3. Data Visualization"),
      sliderInput("bins", "Number of Histogram Bins:",
                  min = 1, max = 50, value = 20)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Statistics",
                 h3("Descriptive Statistics"),
                 tableOutput("stats_table")),
        tabPanel("Histogram",
                 plotOutput("data_hist"))
      )
    )
  )
)

# Define the Server logic
server <- function(input, output) {
  
  # Reactive function to parse the user input data
  parsed_data <- reactive({
    # Split the input string by commas or spaces
    data_str <- unlist(strsplit(input$user_data, "[,\\s]+"))
    # Convert to numeric, handle potential non-numeric entries
    data_numeric <- as.numeric(data_str)
    # Remove NAs and return the vector
    data_numeric[!is.na(data_numeric)]
  })
  
  # Generate the statistics table
  output$stats_table <- renderTable({
    data <- parsed_data()
    if (length(data) < 1) return(NULL)
    
    # Calculate statistics based on user choices
    n <- length(data)
    mean_val <- mean(data)
    median_val <- median(data)
    min_val <- min(data)
    Q1<- quantile(data,.25)
    Q3<- quantile(data,.75)
    max_val <- max(data)
    sd_val <- if (input$type == "sample") sd(data) else sqrt(var(data) * (n - 1) / n)
    var_val <- if (input$type == "sample") var(data) else var(data) * (n - 1) / n
    
    # Create a data frame for display
    stats <- data.frame(
      Statistic = c("N (Count)", "Mean", "Median", "Minimum","Q1","Q3", "Maximum",
                    "Variance", "Std. Deviation"),
      Value = c(n, mean_val, median_val, min_val,Q1,Q3, max_val, var_val, sd_val)
    )
    
    # Apply decimal formatting
    stats$Value <- round(as.numeric(stats$Value), input$decimals)
    
    stats
  })
  
  # Generate the histogram plot
  output$data_hist <- renderPlot({
    data <- parsed_data()
    if (length(data) < 1) return(NULL)
    
    ggplot(data.frame(x = data), aes(x = x)) +
      geom_histogram(bins = input$bins, fill = "dodgerblue", color = "white") +
      theme_minimal() +
      labs(title = "Histogram of User Data", x = "Data Value", y = "Frequency")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
