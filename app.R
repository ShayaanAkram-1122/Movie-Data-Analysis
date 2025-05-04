library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(readxl)
library(shinyjs)
library(scales)

# Load the IMDb dataset
library(here)
imdb_data <- read_excel(here("~/Documents/movies-data-analyzer-main/dataSet/cleaned_imdb_dataset.xlsx"))



# Ensure proper data types
imdb_data$Released_Year <- as.numeric(imdb_data$Released_Year)
imdb_data$IMDB_Rating <- as.numeric(imdb_data$IMDB_Rating)
imdb_data$No_of_Votes <- as.numeric(imdb_data$No_of_Votes)
imdb_data$Gross <- as.numeric(gsub(",", "", imdb_data$Gross, fixed = TRUE)) # Convert Gross to numeric

# Define UI
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "pulse"),
  useShinyjs(),
  
  
  titlePanel("MOVIES DATA ANALYZER"),
  
  # Add the Show/Hide Filters button
  actionButton("toggle_filters", "Show/Hide Filters", icon = icon("toggle-on")),
  
  # Use CSS to handle responsiveness
  tags$style(HTML("
    #filters_panel {
      transition: all 0.5s ease;
      width: 80%; /* Default width */
    }
    #filters_panel.hidden {
      width: 0;
      padding: 0;
      margin: 0;
      opacity: 0;
    }
    .main-content {
      transition: margin-left 0.5s ease;
    }
    .main-content.shifted {
      margin-left: 10%; /* Space to accommodate the sidebar */
    }
  ")),
  
  sidebarLayout(
    sidebarPanel(
      id = "filters_panel",
      h4("Filters"),
      selectInput("genre_filter", "Select Genre:", 
                  choices = c("All", unique(imdb_data$Genre))),
      sliderInput("year_filter", "Select Year Range:",
                  min = min(imdb_data$Released_Year, na.rm = TRUE),
                  max = max(imdb_data$Released_Year, na.rm = TRUE),
                  value = c(min(imdb_data$Released_Year, na.rm = TRUE), max(imdb_data$Released_Year, na.rm = TRUE))),
      actionButton("apply_filter", "Apply Filters", icon = icon("filter")),  # Added filter icon here
      br(),
      p(style = "color: red;", "Note: Some graphs may not be properly displayed due to filtered data. Apply filters to get correct results.") # Added note here
    ),
    
    mainPanel(
      id = "main_content",
      tabsetPanel(
        tabPanel("Overview", 
                 h4("Dataset Summary"),
                 dataTableOutput("summary_table")
        ),
        tabPanel("Graphs", 
                 h4("Graphical Representation"),
                 fluidRow(
                   column(6, 
                          plotOutput("rating_distribution"), 
                          p("The probability distribution shows the frequency of IMDb ratings across all movies. The red line represents the density estimate.")
                   )
                 ),
                 fluidRow(
                   column(6, 
                          plotOutput("votes_distribution"), 
                          p("The probability distribution shows the frequency of number of votes across all movies. The red line represents the density estimate.")
                   ),
                   column(6, 
                          plotOutput("gross_distribution"), 
                          p("The probability distribution shows the frequency of gross earnings across all movies. The red line represents the density estimate.")
                   )
                 )
        ),
        
        tabPanel("Boxplots", 
                 h4("Boxplots of Key Variables"),
                 fluidRow(
                   column(6, 
                          plotOutput("rating_boxplot"), 
                          p("This boxplot shows the distribution of IMDb Ratings across genres.")
                   ),
                   column(6, 
                          plotOutput("votes_boxplot"), 
                          p("This boxplot shows the distribution of Number of Votes across genres.")
                   )
                 ),
                 fluidRow(
                   column(6, 
                          plotOutput("gross_boxplot"), 
                          p("This boxplot shows the distribution of Gross Earnings across genres.")
                   )
                 )
        ),
        tabPanel("Statistics", 
                 h4("Descriptive Statistics"),
                 verbatimTextOutput("desc_stats_rating"),
                 plotOutput("desc_stats_rating_plot"),
                 h4("Descriptive Statistics for Number of Votes"),
                 verbatimTextOutput("desc_stats_votes"),
                 plotOutput("desc_stats_votes_plot"),
                 h4("Descriptive Statistics for Gross Earnings"),
                 verbatimTextOutput("desc_stats_gross"),
                 plotOutput("desc_stats_gross_plot")
        ),
        tabPanel("Modeling", 
                 h4("Regression Model"),
                 verbatimTextOutput("regression_output"),
                 p("This section shows a regression model predicting IMDb ratings based on the number of votes a movie received."),
                 h4("Confidence Intervals"),
                 verbatimTextOutput("confidence_intervals"),
                 p("Confidence intervals provide a range of plausible values for the regression coefficients."),
                 h4("Regression Plots"),
                 plotOutput("regression_plot"),
                 plotOutput("residual_plot")
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive value to track the visibility state of the sidebar
  sidebar_visible <- reactiveVal(TRUE)
  
  observeEvent(input$toggle_filters, {
    # Toggle the visibility state
    sidebar_visible(!sidebar_visible())
    
    # Toggle the visibility of the sidebarPanel
    toggle("filters_panel")
    
    # Adjust the layout based on the visibility state
    if (sidebar_visible()) {
      addClass("main_content", "shifted")  # Sidebar is visible, shift content
    } else {
      removeClass("main_content", "shifted")  # Sidebar is hidden, reset content layout
    }
  })
  
  
  # Reactive Data Filtering with Outlier Removal
  filtered_data <- reactive({
    data <- imdb_data
    if (input$genre_filter != "All") {
      data <- data[data$Genre == input$genre_filter,]
    }
    data <- data[data$Released_Year >= input$year_filter[1] & data$Released_Year <= input$year_filter[2],]
    # Remove outliers based on the 99th percentile
    data <- data %>%
      filter(No_of_Votes < quantile(No_of_Votes, 0.99, na.rm = TRUE)) %>%
      filter(Gross < quantile(Gross, 0.99, na.rm = TRUE))
    data
  })
  
  # Summary Table
  output$summary_table <- renderDataTable({
    datatable(filtered_data())
  })
  
  # Probability Distribution of Number of Votes
  output$votes_distribution <- renderPlot({
    data <- filtered_data()
    if (nrow(data) > 0) {
      hist(data$No_of_Votes, probability = TRUE, main = "Probability Distribution of Number of Votes",
           xlab = "Number of Votes", col = "lightgreen", border = "white")
      lines(density(data$No_of_Votes, na.rm = TRUE), col = "red", lwd = 2)
    } else {
      plot.new()
      text(0.5, 0.5, "No data available for the selected filters.", cex = 1.5)
    }
  })
  
  # Probability Distribution of Gross Earnings
  output$gross_distribution <- renderPlot({
    data <- filtered_data()
    if (nrow(data) > 0) {
      hist(data$Gross, probability = TRUE, main = "Probability Distribution of Gross Earnings",
           xlab = "Gross Earnings", col = "orange", border = "white")
      lines(density(data$Gross, na.rm = TRUE), col = "red", lwd = 2)
    } else {
      plot.new()
      text(0.5, 0.5, "No data available for the selected filters.", cex = 1.5)
    }
  })
  
  
  # Probability Distribution
  output$rating_distribution <- renderPlot({
    data <- filtered_data()
    if (nrow(data) > 0) {
      hist(data$IMDB_Rating, probability = TRUE, main = "Probability Distribution of Ratings",
           xlab = "IMDB Rating", col = "skyblue", border = "white")
      lines(density(data$IMDB_Rating, na.rm = TRUE), col = "red", lwd = 2)
    } else {
      plot.new()
      text(0.5, 0.5, "No data available for the selected filters.", cex = 1.5)
    }
  })
  
  # Descriptive Statistics for IMDB Rating
  output$desc_stats_rating <- renderPrint({
    data <- filtered_data()
    if (nrow(data) > 0) {
      summary(data$IMDB_Rating)
    } else {
      "No data available for the selected filters."
    }
  })
  
  output$desc_stats_rating_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) > 0) {
      ggplot(data, aes(x = IMDB_Rating)) +
        geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
        labs(title = "Distribution of IMDB Ratings", x = "IMDB Rating", y = "Frequency") +
        theme_minimal()
    } else {
      plot.new()
      text(0.5, 0.5, "No data available for the selected filters.", cex = 1.5)
    }
  })
  
  # Descriptive Statistics for Number of Votes
  output$desc_stats_votes <- renderPrint({
    data <- filtered_data()
    if (nrow(data) > 0) {
      summary(data$No_of_Votes)
    } else {
      "No data available for the selected filters."
    }
  })
  
  output$desc_stats_votes_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) > 0) {
      ggplot(data, aes(x = No_of_Votes)) +
        geom_histogram(binwidth = max(1, diff(range(data$No_of_Votes, na.rm = TRUE)) / 30), 
                       fill = "lightgreen", color = "black", alpha = 0.7) +
        labs(title = "Distribution of Number of Votes", x = "Number of Votes", y = "Frequency") +
        theme_minimal()
    } else {
      plot.new()
      text(0.5, 0.5, "No data available for the selected filters.", cex = 1.5)
    }
  })
  
  # Descriptive Statistics for Gross Earnings
  output$desc_stats_gross <- renderPrint({
    data <- filtered_data()
    if (nrow(data) > 0) {
      summary(data$Gross)
    } else {
      "No data available for the selected filters."
    }
  })
  
  output$desc_stats_gross_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) > 0) {
      ggplot(data, aes(x = Gross)) +
        geom_histogram(binwidth = max(1, diff(range(data$Gross, na.rm = TRUE)) / 30), 
                       fill = "orange", color = "black", alpha = 0.7) +
        labs(title = "Distribution of Gross Earnings", x = "Gross Earnings (in billions)", y = "Frequency") +
        theme_minimal()
    } else {
      plot.new()
      text(0.5, 0.5, "No data available for the selected filters.", cex = 1.5)
    }
  })
  
  
  # Boxplot for IMDb Rating
  output$rating_boxplot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) > 0) {
      ggplot(data, aes(x = Genre, y = IMDB_Rating)) +
        geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
        labs(title = "IMDb Rating by Genre", x = "Genre", y = "IMDb Rating") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else {
      plot.new()
      text(0.5, 0.5, "No data available for the selected filters.", cex = 1.5)
    }
  })
  
  # Boxplot for Number of Votes
  output$votes_boxplot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) > 0) {
      ggplot(data, aes(x = Genre, y = No_of_Votes)) +
        geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
        labs(title = "Number of Votes by Genre", x = "Genre", y = "Number of Votes") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else {
      plot.new()
      text(0.5, 0.5, "No data available for the selected filters.", cex = 1.5)
    }
  })
  
  # Boxplot for Gross Earnings
  output$gross_boxplot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) > 0) {
      ggplot(data, aes(x = Genre, y = Gross)) +
        geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
        scale_y_log10(labels = label_comma()) +  # Format y-axis with commas
        labs(title = "Gross Earnings by Genre", x = "Genre", y = "Gross Earnings (log scale)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else {
      plot.new()
      text(0.5, 0.5, "No data available for the selected filters.", cex = 1.5)
    }
  })
  
  # Regression Model: IMDB Rating vs. Number of Votes
  output$regression_output <- renderPrint({
    data <- filtered_data()
    if (nrow(data) > 0) {
      model <- lm(IMDB_Rating ~ No_of_Votes, data = data)
      summary(model)
    }
  })
  
  # Regression Plot: IMDB Rating vs. Number of Votes
  output$regression_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) > 0) {
      model <- lm(IMDB_Rating ~ No_of_Votes, data = data)
      ggplot(data, aes(x = No_of_Votes, y = IMDB_Rating)) +
        geom_point(alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        labs(title = "Regression: IMDB Rating vs. Number of Votes", x = "Number of Votes", y = "IMDB Rating") +
        theme_minimal()
    } else {
      plot.new()
      text(0.5, 0.5, "No data available for the selected filters.", cex = 1.5)
    }
  })
  
  # Residual Plot
  output$residual_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) > 0) {
      model <- lm(IMDB_Rating ~ No_of_Votes, data = data)
      residuals <- resid(model)
      ggplot(data, aes(x = No_of_Votes, y = residuals)) +
        geom_point(alpha = 0.7) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Residual Plot: IMDB Rating vs. Number of Votes", x = "Number of Votes", y = "Residuals") +
        theme_minimal()
    } else {
      plot.new()
      text(0.5, 0.5, "No data available for the selected filters.", cex = 1.5)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
