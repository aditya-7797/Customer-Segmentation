# Load necessary libraries
library(shiny)
library(dplyr)
library(DT)
library(plotly)
library(stats)

# Load the customer data
customer_data <- read.csv("C:/DS NOTES/Mall_Customers.csv")

# Define the UI
ui <- fluidPage(
  titlePanel("Customer Data Analysis and Clustering"),
  sidebarLayout(
    sidebarPanel(
      selectInput("analysis", "Choose Analysis",
                  choices = c("Gender Visualization", "Age Distribution", "Spending Score Distribution", "K-means Clustering")),
      conditionalPanel(
        condition = "input.analysis == 'K-means Clustering'",
        numericInput("k_clusters", "Number of Clusters (K):", value = 3)
      ),
      plotOutput("visualization")
    ),
    mainPanel(
      DTOutput("cluster_table"),
      plotlyOutput("kmeans_clusters")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # K-means clustering
  kmeans_clusters <- reactive({
    if (input$analysis == "K-means Clustering") {
      # Perform K-means clustering with the specified number of clusters (K)
      kmeans_model <- kmeans(customer_data[, c("Age", "Spending.Score..1.100.")], centers = input$k_clusters)
      customer_data$Cluster <- as.factor(kmeans_model$cluster)
      
      # Create a scatter plot with color-coded clusters
      kmeans_plot <- plot_ly(customer_data, x = ~Age, y = ~Spending.Score..1.100., color = ~Cluster) %>%
        add_markers()
      
      # Return the plot and the clustering results
      list(kmeans_plot, customer_data)
    } else {
      # Return NULL if K-means clustering is not selected
      NULL
    }
  })
  
  # Output for K-means clustering
  output$kmeans_clusters <- renderPlotly({
    if (!is.null(kmeans_clusters())) {
      kmeans_plot <- kmeans_clusters()[[1]]
      kmeans_plot
    }
  })
  
  output$visualization <- renderPlot({
    if (input$analysis == "Gender Visualization") {
      a <- table(customer_data$Gender)
      barplot(a, main = "Gender Comparison",
              ylab = "Count",
              xlab = "Gender",
              col = rainbow(2),
              ylim = c(0,1200),
              legend = rownames(a))
    } else if (input$analysis == "Age Distribution") {
      hist(customer_data$Age,
           col = "blue",
           main = "Age Distribution",
           xlab = "Age",
           ylim = c(0,400),
           ylab = "Frequency",
           labels = TRUE)
    } else if (input$analysis == "Spending Score Distribution") {
      # Bar graph of spending score vs frequency
      spending_score_freq <- table(customer_data$Spending.Score..1.100.)
      barplot(spending_score_freq, main = "Spending Score Distribution",
              xlab = "Spending Score",
              ylab = "Frequency",
              col = "green")
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)
