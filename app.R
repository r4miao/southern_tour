# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)

# Load the results data frame
results <- readRDS("tour_cost_results.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Southern Tour Cost Estimator"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "num_adult",
        "Number of Adults (age > 12):",
        choices = 1:4,
        selected = 1
      ),
      selectInput(
        "num_children",
        "Number of Children (age ≤ 12):",
        choices = 0:3,
        selected = 0
      ),
      selectInput(
        "family_number",
        "Total Number of Families:",
        choices = 1:4,
        selected = 1
      ),
      actionButton("get_quote", "Get Quote")
    ),
    mainPanel(
      h3("Your Family's Tour Quote"),
      uiOutput("quote_output"),
      plotOutput("price_plot")  # Always show the plot
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Default plot when app loads
  output$price_plot <- renderPlot({
    ggplot(results, aes(x = family_size, y = family_person_price, 
                        color = factor(num_adult), shape = factor(num_children))) +
      geom_point(size = 3) +
      facet_wrap(~ family_number, ncol = 2, labeller = labeller(family_number = function(x) paste("Family Number =", x))) +
      scale_color_manual(values = c("1" = "blue", "2" = "red", "3" = "green", "4" = "purple"), name = "Number of Adults") +
      scale_shape_manual(values = c("0" = 16, "1" = 17, "2" = 15, "3" = 18), name = "Number of Children") +
      labs(
        title = "Per-Person Price vs. Family Size by Family Number",
        x = "Family Size",
        y = "Per-Person Price (USD)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14),
        strip.text = element_text(size = 12)
      ) +
      scale_x_continuous(breaks = 1:7)
  })
  
  observeEvent(input$get_quote, {
    # Filter results based on user input
    quote <- results %>%
      filter(
        num_adult == as.integer(input$num_adult),
        num_children == as.integer(input$num_children),
        family_number == as.integer(input$family_number)
      ) %>%
      select(family_number, family_total_price, family_total_cost, 
             family_person_cost, family_person_price, family_total_profit, family_size) %>%
      slice(1) # Take the first row
    
    # Update the quote output
    output$quote_output <- renderUI({
      if (nrow(quote) == 0) {
        HTML("<p>No matching quote found. Please check your inputs.</p>")
      } else {
        HTML(paste0(
          "<p style='font-size:18px; color:blue; font-weight:bold;'>Total price for family: $", sprintf("%.2f", quote$family_total_price), "</p>",
          "<p style='font-size:18px; color:blue; font-weight:bold;'>Average price per person: $", sprintf("%.2f", quote$family_person_price), "</p>",
          "<p>Total cost for family: $", sprintf("%.2f", quote$family_total_cost), "</p>",
          "<p>Average cost per person: $", sprintf("%.2f", quote$family_person_cost), "</p>",
          "<p>Profit from each family: $", sprintf("%.2f", quote$family_total_profit), "</p>",
          "<p>Profit from all families: $", sprintf("%.2f", quote$family_total_profit * quote$family_number), "</p>"
        ))
      }
    })
    
    # Update the plot and highlight the selected quote
    output$price_plot <- renderPlot({
      base_plot <- ggplot(results, aes(x = family_size, y = family_person_price, 
                                       color = factor(num_adult), shape = factor(num_children))) +
        geom_point(size = 3) +
        facet_wrap(~ family_number, ncol = 2, labeller = labeller(family_number = function(x) paste("Family Number =", x))) +
        scale_color_manual(values = c("1" = "blue", "2" = "red", "3" = "green", "4" = "purple"), name = "Number of Adults") +
        scale_shape_manual(values = c("0" = 16, "1" = 17, "2" = 15, "3" = 18), name = "Number of Children") +
        labs(
          title = "Per-Person Price vs. Family Size by Family Number",
          x = "Family Size",
          y = "Per-Person Price (USD)"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 14),
          strip.text = element_text(size = 12)
        ) +
        scale_x_continuous(breaks = 1:7)
      
      if (nrow(quote) > 0) {
        base_plot +
          geom_point(data = quote, aes(x = family_size, y = family_person_price), 
                     color = "black", shape = 8, size = 6, stroke = 2)
      } else {
        base_plot
      }
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
