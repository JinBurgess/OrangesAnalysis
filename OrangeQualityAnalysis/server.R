library(shiny)
library(readr)
library(plotly)
library(forcats)
library(shinyWidgets)

shinyServer(function(input, output, session) {
  observe({
    updateSelectInput(session, "distributionplot", label = "Select Variable:", choices = c(names(df)), selected = "Variety")
  })
  
  colplot <- eventReactive(c(input$distributionplot, input$groupBlemish), {
    df_copy <- df  # Create a copy of the dataframe
    
    if (input$groupBlemish == 'Group') {
      df_copy <- df_copy %>%
        mutate(Blemish = if_else(str_detect(Blemish, 'Y'), 'Y', 'N'))
    }
    
    df1 <- df_copy %>%
      count(!!sym(input$distributionplot)) %>%
      mutate(prec = round(100 * n / sum(n), 2)) %>%
      mutate_at(vars(input$distributionplot), as.factor) %>%
      mutate(purpose = fct_reorder(!!sym(input$distributionplot), prec))
    
    return(df1)
  }) # colplot
  
  
  output$predictorDist <- renderPlot({
    req(colplot())  # Ensure colplot is not NULL
    
    df1 <- colplot()  # Get the dataframe from colplot
    
    if (input$distributionplot %in% c("Size", "Weight", "Sweetness", "Acidity")) {
      # Convert the selected variable to numeric if it's categorical
      df1[[input$distributionplot]] <- as.numeric(as.character(df1[[input$distributionplot]]))
      
      # Create a histogram if Size, Weight, Sweetness, or Acidity is selected
      ggplot(df1, aes(x = !!sym(input$distributionplot))) +
        geom_histogram(bins = 20) +  # Adjust binwidth as needed
        labs(x = input$distributionplot, y = "Frequency") + theme_minimal()
    } else {
      # Create a column plot for other variables
      ggplot(df1, aes(x = !!sym(input$distributionplot), y = prec)) +
        geom_col() +
        labs(x = input$distributionplot, y = "Frequency") + + theme_minimal()
    }
  }) # predictorDist
  # predictorDist
}) # shinyUIServer