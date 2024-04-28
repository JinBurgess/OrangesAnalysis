library(shiny)
library(readr)
library(plotly)
library(shinyWidgets)
library(DT)

shinyServer(function(input, output, session) {
  # 
  # observe({
  #   updateSelectInput(session, "distributionplot", label = "Select Variable:", choices = c(names(df)), selected = "Variety")
  # }) # observe
  
  colReactive <- eventReactive(c(input$distributionplot, input$groupBlemish, df), {
    df_copy <- df  
    
    if (!(input$distributionplot %in% c("Size", "Weight", "Sweetness", "Acidity"))) {
      if (input$groupBlemish == 'Group') {
        df_copy <- df_copy %>%
          mutate(Blemish = if_else(str_detect(Blemish, 'Y'), 'Y', 'N'))
      }
      df_copy <- df_copy %>%
        count(!!sym(input$distributionplot)) %>%
        mutate(prec = round(100 * n / sum(n), 2)) %>%
        mutate_at(vars(input$distributionplot), as.factor) %>%
        mutate(purpose = fct_reorder(!!sym(input$distributionplot), prec))
    }
    
    return(df_copy)
  }) # colplot
  
  output$predictorDist <- renderPlot({
    req(colReactive())  # Ensure colplot is not NULL
    
    df1 <- colReactive()  # Get the dataframe from colplot
    
    if (input$distributionplot %in% c("Size", "Weight", "Sweetness", "Acidity")) {
      # Create a histogram if Size, Weight, Sweetness, or Acidity is selected
      ggplot(df1, aes(x = !!sym(input$distributionplot))) +
        geom_histogram(bins = 20) +  # Adjust binwidth as needed
        labs(x = input$distributionplot, y = "Frequency") + theme_minimal()
    } else {
      # Create a column plot for other variables
      ggplot(df1, aes(x = !!sym(input$distributionplot), y = prec)) +
        geom_col() +
        labs(x = input$distributionplot, y = "Frequency") + theme_minimal()
    }
  }) # predictorDist 
  
  rfcReactive <- eventReactive(c(input$rfcestimators, input$rfcdepth, input$rfcsamplesplit, input$rfcsampleleaf, input$rfcmaxleafnodes, input$rfcmaxsamples, input$rfcmaxfeature), {
    df_copy <- df  
    # Extract parameters from input
    n_estimators <- ifelse(is.null(input$rfcestimators), 1, input$rfcestimators)
    max_depth <- ifelse(is.null(input$rfcdepth), NULL, input$rfcdepth)
    min_samples_split <- ifelse(is.null(input$rfcsamplesplit), 2, input$rfcsamplesplit)
    min_samples_leaf <- ifelse(is.null(input$rfcsampleleaf), 1, input$rfcsampleleaf)
    max_leaf_nodes <- ifelse(is.null(input$rfcmaxleafnodes), NULL, input$rfcmaxleafnodes)
    max_samples <- ifelse(is.null(input$rfcmaxsamples), NULL, input$rfcmaxsamples)
    max_feature <- ifelse(is.null(input$rfcmaxfeature), "None", input$rfcmaxfeature)
    
    # Create a random forest model
    if (max_feature == "None"){
      rfc_model <- randomForest(Sweetness ~ . - Variety, 
                                data = df_copy, 
                                ntree = n_estimators,
                                mtry = ncol(df_copy),
                                max_depth = max_depth,
                                min_samples_split = min_samples_split,
                                min_samples_leaf = min_samples_leaf,
                                max_leaf_nodes = max_leaf_nodes,
                                max_samples = max_samples)
    } else if(max_feature == "sqrt"){
      rfc_model <- randomForest(Sweetness ~ . - Variety, 
                                data = df_copy, 
                                ntree = n_estimators,
                                mtry = sqrt(ncol(df_copy)),
                                max_depth = max_depth,
                                min_samples_split = min_samples_split,
                                min_samples_leaf = min_samples_leaf,
                                max_leaf_nodes = max_leaf_nodes,
                                max_samples = max_samples)
    }else{
      rfc_model <- randomForest(Sweetness ~ . - Variety, 
                                data = df_copy, 
                                ntree = n_estimators,
                                mtry = log2(ncol(df_copy)),
                                max_depth = max_depth,
                                min_samples_split = min_samples_split,
                                min_samples_leaf = min_samples_leaf,
                                max_leaf_nodes = max_leaf_nodes,
                                max_samples = max_samples)
    }
    
    return(rfc_model)
  })
  
  output$rfcOutput <- renderDataTable({
    req(rfcReactive())  # Ensure rfcReactive is not NULL
    
    model <- rfcReactive()  # Get the trained model
    
    # Get the structure of the first tree in the forest
    tree <- getTree(model, 1, labelVar=TRUE)
    
    # # # Convert the tree structure to a data frame
    tree_df <- as.data.frame(tree)

    # Return the data frame to be rendered in the UI
    datatable(tree_df)
  })
  
})  # shinyServer




