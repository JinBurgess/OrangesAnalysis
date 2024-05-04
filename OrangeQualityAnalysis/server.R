library(shiny)
library(readr)
library(plotly)
library(shinyWidgets)
library(DT)
library(forcats)
library(GGally)


shinyServer(function(input, output, session) {
  
  ######################################## Overview ############################################
  output$overview.content <- renderUI({
    overview.content
  })
  
  #################################### Render Table ############################################
  
  output$dataset <- renderDataTable({
    datatable(df)
  })

  ################################ Exploratory Analysis ########################################
  # distribution plot
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
  
  output$corrmatrix <- renderPlot({
    df_copy <- df %>%
      mutate(Blemish = if_else(str_detect(Blemish, 'Y'), 1, 0))%>%
      select(-c(Variety, Color))
    
    ggcorr(df_copy)
  })
  
  # Scatter plot 
  output$predictorScatter <- renderPlot({
      ggplot(df, aes(x = !!sym(input$scatterplot_x), y = !!sym(input$scatterplot_y)))+
        geom_point() + theme_minimal()
  })
  
  ################################ Supervised Learning ######################################## 
  
  rfcReactive <- eventReactive(c(input$rfcestimators, input$rfcsampleleaf, input$rfcmaxleafnodes, input$rfcmaxsamples, input$rfcmaxfeature),
                               {
    df_copy <- df %>%
      mutate(Blemish = if_else(str_detect(Blemish, 'Y'), 1, 0))


    n_estimators <- if (is.na(input$rfcestimators)) 1 else input$rfcestimators
    min_samples_leaf <- if (is.na(input$rfcsampleleaf)) 1 else input$rfcsampleleaf
    max_leaf_nodes <- if (is.na(input$rfcmaxleafnodes)) NULL else input$rfcmaxleafnodes
    max_samples <- if (is.na(input$rfcmaxsamples)) NULL else input$rfcmaxsamples
    max_feature <- if (is.na(input$rfcmaxfeature)) "None" else input$rfcmaxfeature

    # Create a random forest model
    if (max_feature == "None"){
      rfc_model <- randomForest(as.factor(Blemish) ~ . - Variety,
                                data = df_copy,
                                ntree = n_estimators,
                                mtry = ncol(df_copy), # related to max_feature
                                nodesize = min_samples_leaf,
                                maxnodes = max_leaf_nodes,
                                samplesize = max_samples,
                                replace = TRUE)
    } else if(max_feature == "sqrt"){
      rfc_model <- randomForest(as.factor(Blemish) ~ . - Variety,
                                data = df_copy,
                                ntree = n_estimators,
                                mtry = min(floor(sqrt(ncol(df_copy) - 1)), ncol(df_copy) - 1),#sqrt(ncol(df_copy)), # related to max_feature

                                nodesize = min_samples_leaf,
                                maxnodes = max_leaf_nodes,
                                samplesize = max_samples,
                                replace = TRUE)
    }else{
      rfc_model <- randomForest(as.factor(Blemish) ~ . - Variety,
                                data = df_copy,
                                ntree = n_estimators,
                                mtry = min(floor(log(ncol(df_copy) - 1)), ncol(df_copy) - 1), #log2(ncol(df_copy)), # related to max_feature
                                min_samples_leaf = min_samples_leaf,
                                max_leaf_nodes = max_leaf_nodes,
                                max_samples = max_samples)
    }

    return(rfc_model)
  }) # rfcReactive

  output$rfcOutput <- renderDataTable({
    req(rfcReactive())  # Ensure rfcReactive is not NULL

    model <- rfcReactive()  # Get the trained model

    # Get the structure of the first tree in the forest
    tree <- getTree(model, 1, labelVar=TRUE)

    # # # Convert the tree structure to a data frame
    tree_df <- as.data.frame(tree)

    # Return the data frame to be rendered in the UI
    datatable(tree_df)
  }) # rfcOutput

  # Server
  server <- function(input, output) {
    
    output$rfcPlot <- renderPlot({
      req(input$parent_node, input$max_depth)
      
      # Get the selected parent node
      parent_node <- input$parent_node
      
      # Get the selected maximum depth
      max_depth <- input$max_depth
      
      # Your random forest model function (rfcReactive) goes here
      model <- rfcReactive()  
      
      # Extract the decision tree from the model
      tree <- getTree(model, 1, labelVar = TRUE)
      
      # Convert the tree to a data frame
      tree_df <- as.data.frame(tree)
      
      # Filter the tree to get the subtree starting from the selected parent node
      sub_tree <- subset(tree_df, nodeID == parent_node | grepl(paste("^", parent_node, ".", sep = ""), nodeID))
      
      # Extract the left and right daughter nodes of the selected parent node
      left_node <- sub_tree$subtree.left[1]
      right_node <- sub_tree$subtree.right[1]
      
      # Filter the tree to get the subtree with left and right daughters
      sub_tree <- subset(tree_df, nodeID %in% c(parent_node, left_node, right_node))
      
      # Create an rpart object from the subtree (Note: you need to replace 'formula', 'data', 'subset', and 'weights' with appropriate values)
      rpart_tree <- rpart::rpart(formula = formula, data = data, subset = subset, weights = weights, ...)
      
      # Plot the decision tree with the selected maximum depth
      rpart.plot(rpart_tree, fallen.leaves = TRUE, branch.lty = 3, yesno = 2, maxdepth = max_depth)
    })
  }
  
  
  svmReactive <- eventReactive(list(input$kernel, input$cost_linear, input$cost_poly, input$poly_gamma, input$poly_degree, input$cost_rgb, input$gamma_rgb), {
    df_copy <- df %>%
      mutate(Blemish = if_else(str_detect(Blemish, 'Y'), 1, 0))

    line_cost <- if (is.na(input$cost_linear)) 0.1 else input$cost_linear
    poly_cost <- if (is.na(input$cost_poly)) 0.1 else input$cost_poly
    poly_gamma <- if (is.na(input$poly_gamma)) 0.1 else input$poly_gamma
    poly_degree <- if (is.na(input$poly_degree)) 2 else input$poly_degree
    rad_cost <- if (is.na(input$cost_rgb)) 0.1 else input$cost_rgb
    rad_gamma <- if (is.na(input$gamma_rgb)) 0.1 else input$gamma_rgb

    # Identify numeric columns
    numeric_cols <- sapply(df_copy, is.numeric)

    # Exclude non-numeric columns
    numeric_df <- df_copy[, numeric_cols]

    # Scale numeric columns
    scaled_numeric_df <- scale(numeric_df)

    # Replace scaled numeric columns in the original dataframe
    df_copy[, numeric_cols] <- scaled_numeric_df

    model <- switch(input$kernel,
                    "linear" = svm(Blemish ~ . - Variety - Color - Size, data = df_copy, kernel = "linear", cost = line_cost, scale = FALSE, max_iter = 5000),
                    "poly" = svm(Blemish ~ . - Variety - Color - Size, data = df_copy, kernel = "poly", cost = poly_cost, gamma = poly_gamma, degree = poly_degree, scale = FALSE, max_iter = 5000),
                    "radial" = svm(Blemish ~ . - Variety - Color - Size, data = df_copy, kernel = "radial", cost = rad_cost, gamma = rad_gamma, scale = FALSE, max_iter = 5000)
    )

    return(model)
  })  # svmReactive

  output$svmOutput <- renderPlot({
    df_copy <- df %>%
      mutate(Blemish = if_else(str_detect(Blemish, 'Y'), 1, 0))

    req(svmReactive())  # Ensure svmReactive is not NULL

    model <- svmReactive()  # Get the trained model

    plot(model)  # Plot the SVM model
  })  # svmOutput

})  # shinyServer




