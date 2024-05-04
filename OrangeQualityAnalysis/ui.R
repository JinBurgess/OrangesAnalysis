library(shiny)
library(readr)
library(plotly)
library(shinydashboard)
library(shinyWidgets)

shinyUI(
  navbarPage("Orange Quality Data", 
             tabPanel("Overview", 
                      htmlOutput("overview.content")), # tabPanel Overview
             tabPanel("Dataset",
                      dataTableOutput("dataset")),
             tabPanel("Exploratory Analysis",
                      fluidRow(
                        box(width = 6,
                            status = "info", title = "Predictor Distribution", solidHeader = TRUE, collapsible = TRUE,
                            selectInput(inputId = "distributionplot", label = "Select Variable:",
                              choices = colnames(df), selected = "Variety"
                            ), # selectInput
                            conditionalPanel("input.distributionplot =='Blemish'",
                                             selectInput("groupBlemish", "Group Choices",
                                                         choices=c("Group", "Separate"),
                                                         selected = "Separate", multiple=FALSE)
                            ), # conditional
                            plotOutput("predictorDist")
                        ),
                        box(width = 6,
                            status = "info", title = "Smooth Distribution", solidHeader = TRUE, collapsible = TRUE,
                            selectInput(inputId = "smoothdist", label = "Select Variable:", choices = colnames(df), selected = "Size"
                            ), # selectInput
                            plotOutput("predictorSmooth")
                        ),
                        box(width = 6,
                            status = "info", title = "Correlation Matrix", solidHeader = TRUE, collapsible = TRUE,
                            plotOutput("corrmatrix")
                        ),
                        box(width = 6,
                            status = "info", title = "Predictor ScatterPlot", solidHeader = TRUE, collapsible = TRUE,
                            selectInput(inputId = "scatterplot_x", label = "Select Explanatory Variable:",
                              choices = colnames(df), selected = "Size"
                            ), # selectInput
                            selectInput(inputId = "scatterplot_y", label = "Select Response Variable:", 
                                        choices = colnames(df), selected = "Weight"
                            ),
                            plotOutput("predictorScatter")
                        ),
                        box(width = 4,
                            status = "warning", title = "Data Frame", solidHeader = TRUE, collapsible = TRUE,
                            footer = "Read Remotely from File",
                            tableOutput("mydata")
                        )
                      )),
             tabPanel("Supervised Machine Learning",
                      dashboardPage(
                        dashboardHeader(title = ""),
                        dashboardSidebar(
                          sidebarMenu(
                            menuItem("Random Forest", tabName = "rfc"),
                            menuItem("Support Vector Machine", tabName = "svm"),
                            menuItem("Logistic Regression", tabName = "regress")
                            )
                      ),
                        dashboardBody(
                          tabItems(
                            tabItem(tabName = "rfc",
                                    fluidRow(
                                      sidebarLayout(
                                        sidebarPanel(
                                             numericInput(inputId = "rfcestimators", label = "Select n_estimators:",
                                                          value = 1, min = 1, max = NA, step = 1
                                                          ), # numericInput n_estimator
                                             numericInput(inputId = "rfcsampleleaf", label = "Select min samples leaf:",
                                                          value = NULL, min = 1, max = NA, step = 1
                                                          ), # numericInput min_samples_leaf
                                             prettyRadioButtons(inputId = "rfcmaxfeature", label = "Select max features:",
                                                                choices = c("sqrt", "log2", "None"),
                                                                selected = "None", inline = TRUE
                                                                ), # numericInput max_features
                                             numericInput(inputId = "rfcmaxleafnodes", label = "Select max leaf node:",
                                                          value = NULL, min = 2, max = NA, step = 1
                                                          ), # numericInput max_leaf_node
                                             numericInput(inputId = "rfcmaxsamples",
                                                          label = "Select max samples:",
                                                          value = NULL, min = 1, max = NA, step = 1
                                                          ) # numericInput max_samples
                                             ), # sidebarPanel
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("Random Forest Output",
                                                   box(width = 12, dataTableOutput("rfcOutput"))
                                          ),
                                          tabPanel("Random Forest Tree",
                                                   numericInput("parent_node", "Select Parent Node", min = 1, max = NA, value = 1),
                                                   numericInput("max_depth", "Maximum Depth:", min = 1, max = 10, value = 3),
                                                   box(width = 12, renderPlot("rfcPlot", height = 500)))
                                        ) #tabsetPanel
                                      ) # mainPanel
                                      ) # sidebarPanel
                            ) # fluid row
                            ),# tabItem rfc
                            tabItem(tabName = "svm",
                                    fluidRow(
                                      sidebarLayout(
                                        sidebarPanel(
                                          prettyRadioButtons("kernel", "Select Kernal Type:",
                                                             choices=list("Linear"="linear", "Polynomial"="poly", "Gaussian"="rgb")
                                          ),
                                          conditionalPanel("input.kernel =='linear'",
                                                           numericInput(inputId = "cost_linear", label = "Select Cost:", value = NULL, min = 0.01, max = NA)
                                          ),
                                          conditionalPanel("input.kernel =='poly'",
                                                           numericInput(inputId = "poly_degree",
                                                                        label = "Select Degree:", value = NULL, min = 2, max = NA
                                                                        ), # numericInput degree
                                                           numericInput(inputId = "cost_poly",
                                                                        label = "Select Cost:",value = NULL, min = 0.01, max = NA
                                                                        ), # numericInput cost_poly
                                                           numericInput(inputId = "poly_gamma",
                                                                        label = "Select Gamma:", value = NULL, min = 0.01, max = NA
                                                                        ) # numericInput gamma),
                                          ),
                                          conditionalPanel("input.kernel =='rgb'",
                                                           numericInput(inputId = "cost_rgb",
                                                                        label = "Select Cost:", value = NULL, min = 0.01, max = NA
                                                                        ), # numericInput cost_rbf
                                                           numericInput(inputId = "gamma_rgb",
                                                                        label = "Select Gamma:", value = NULL, min = 0.01, max = NA
                                                                        ) # numericInput gamma_rbf
                                          )
                                        ), # sidebarPanel
                                        mainPanel(
                                          tabsetPanel(
                                            tabPanel("SVM Output",
                                                     box(width = 12, plotOutput("svmOutput", height=550))
                                                     ),
                                            tabPanel("Happiness World Satisfication", 
                                                     box(width = 12, htmlOutput("Happiness.World.Satisfaction"))
                                                     ),
                                            tabPanel(" Analyze and Predict GDP",
                                                     box(width = 12, htmlOutput("Analyze.and.Predict"))
                                                     )
                                            )
                                        ) # mainPanel
                                      ) # sidebarLayout
                                    ) # fluid row
                            ), # tabItem svm
                            tabItem("regress",
                                    fluidPage(
                                        sidebarLayout(
                                          sidebarPanel(
                                          ), #sidebarPanel

                                          mainPanel(
                                            plotOutput("regressOutput", height=550)
                                          ) # mainPanel regress
                                        ) # sidebarPanel
                                    ) # fluidPage
                                    ) # tabItem
                            ) # tabItems
                          ) # dashboard body
                        ) # dashboard page
             ) # tab Panel
  ) # navbarPage
) # shinyUI