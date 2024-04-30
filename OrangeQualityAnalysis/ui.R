library(shiny)
library(readr)
library(plotly)
library(shinydashboard)
library(shinyWidgets)

shinyUI(
  navbarPage("Orange Quality Data", 
             tabPanel("Overview", 
                      htmlOutput("overview.content")), # tabPanel Overview
             tabPanel("DataVis",
                      dashboardPage(
                        dashboardHeader(title = "Exploratory Analysis"),
                        dashboardSidebar(
                          sidebarMenu(
                            menuItem("Distribution", tabName = "dataDist", icon = icon("bar-chart")),
                            menuItem("Supervised Learning", tabName = "supervised", icon = icon("dashboard"),
                            menuSubItem("Random Forest", tabName = "rfc"),
                            menuSubItem("Support Vector Machine", tabName = "svm"),
                            menuSubItem("Logistic Regression", tabName = "regress")
                            )
                          )
                      ),
                        dashboardBody(
                          tabItems(
                            tabItem(tabName = "dataDist", 
                                    fluidRow(
                                      column(width = 4,
                                             selectInput(
                                               inputId = "distributionplot",
                                               label = "Select Variable:",
                                               choices = colnames(df),
                                               selected = "Variety"
                                             ), # selectInput
                                             conditionalPanel("input.distributionplot =='Blemish'", 
                                                              selectInput("groupBlemish", "Group Choices", 
                                                                          choices=c("Group", "Separate"), 
                                                                          selected = "Separate", multiple=FALSE)
                                             ) # conditional
                                      ), # column inputs
                                      column(width = 8,
                                             plotOutput("predictorDist")
                                      ) # column plot
                                    ) # fluidRow
                            ), # tabItem dataDist
                            tabItem(tabName = "rfc", 
                                    fluidRow(
                                      column(width = 4,
                                             numericInput(inputId = "rfcestimators",
                                                          label = "Select n_estimators:",
                                                          value = 1, min = 1, max = NA, step = 1
                                                          ), # numericInput n_estimator
                                             numericInput(inputId = "rfcdepth",
                                                          label = "Select max depth:",
                                                          value = NULL, min = 1, max = NA, step = 1
                                                          ), # numericInput max_depth
                                             numericInput(inputId = "rfcsamplesplit",
                                                          label = "Select min samples split:",
                                                          value = NULL, min = 1, max = NA, step = 1
                                                          ), # numericInput min_samples_split
                                             numericInput(inputId = "rfcsampleleaf",
                                                          label = "Select min samples leaf:",
                                                          value = NULL, min = 1, max = NA, step = 1
                                                          ), # numericInput min_samples_leaf
                                             prettyRadioButtons(inputId = "rfcmaxfeature",
                                                                label = "Select max features:",
                                                                choices = c("sqrt", "log2", "None"),
                                                                selected = "None", inline = TRUE
                                                                ), # numericInput max_features
                                             numericInput(inputId = "rfcmaxleafnodes",
                                                          label = "Select max leaf node:",
                                                          value = NULL, min = 2, max = NA, step = 1
                                                          ), # numericInput max_leaf_node
                                             numericInput(inputId = "rfcmaxsamples",
                                                          label = "Select max samples:",
                                                          value = NULL, min = 1, max = NA, step = 1
                                                          ) # numericInput max_samples
                                             ), # column
                                      column(width = 8,
                                             dataTableOutput("rfcOutput")
                                      ) # column output
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
                                          plotOutput("svmOutput", height=550)
                                        ) # mainPanel
                                      ) # sidebarLayout
                                    ) # fluid row
                            ), # tabItem svm
                            tabItem("regress",
                                    fluidPage(  
                                        sidebarLayout(
                                          sidebarPanel(
                                          ),
                                          
                                          mainPanel(
                                            plotOutput("regressOutput", height=550)
                                          )
                                        )
                                    ))
                            ) # tabitems
                          ) # dashboard body
                        ) # dashboard page
             ), # tab Panel 
             tabPanel("Statistics")
  ) # navbarPage
) # shinyUI