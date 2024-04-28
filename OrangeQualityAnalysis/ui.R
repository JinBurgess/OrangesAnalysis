library(shiny)
library(readr)
library(plotly)
library(shinydashboard)
library(shinyWidgets)

shinyUI(
  navbarPage("Orange Quality Data", 
             tabPanel("Overview"), # tabPanel Overview
             tabPanel("DataVis",
                      dashboardPage(
                        dashboardHeader(title = "Exploratory Analysis"),
                        dashboardSidebar(
                          sidebarMenu(
                            menuItem("Distribution", tabName = "dataDist", icon = icon("dashboard")),
                            menuItem("RandomForest", tabName = "rfc", icon = icon("dashboard"))
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
                                                          value = NULL, min = 1, max = NA, step = 1
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
                            )# tabItem rfc
                            ) # tabitems
                          ) # dashboard body
                        ) # dashboard page
             ), # tab Panel 
             tabPanel("Statistics")
  ) # navbarPage
) # shinyUI