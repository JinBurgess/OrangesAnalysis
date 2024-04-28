#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

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
                            menuItem("Distribution", tabName = "dataDist", icon = icon("dashboard"))
                            ) # sidebarMenu
                        ), # dashboardSidebar
                        dashboardBody(
                              tabItem(tabName = "dataDist", 
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput(
                                            inputId = "distributionplot",
                                            label = "Select Variable:",
                                            choices = colnames(df),
                                            selected = "Variety"
                                          ), # prettyRadioButtons
                                          conditionalPanel("input.distributionplot =='Blemish'", 
                                                           selectInput("groupBlemish", "Group Choices", 
                                                                       choices=c("Group", "Separate"), 
                                                                       selected = "Separate", multiple=FALSE)
                                          ), # conditionalPanel
                                        ), # sidebarPanel
                                        mainPanel(
                                          plotOutput("predictorDist")
                                                  ) #mainPanel
                                      ) # fluidPage
                                      ) # tabItem
                        ) # dashboardBody
                        ) # dashboardPage
                      ), # tabPanel
             tabPanel("Statistics")
  ) # navbarPage
) # shinyUI