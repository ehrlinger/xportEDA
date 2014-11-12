
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("Exploratory Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      includeText("fileInclude.txt"),
      fileInput('file1', 'Choose Data File (.xpt file)',
                accept=c('.xpt'))
#       , 'text/csv',
#                          'text/comma-separated-values,text/plain',
#                          '.csv'))
      ,

      # Only show this panel if the plot type is a histogram
      uiOutput("xvars")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Categorical",
                           plotOutput('categorical', width="100%",
                                      height=900)
                  ),
                  tabPanel("Continuous",
                           plotOutput('continuous', width="100%",
                                      height=900)
                  ),
                  tabPanel("data.frame",
                           verbatimTextOutput('contents')
                  )
      )
    ),
    fluid=FALSE
  )
))