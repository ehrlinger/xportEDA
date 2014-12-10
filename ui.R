
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
      fileInput('file1', 'Choose Data File:',
                accept=c('.xpt',
                         '.csv', 'rda'))
      ,

      # Only show this panel if the plot type is a histogram
      uiOutput("xvars")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("About",
                           includeMarkdown("./README.md")
                  ),
                  tabPanel("Categorical Panel",
                           plotOutput('categorical', width="100%",
                                      height=900)
                  ),
                  tabPanel("Continuous Panel",
                           plotOutput('continuous', width="100%",
                                      height=900)
                  ),
                  tabPanel("Single Variable",
                           uiOutput("selectPlot"),
                           hr(),
                           plotOutput("varPlot")
                  ),
                  tabPanel("data.frame Summary",
                           verbatimTextOutput('contents')
                  )
      )
    ),
    fluid=FALSE
  )
))