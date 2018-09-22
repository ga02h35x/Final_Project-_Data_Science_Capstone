#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predictive of Next Word in the sentence"),
  h5("By Juan Luis García Dus"),
  br(),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(

    sidebarPanel(
      
      textInput("sentence",
                "Input your sentence",
                value="")
    ),

    mainPanel(
      h1(textOutput("word1")),
      br(),
      h2("Why this?"),
      uiOutput("list_predictions")

    )

  )
))
