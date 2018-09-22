#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

source("** - functions.R")



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  temp <- reactive({
    a <- next_word(input$sentence);
    names(a) <- c("Predicted Word", "Certainty");
    a
    })
  
  output$word1<- renderText({
    pr <- temp();
    a <- pr[["Predicted Word"]][1];
    paste("Out predicted word: ", a)
  })
  
  output$list_predictions <-renderTable({
    temp();
    })
  
  #a<-1;
  #
  
  #output$word1 <- renderText({})
  # https://stackoverflow.com/questions/22923784/how-to-add-bullet-points-in-r-shinys-rendertext
  #output$list_predictions <- renderUI(HTML(paste("<ul><li>", a, "</li><li>", a, "</li><li>", a, "</li></ul>")))

})
