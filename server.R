# Load the required libraries
library(shiny)
source(file="predictNextWord.R")

OUTPUT_HTML_01 <- "<div>&nbsp;&nbsp;&nbsp;&nbsp;<strong>Next words:</strong><br>&nbsp;&nbsp;&nbsp;&nbsp;"
OUTPUT_HTML_02 <- "</div>"
OUTPUT_HTML_03 <- "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"

shinyServer(
  
  function(input, output) 
  {
    predictNextWords <- eventReactive(input$goButton, {
      
      pred <- predictNextWord(input$nGram)
      resp <- pred
      
      if ((length(pred) > 1) && (pred[1] != BAD_INPUT)) {

        resp <- EMPTY_STRING
        pred <- names(pred)
        
        for(i in 1:length(pred)) {
          
          if (!is.na(pred[i]))
            resp <- paste0(resp, pred[i], OUTPUT_HTML_03)
          
        }  # END for
                
      }  # END if
      
      words <- paste0(OUTPUT_HTML_01, resp, OUTPUT_HTML_02)
      return(words)
      
    })
    
    output$predictedWords <- renderText( {predictNextWords()} )
    
  }  # END funtion
  
)  # END shinyServer
