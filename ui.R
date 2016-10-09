# Load required libraries
library(shiny)
source(file="predictNextWord.R")

# User interface texts
appTitle <- "Next Word Predictor"
appDesc  <- "A Natural Language Processing R&D project"
appOwner <- "Developed by Herbert Barrientos"

callToAction_0 <- "Given an input word or phrase, this application will predict the next word that would follow a coherent line of thought. The purpose is to facilitate and expedite the user's text typing experience, especially on mobile devices."
callToAction_1 <- "To use this application, simply type your input on the field below, click on the Go! button, and a list of up to five 'next words,' intended to complement your text, will be presented."
callToAction_2 <- "If you don't like the proposed predicted words, click on the Go! button as many times as you want, in order to see more predicted word options."
callToAction_3 <- "NOTES:" 
callToAction_4 <- " - Please do not use numbers in your text."
callToAction_5 <- " - Currently this application is configured to run on a low-resource server. Your patience with the response time is appreciated."
  
# Object id
outputObjectId <- "predictedWords"

shinyUI(
  
  fluidPage(
    
    # Provide the title for the app, and the "app owner" name
    titlePanel(appTitle),
    h4(appDesc),
    h4(appOwner),
  
    sidebarLayout(
      
      sidebarPanel(
        
        width = 6,
        div(callToAction_0),
        br(),
        div(callToAction_1),
        br(),
        div(callToAction_2),
        br(),
        div(callToAction_3),
        div(callToAction_4),
        div(callToAction_5),
        br(),
        
        textInput("nGram", "Word or Phrase:", ""), 
        actionButton("goButton", "Go!"),
        
        HTML(EMPTY_STRING)
        
      ),  # END sidebarPanel
      
      # Call the output function
      mainPanel(uiOutput(outputObjectId))
      
    )  # END sidebarLayout
    
  )  # END fluidPage
  
)  # END shinyUI