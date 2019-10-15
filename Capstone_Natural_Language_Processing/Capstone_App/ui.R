#ui.R (for use interface) controls how the app looks like

library(shiny)


#Shinny layout guide: https://shiny.rstudio.com/articles/layout-guide.html
fluidPage(
  
  # Title
  titlePanel("Prediction of next word"),
  
  textOutput('text1'),
  tags$head(tags$style("#text1{color: grey;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
  )
  ),
  
  textInput(inputId="caption", label="Enter your sentence:"),
  tags$h3("Words in the sentence to search:"),

  verbatimTextOutput("entered_words_last"),
  
  
         
         verbatimTextOutput("next_word_message"),
    
         tags$h3("Predicted result(s)"),
         
         column(width=3, 
         verbatimTextOutput("predicted_next_word_1")
         ),
         
         column(width=3, 
         verbatimTextOutput("predicted_next_word_2")
         ),
         
         
         column(width=3,
         verbatimTextOutput("predicted_next_word_3")
         ),
         
         column(width=3,
         verbatimTextOutput("predicted_next_word_4")
         )
         
         
  
         
         
  
  
  
)