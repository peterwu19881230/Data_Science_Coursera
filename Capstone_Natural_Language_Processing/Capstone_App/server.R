#server.R controls what the app does


#Make tetragram for use
'
load("words.RData")
load("ngrams.RData")


##(n==4 added. The previous function does not have it) function to filter sentences that have equal or less than n words
filter_sentence_and_sort=function(sentence_vector,n=1){
  if(n==1){
    indices=grep("\\S+",sentence_vector)
  }else if(n==2){
    indices=grep("\\S+\\s+\\S+",sentence_vector)
  }else if(n==3){
    indices=grep("\\S+\\s+\\S+\\s+\\S+",sentence_vector)
  }else if(n==4){
    indices=grep("\\S+\\s+\\S+\\s+\\S+\\s+\\S+",sentence_vector)
  }else{print("With this n the process is not written")
    return(0)
  }
  
  return(sentence_vector[indices][order(nchar(sentence_vector[indices]))])
}


four_gram=ngram(filter_sentence_and_sort(words,n=4)[1:430000],n=4)
save(one_gram,two_gram,three_gram,four_gram,file="ngrams.RData")

load("ngram_tabs.RData")
four_gram_tab=get.phrasetable(four_gram)
ngram_tab=list(one_gram_tab,two_gram_tab,three_gram_tab,four_gram_tab)
save(ngram_tab,file="ngram_tabs.RData")
'

library(shiny)
library(stringr)

load("ngram_tabs.RData")
server=function(input, output) {
  
  ##function to remove the space at the begining and the end of the entry
  #========================================================================================================================
  clean_text_entry=function(text_entry){
    
    cleaned_input=sub(pattern="^\\s",replacement = "",x=text_entry)
    cleaned_input=sub(pattern="\\s*$",replacement = "",x=cleaned_input) # /* stands for: 0 or more previous repeated pattern
    cleaned_input=tolower(cleaned_input) #make sure the text to search are all lowercase (in the ngram they are all lower case)
  }
  #========================================================================================================================
  
  
  next_word=reactive({ 
    
    ##remove the space at the begining and at the end of the entry
    cleaned_input=clean_text_entry(input$caption)
    result=NULL
    freq=NULL
    word_to_search=""
    msg=''
 
    if(cleaned_input==""){
      msg='~No words entered~'
      indices=integer(0)
      
    }else{
      
      if(identical(grep(cleaned_input,pattern = "\\s"),integer(0))){
        word_to_search=cleaned_input
        ngram_tab_to_use=ngram_tab[[2]] #two_gram_tab
        n=2
        
      }else if(identical(grep(cleaned_input,pattern = "\\s\\S+\\s"),integer(0))){
        word_to_search=str_extract(cleaned_input,pattern="\\S+\\s\\S+$") #regex depends on how many end words should be used
        ngram_tab_to_use=ngram_tab[[3]] #three_gram_tab
        n=3
        
      }else{
        word_to_search=str_extract(cleaned_input,pattern="\\S+\\s\\S+\\s\\S+$") #regex depends on how many end words should be used
        ngram_tab_to_use=ngram_tab[[4]] #four_gram_tab
        n=4
      }
      
      indices=grep(x=ngram_tab_to_use$ngrams,pattern=paste("^",word_to_search,sep="")) 
      #(good or bad I am not sure): user can use regex to search based on the above line
      
      result=str_extract(string=ngram_tab_to_use$ngrams[indices][1:4],pattern="\\S+\\s$")
      
      #If there is not enought result, search for lower gram library until one_gram
      #This is clever recursion I thought of by myself
      i=1
      repeat{
        
        if(n==1) break #if one_gram has already been reached, stop because there is no lower "gram" to go
        
        if(is.na(result[i])) {
          n=n-1
          word_to_search=str_replace(string=word_to_search,pattern="^\\S+\\s",replacement="") #removed the first word and search again
          indices=grep(x=ngram_tab[[n]]$ngrams,pattern=paste("^",word_to_search,sep="")) 
          result[i:4]=str_extract(string=ngram_tab[[n]]$ngrams[indices][1:(4-i+1)],pattern="\\S+\\s$")
        }else{
          i=i+1
          if(i>4) break
        } 
          
      }
      
      if(identical(indices,integer(0))){
        msg='(!)No results found'
      }
      
    } 
       
    
    
    
    
    
    return(list(word_to_search=word_to_search,msg=msg,result=result))
  })
 
  
  
  #Follwing are for the outputs
  
  output$entered_words_last=renderText({
    next_word()$word_to_search
    ##Have to have the parenthesis bc class(nex_word)=c("reactiveExpr","reactive")
    ##Ref: https://stackoverflow.com/questions/36522095/shiny-app-fails-with-argument-1-type-closure-cannot-be-handled-by-cat
  })
  
  
  output$text1 <- renderText({ paste("Author: Peter Wu ( Date: 1/25/2019 )",input$n) }) 
  ##This prints out my authorship. Dont' know why I have to write it here and send it back to ui.R. So stupid.
  ##Ref: https://stackoverflow.com/questions/24049159/change-the-color-and-font-of-text-in-shiny-app
  
  output$next_word_message=renderText({ 
    next_word()$msg
  })
  
  output$predicted_next_word_1=renderText({ 
    next_word()$result[1]
  })
  

  output$predicted_next_word_2=renderText({ 
    next_word()$result[2]
  })
  
  output$predicted_next_word_3=renderText({ 
    next_word()$result[3] 
  })
  
  output$predicted_next_word_4=renderText({ 
    next_word()$result[4] 
  })
  
  

}