#Do summary statistics for 3 different text files.
##Part I: read the files
##Part II: Summary statistics


##Part I: read the files
library(stringr) ##load to use the str_extract()
library(tidyverse)

#function to read large txt file line by line & store as a vector
parseTXT=function(file){
  con = file(file, "r")
  
  dat=c()
  i=1
  while ( TRUE ) {
    line = readLines(con, n = 1) ##readLines will put escape characters if the line being read contains any meta characters
    
    ##stop parsing at the end of the file
    if ( length(line) == 0 ) {
      break
    }
    
    print(line) ##print the line being processed
    
    dat[i]=line
    i=i+1
    
  }
  close(con)
  
  return(dat)
}



##parse 3 files
en_US.twitter=parseTXT("/Users/peterwu/TAMU Google Drive/Coursera Data Science/Capstone/final/en_US/en_US.twitter.txt")
en_US.news = parseTXT("/Users/peterwu/TAMU Google Drive/Coursera Data Science/Capstone/final/en_US/en_US.news.txt")
en_US.blogs = parseTXT("/Users/peterwu/TAMU Google Drive/Coursera Data Science/Capstone/final/en_US/en_US.blogs.txt")



##Examine 3 corpus that have been loaded by the .RData
txt_list=list(en_US.twitter=en_US.twitter,
              en_US.news=en_US.news,
              en_US.blogs=en_US.blogs)

str(txt_list)

#save(txt_list,file="/Users/peterwu/TAMU Google Drive/Coursera Data Science/Capstone/txt_list.RData")
#load("/Users/peterwu/TAMU Google Drive/Coursera Data Science/Capstone/txt_list.RData")

##Function to do word count
count_word=function(txt) { sum(stringr::str_count(txt,"[a-zA-Z]+")) }


line.count=sapply(txt_list,length)
word.count=sapply(txt_list,count_word)

par(mfrow=c(1,2))
barplot(line.count)

y = format(1:8*5e6, scientific = FALSE)
barplot(word.count)
axis(side=2,at=1:8*5e6,labels=y)

##Line count & word count
line.count
word.count


##function to extract all the words
extract_all_words=function(sentence){
  str_extract_all(sentence,"\\S+") %>% unlist
}

##function to remove , ? ! . ... from each word
remove_marks=function(characters){
  str_replace_all(characters,"[?,.;:\"&]","")
}


##extract words and remove symbols from each corpus
if(!file.exists("/Users/peterwu/TAMU Google Drive/Coursera Data Science/Capstone/final/word_list_Coursera_Capstone.RData")){
  word_list=lapply(txt_list,function(x){
    x %>% tolower %>% extract_all_words %>% remove_marks
  })
  ##This takes a while, so I saved it
  save(word_list,file="/Users/peterwu/TAMU Google Drive/Coursera Data Science/Capstone/final/word_list_Coursera_Capstone.RData")
}else{load("/Users/peterwu/TAMU Google Drive/Coursera Data Science/Capstone/final/word_list_Coursera_Capstone.RData")}


#Create tables for word counts
word_sorted_table_list=lapply(word_list,function(x){
  x %>% table %>% sort(decreasing=T)
})




#barplot showing frequency of word usage
par(mfrow=c(1,3))

##en_US.twitter
barplot(word_sorted_table_list[[1]][1:10],main="en_US.twitter")
##en_US.news
barplot(word_sorted_table_list[[1]][1:10],main="en_US.news")
##en_US.blogs
barplot(word_sorted_table_list[[1]][1:10],main="en_US.blogs")






