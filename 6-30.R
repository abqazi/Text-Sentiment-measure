library(tm)
library(stringr)
library(tokenizers)


filepath = "C:/Users/Hannan Qazi/Desktop/ClassDemonstration"
setwd(filepath)

negation_word=c("not","nor","no","none","doesnt","nobody","never","nothing",
                "without","lack","havent","dont","isnt","arent")

sentiment_dictionary=as.data.frame(read.csv("Sentiment Database.csv"))
head(sentiment_dictionary)

file = "HBO_NOW.txt"
text =file(file, open= "r")
text.decomposition = readLines(text)
text.decomposition[20]

docnumber=length(text.decomposition)
senti.score=rep(0,docnumber)
#sentence=unlist(tokenize_sentences(text.decomposition[20],strip_punctuation = TRUE))
#sentence

for(i in 1:docnumber){
  polarity_net=0
  polarity_total=0
  sentences=unlist(tokenize_sentences(text.decomposition[i],strip_punctuation = TRUE))
  corpus=corpus(VectorSource(sentences))
  corpus=tm_map(corpus,tolower)
  
  #analyze sentences
  
  for(j in 1:length(corpus)){
    negation.sign=1
    text.temp=corpus[[j]][1]
    term.list=tokenize_words((text.temp))
    
    for (term in term.list){
      if (term %in% negation_word){
        if(negation.sign==-1)
        {negation.sign=-1}
        else
        {negation.sign=1}
      }
      if (term %in% sentiment_dictionary$word){
        temp.pola= sentiment_dictionary$polarity[sentiment_dictionary$word==term][1]
        temp.strength= sentiment_dictionary$type[sentiment_dictionary$word==term][1]
        
        term.score=0
        
        if(term.pola == 'negative' & term.strength == 'strongsubj'){term.score=-2}
        if(term.pola == 'negative' & term.strength == 'weaksubj'){term.score=-1}
        if(term.pola == 'positive' & term.strength == 'weaksubj'){term.score=1}
        if(term.pola == 'positive' & term.strength == 'strongsubj'){term.score=2}
        
        polarity_net=polarity_net+negation.sign*term.score
        polarity_total=polarity_total+abs(term.score)
      }
    }
  }
  if(polarity_total!=0){
    senti.score[i]=polarity_net/polarity_total
  }
}

