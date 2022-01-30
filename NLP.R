library(stringr)
library(ggplot2)
library(LSAfun)
library(igraph)
library(tidyverse)
library(rvest)
library(lexRankr)
library(textmineR)
library(quanteda)





#we delete all the columns except the headlines and complete text of the article
news_summary$author <- NULL
news_summary$date <- NULL
news_summary$read_more <- NULL
news_summary$headlines <- NULL

#delete empty values
news_summary=na.omit(news_summary)

#delete characters that we don't want in the dataset
news_summary$ctext[2]=str_replace_all(news_summary$ctext[2],"[?]"," ")


news_summary$ctext[2]

#Perform summarization, ranking most important sentences
summarylexrank = lexRankr::lexRank(news_summary$ctext[2],
                                 
                                  docId = rep(1, length(news_summary$ctext[2])),
                                  
                                  n = 3,
                                  continuous = TRUE)

summarylexrank$sentence


summaryLSA=genericSummary(news_summary$ctext[2],k=1)

wordssummaryLSA=str_count(summaryLSA, '\\w+')

summaryLSA


wordsoriginal=str_count(news_summary$ctext[2], '\\w+')
wordssummary=str_count(summarylexrank$sentence, '\\w+')


data <- data.frame(
  text=c("article","summarylex") ,  
  numberwords=c(wordsoriginal,wordssummary)
)

data2 <- data.frame(
  text=c("article","summaryLSA") ,  
  numberwords=c(wordsoriginal,wordssummaryLSA)
)

ggplot(data, aes(x=text, y=numberwords)) + 
  geom_bar(stat = "identity", fill="blue")


ggplot(data2, aes(x=text, y=numberwords)) + 
  geom_bar(stat = "identity", fill="blue")


