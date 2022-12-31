library(rjson)
library(tidyverse)
library(wordcloud2)
require(devtools)
library(devtools)
install_github("lchiffon/wordcloud2")

df <- data.frame(Reduce(rbind, 
                        lapply(readLines("data/climate-fever-dataset-r1.jsonl"),fromJSON)))




wordcloud2( data=demoFreq, size=1.6)
letterCloud(data=demoFreq, size = 1.6, word = "R")

letterCloud(demoFreq, word = "WORDCLOUD2", wordSize = 1)



Reduce(rbind, list(a = c(1,1), b = c(2,2), c = c(3,3)))

Reduce(cbind, c(c(1,1), c(2,2)))
Reduce(rbind, c(c(1,1), c(2,2)))
Reduce(rbind, c(matrix(c(1,1), ncol = 2), matrix(c(2,2), ncol = 2)))
wordcloud2(demoFreq, size = 1,shape = 'star')
