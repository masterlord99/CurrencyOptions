library(RCurl)
library(jsonlite)
library(digest)
library(anytime)
library(dplyr)

require(XML)
load("data.RDATA")

download.file("https://www.bsi.si/_data/tecajnice/dtecbs-l.xml","tmp.xml")
data <- xmlParse("tmp.xml")
xml_data <- xmlToList(data)

x <- unlist(xml_data[[1]])

df <- as.data.frame(x)
df <- as.data.frame(t(df),stringsAsFactors = F)
df <- df[,1:102]
df <- df[,c(T,T,F)]
ime <- df[,c(F,T)]
valuta <- df[,c(T,F)]

im <- c()
for(i in 1:ncol(ime)){
  im <- c(im,as.character(ime[1,i]))
}

colnames(valuta) <- im

df <- valuta
rm(ime,valuta,im)
df <- cbind(TIME=as.Date(tail(x,1)),df)
df <- df[,1:29]
df <- df[, colnames(df)!="ISK"]
df <- df[, colnames(df)!="BGN"]
w <- length(xml_data)

for(i in 2:(length(xml_data)-1)){
  df[i,] <- NA
  x <- unlist(xml_data[[i]])
  df$TIME[i] <- tail(x,1) %>% as.Date()
  for(j in 2:ncol(df)){
    if(colnames(df)[j] %in% x){
    ind <- which(x==colnames(df)[j]) %>% as.numeric()
    df[i,j] <- x[ind - 1] %>% as.numeric()
    }
  }
  print(i/w)
}

#history <- df

kat <-  !(df$TIME %in% history$TIME)



history <- rbind(history,df[kat,])

history[,2:ncol(history)] <- apply(history[,2:ncol(history)],2,as.numeric)

#naniar::gg_miss_var(history[,2:ncol(history)],show_pct = T)
rm(list=setdiff(ls(),c("history")))
save.image("data.RDATA")

