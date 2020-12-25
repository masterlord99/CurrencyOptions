load("data.RDATA")
lastday <- tail(history$TIME,1)

danes <- Sys.Date()
download.file("https://www.bsi.si/_data/tecajnice/dtecbs.xml","tmp2.xml")

data <- xmlParse("tmp2.xml")
xml_data <- xmlToList(data)

x <- unlist(xml_data[[1]])

df <- as.data.frame(x)
df <- as.data.frame(t(df),stringsAsFactors = F)
df <- df[,1:(ncol(df)-1)]
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



if(df$TIME > lastday){
  err <- T
  try({
  history <- rbind(history,df[,colnames(df) %in% colnames(history)])
  err <- F
  rm(list=setdiff(ls(),c("history")))
  save.image("data.RDATA")
  },silent=T)
  if(err){
    source("historyDataImport.r")
  }
}