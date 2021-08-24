setwd("C:/corpus_final"); getwd()
library(wordcloud)
library(AMR)

# reference corpus - frequency table
ref_cor <- read.delim(file = "10_Sejong_Freq.txt", header = T,
                      row.names = 1, quote = NULL)
head(ref_cor, 20)

# Frequency Analysis - including Function words
for (company in c("Hanhwa", "LG")) {
  for (year in c(2016:2020)) {
    word <- scan(file = paste(year, company, "NewYear.txt.tag", sep = "_"),
                 encoding = "UTF-8", what = "char", quote = NULL)
    word <- unlist(strsplit(word, "[+]"))
    word.freq <- data.frame(sort(table(word), decreasing = T))
    print(paste(company, year))
    print(head(word.freq, 20))
  }
}

# Frequency Analysis - except Function words
for (company in c("Hanhwa", "LG")) {
  for (year in c(2016:2020)) {
    word <- scan(file = paste(year, company, "NewYear.txt.tag", sep = "_"),
                 encoding = "UTF-8", what = "char", quote = NULL)
    word <- unlist(strsplit(word, "[+]"))
    word.freq <- data.frame(sort(table(word), decreasing = T))
    word.freq <- word.freq[grep("/NNG|/NNP|/VV|/VA|/MAG|SL", word.freq$word),]
    print(paste(company, year))
    print(head(word.freq, 20))
  }
}

# KeyWord - for comparison
for (year in c(2016:2020)){
  TDM <- data.frame(word=vector())
  for (company in c("Hanhwa", "LG")){
    word <- scan(file = paste(year, company, "NewYear.txt.tag", sep = "_"),
                 encoding = "UTF-8", what = "char", quote = NULL)
    word <- unlist(strsplit(word, "[+]"))
    TDM <- merge(TDM, data.frame(table(word)),
                 by.x="word", by.y = "word", all = T)
    colnames(TDM)[length(TDM)] <- company
  }
  TDM <- data.frame(row.names = TDM$word, TDM[2:length(TDM)])
  TDM[is.na(TDM)] <- 0
  comparison.cloud(TDM[c(1,2)], random.order = F, scale = c(1,0.5),
                   rot.per = 0.3, max.words = 150,
                   colors = brewer.pal(8,"Dark2"), title.size = 1.0)
  CHI <- chisq.test(TDM[c(1,2)])$residuals
  CHI <- as.data.frame(CHI)
  print(year)
  print(head(CHI[order(CHI$Hanhwa, decreasing = T),], 20))
  print(head(CHI[order(CHI$LG, decreasing = T),], 20))
}


# Bi-gram Analysis - LG - Customer
customer.bi <- vector()
for (year in c(2016:2020)) {
  word <- scan(file = paste(year, "LG", "NewYear.txt.tag", sep = "_"),
                encoding = "UTF-8", what = "char", quote = NULL)
  word <- unlist(strsplit(word, "[+]"))
  cust <- which(word %in% c("고객__04/NNG"))
  for (i in cust){
    customer.bi <- c(customer.bi,
                     paste(word[i-1], word[i]),
                     paste(word[i], word[i+1]))
  }
}
cust.Freq <- data.frame(sort(table(customer.bi),decreasing = T))
head(cust.Freq, 10)

# Concordance - /JKO
node <- "/JKO$"
for (company in c("Hanhwa", "LG")) {
  for (year in c(2016:2020)) {
    word <- scan(file = paste(year, company, "NewYear.txt.tag", sep = "_"),
                 encoding = "UTF-8", what = "char", quote = NULL)
    word <- unlist(strsplit(word, "[+]"))
    index <- grep(node, word)
    span <- vector()
    for (i in index){
      span <- c(span ,c((i-4):(i-1), (i+1):(i+4)))
    }
    span <- span[span>0 & span <= length(word)]
    crc <- word[span]
    Freq.span <- sort(table(crc), decreasing = T)
    Freq.all <- table(word)
    Freq.co <- data.frame(W1 = vector(), W2 = vector(),
                          W1W2 = vector(), N = vector())
    n <- 1
    for (i in (1:length(Freq.span)))
    {
      Freq.co[n,] <- c(length(index), Freq.all[names(Freq.all)==names(Freq.span)[i]],
                       Freq.span[i], length(word))
      rownames(Freq.co)[n] <- names(Freq.span)[i]
      n <- n+1
    }
    collocates <- data.frame(Freq.co,
                             MI = log2((Freq.co$W1W2*Freq.co$N)/
                                         (Freq.co$W1*Freq.co$W2)))
    MI.sort <- collocates[order(collocates$MI, decreasing = T),]
    print(paste(company, year))
    print(head(MI.sort[MI.sort$W1W2>2&grepl('/(N|V).+$', rownames(MI.sort)),], 20))
  }
}
# CA
stopword <- scan(file="stopword_KOR.txt.tag", quote = NULL, what = "char",
                 encoding = "UTF-8")
stopword <- unique(unlist(strsplit(stopword, "[+]")))

for (company in c("Hanhwa", "LG")){
  TDM <- data.frame(word=vector())
  for (year in c(2016:2020)){
    word <- scan(file = paste(year, company, "NewYear.txt.tag", sep = "_"),
                 encoding = "UTF-8", what = "char", quote = NULL)
    word <- unlist(strsplit(word, "[+]"))
    word <- word[!(word %in% stopword)]
    word <- word[grep("/NNG|/NNP|/VV|/VA|/MAG|SL", word)]
    TDM <- merge(TDM, data.frame(table(word)),
                 by.x="word", by.y = "word", all = T)
    colnames(TDM)[length(TDM)] <- year
  }
  TDM <- data.frame(row.names = TDM$word, TDM[2:length(TDM)])
  TDM[is.na(TDM)] <- 0
  colnames(TDM) <- substring(colnames(TDM), 2)
  TDM['RowSums'] <- rowSums(TDM)
  TDM <- TDM[order(TDM$RowSums, decreasing = T),]
  plot(ca(TDM[1:20, -length(TDM)]), arrows = c(1,0))
}