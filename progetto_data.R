rm(list=ls())
library(tm)
library(SnowballC)
library(wordcloud)
library(cluster)
library(fpc)
library(proxy)
library(wordcloud2)
library(here) # used to load corpus in different environments without having to set working directory
library(syuzhet)# for sentiment analysis
library(ggplot2)
set.seed(101) # for reproducibility

# Innanzitutto creo un corpus di documenti:
docs <- Corpus(DirSource(paste0(here(),'/business/business')))


# Tolgo prima di tutto i trattini alti che, altrimenti, togliendoli con il comando predefinito, attaccherebbero le parole
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, '-')


# pulisco i testi con i comandi predefiniti
#writeLines(as.character(docs[[40]]))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, content_transformer(tolower))

# writeLines(as.character(docs[[40]]))


# faccio stopping e stemming e strippo gli spazi vuoti
docs <- tm_map(docs, removeWords, stopwords("SMART"))
docsCopy <- docs
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, stripWhitespace)

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

# the company name "Yukos" gets stemmed to "Yuko"
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "yuko", replacement = "yukos")

#Modifico le parole (es. said) che le metto al presente!
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "said", replacement = "say")


# DESTEMMING
docs <- lapply(docs, stemCompletion2, dictionary=docsCopy)
docs <- Corpus(VectorSource(docs))

#############################
# runnare ogni volta che si riesegue il destemming per qualsiasi motivo e PUSHARE il file docs aggiornato !!!
# save(docs, paste0(here(),'/docs.RData'))
#############################




#Creo la Document-Term matrix attuo una prima valutazione delle frequenze di appirizione delle parole:
# writeLines(as.character(docs[[40]]))
dtm <- DocumentTermMatrix(docs)
inspect(dtm)
freq <- colSums(as.matrix(dtm))
ord <- order(freq, decreasing=T)
freq[head(ord)]
freq[tail(ord)]


# Creo una nuova dtm dove faccio una selezione della frequenza e delle lunghezza dei termini che vi entrano:
dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20), bounds = list(global = c(3,510))))
inspect(dtmr)
freqr=colSums(as.matrix(dtmr))
ordr=order(freqr, decreasing = T)
freqr[head(ordr)]
freqr[tail(ordr)]

# faccio le freq tfxidf
idf <- weightTfIdf(dtmr)
freq_idf <- colSums(as.matrix(idf))
ord_idf <- order(freq_idf, decreasing = T)
freq_idf[head(ord_idf)]
freq_idf[tail(ord_idf)]

#faccio la wordcloud
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
# png(paste0(here(), "/graphs/wordcloud1.png"), width=1280,height=800) # saves the wordcloud
wordcloud(names(freq_idf), freq_idf, max.words = 100, min.freq = 1, colors=pal, scale=c(8, .3), random.order=F, vfont=c('sans serif', 'plain'))
# dev.off()

idf_dataframe<-data.frame(names(freq_idf), freq_idf)
wordcloud2(idf_dataframe, size =.2, minSize = 0.5, shape='star', shuffle=F, color = 'skyblue', backgroundColor = 'black')

#QUESTA PARTE E' EVITABILE, LA TENGO SOLO PER APPREZZARE GLI SFORZI CHO HO FATTO
##########################################################
#Procedo ora con la clusterizzazione dei testi attravero il metodo delle k-means:
 # Preliminarmente individuo quel k che minimizza la DEVin media o quello che massimizza la silhouette:
#DEVin
dev_in=c()
for (k in 1:15){
  km = kmeans(as.matrix(dtmr), k)
  dev_in[k]=km$tot.withinss
}

plot(1:15, dev_in, type='b')

#Sil
d <- dist(as.matrix(dtmr))
head(d)
v_sil=c()
for (i in 2:13){
  km = kmeans(as.matrix(dtmr), i)
  sil =silhouette(km$cluster, d)
  s.sil <- summary(sil)
  v_sil[i-1]=s.sil$avg.width
}

plot(1:12, v_sil, type='b')
#Scelgo k=7 poich? ? il migior trade-off tra una DEVin bassa ed un'alta silhouette media!
km = kmeans(dtmr, 7)
dtmr$clustering = km$cluster

###########################################################
#calcoliamo la distanza 'coseno' e applichiamo l'algoritmo dei k-medoidi con k in base alla silhouette media
distanze <- dist(as.matrix(idf), method='cosine')
#'pamk' --> fornisce anche il k ottimale in funzione di quello che massimizza la silhouette media
cluster <-pamk(distanze, krange = 2:13, diss=T)

idf$clustering <- cluster$pamobject$clustering

gr1 = as.matrix(idf[which(idf$clustering==1),])
gr2 = as.matrix(idf[which(idf$clustering==2),])
gr3 = as.matrix(idf[which(idf$clustering==3),])
gr4 = as.matrix(idf[which(idf$clustering==4),])
gr5 = as.matrix(idf[which(idf$clustering==5),])
gr6 = as.matrix(idf[which(idf$clustering==6),])
gr7 = as.matrix(idf[which(idf$clustering==7),])
gr8 = as.matrix(idf[which(idf$clustering==8),])
gr9 = as.matrix(idf[which(idf$clustering==9),])
gr10 = as.matrix(idf[which(idf$clustering==10),])
gr11 = as.matrix(idf[which(idf$clustering==11),])
gr12 = as.matrix(idf[which(idf$clustering==12),])


###########################

## SENTIMENT ANALYSIS NEL GRUPPO 1 e 3: ESEMPIO
#Gruppo 1
df_text_gr1 <- data.frame(text = sapply(docs[which(idf$clustering==1)], paste, collapse=" "), stringsAsFactors = FALSE)
d <- get_nrc_sentiment(df_text_gr1$text)
td<-data.frame(t(d))

td_new <- data.frame(rowSums(td))

names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
qplot(sentiment, data=td_new, weight=count, geom="bar",fill=sentiment)+ggtitle("Sentiments")

# Gruppo 3
df_text_gr3 <- data.frame(text = sapply(docs[which(idf$clustering==3)], paste, collapse=" "), stringsAsFactors = FALSE)
d <- get_nrc_sentiment(df_text_gr3$text)
td<-data.frame(t(d))

td_new <- data.frame(rowSums(td))

names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
qplot(sentiment, data=td_new, weight=count, geom="bar",fill=sentiment)+ggtitle("Sentiments")
######################

freq1=colSums(gr1)
wordcloud(names(freq1),freq1, min.freq=0.3,colors=brewer.pal(6,"Dark2"))

freq2=colSums(gr2)
wordcloud(names(freq2),freq2, min.freq=0.3,colors=brewer.pal(6,"Dark2"))

freq3=colSums(gr3)
wordcloud(names(freq3),freq3, min.freq=0.3,colors=brewer.pal(6,"Dark2"))

freq4=colSums(gr4)
wordcloud(names(freq4),freq4, min.freq=0.3,colors=brewer.pal(6,"Dark2"))

freq5=colSums(gr5)
wordcloud(names(freq5),freq5, min.freq=0.8,colors=brewer.pal(6,"Dark2"))

freq6=colSums(gr6)
wordcloud(names(freq6),freq6, min.freq=0.75,colors=brewer.pal(6,"Dark2"))

freq7=colSums(gr7)
wordcloud(names(freq7),freq7, min.freq=0.8,colors=brewer.pal(6,"Dark2"))

freq8=colSums(gr8)
wordcloud(names(freq8),freq8, min.freq=0.6,colors=brewer.pal(6,"Dark2"))

freq9=colSums(gr9)
wordcloud(names(freq9),freq9, min.freq=0.4,colors=brewer.pal(6,"Dark2"))

freq10=colSums(gr10)
wordcloud(names(freq10),freq10, min.freq=0.5,colors=brewer.pal(6,"Dark2"))

freq11=colSums(gr11)
wordcloud(names(freq11),freq11, min.freq=0.6,colors=brewer.pal(6,"Dark2"))

freq12=colSums(gr12)
wordcloud(names(freq12),freq12, min.freq=0.3,colors=brewer.pal(6,"Dark2"))

clustersize <- numeric()
for (cluster in 1:12) {
  clustersize[cluster] <- sum(idf$clustering == cluster)
}
clustersize
#######################################################
#faccio le wordscluods dei 7 gruppi

set.seed(42)
freq1=colSums(gr1)
wordcloud(names(freq1),freqr,min.freq=30,colors=brewer.pal(6,"Dark2"))

freq2=colSums(gr2)
wordcloud(names(freq2),freqr,min.freq=30,colors=brewer.pal(6,"Dark2"))

freq3=colSums(gr3)
wordcloud(names(freq3),freqr,min.freq=30,colors=brewer.pal(6,"Dark2"))

freq4=colSums(gr4)
wordcloud(names(freq4),freqr,min.freq=30,colors=brewer.pal(6,"Dark2"))

freq5=colSums(gr5)
wordcloud(names(freq5),freqr,min.freq=30,colors=brewer.pal(6,"Dark2"))

freq6=colSums(gr6)
wordcloud(names(freq6),freqr,min.freq=30,colors=brewer.pal(6,"Dark2"))

freq7=colSums(gr7)
wordcloud(names(freq1),freqr,min.freq=30,colors=brewer.pal(6,"Dark2"))


#OSSERVAZIONE
#potremmo fare un back test attraverso la lettura di documenti che fungono da MEDOIDI:
cluster$pamobject$medoids #"1","273","164","160","438","434","186","498","196","265","392","209"
#dovremmo formalizzare la seguente idea: preso il doc. 1 confrontiamo la wordcloud
#con gli arg principali del testo verficando se, efffettivamente, essendo medoide (e quindi
#riducendo la dissimilarità media con tutte le oss. del proprio gruppo) racchiude tutte, o quasi, le
#parole della sua wordcloud. Se non mi so spiegato bene chiamami.
#P.S. potrebbe essere un'idea idiota.

#DOBBIAMO FARE LE ASSOCIAZIONI MA DIREI CHE LE POSSIAMO FARE GIOVEDI ASSIEME
