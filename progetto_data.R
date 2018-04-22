rm(list=ls())
library(tm)
library(SnowballC)
library(wordcloud)
library(cluster)
library(fpc)
library(proxy)
library(here) # used to load corpus in different environments without having to set working directory
library(syuzhet)# for sentiment analysis
library(ggplot2)
library(topicmodels)
source(paste0(here(),'/compute_ngd.R'))
library(tidytext)
library(dplyr)
library(reticulate)
set.seed(101) # for reproducibility

pythonpath = '/Users/XelmagaX/anaconda3/bin/python'
use_python(pythonpath, required=T)

get_top_words_from_dtm <- function (dtm, n) {
  freqr=colSums(as.matrix(dtm))
  ordr=order(freqr, decreasing = T)
  freqr[head(ordr, n)]
}

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})

# Innanzitutto creo un corpus di documenti:
docs <- Corpus(DirSource(paste0(here(),'/business/business')))
docs_raw <- docs
# save(docs_raw, file=paste0(here(),'/rdata_files/docs_raw.RData'))

# Tolgo prima di tutto i trattini alti che, altrimenti, togliendoli con il comando predefinito, attaccherebbero le parole
docs %<>% tm_map(toSpace, '-') %>%
         tm_map(removePunctuation) %>%
         tm_map(content_transformer(tolower)) %>%
         tm_map(removeNumbers) %>%
         tm_map(removeWords, c(stopwords("SMART"), 'year', 'hour', 'month'))

docsCopy <- docs

docs %<>% tm_map(stemDocument) %>% tm_map(stripWhitespace) %>%
        # the company name "Yukos" gets stemmed to "Yuko"
         tm_map(content_transformer(gsub), pattern = "yuko", replacement = "yukos") %>%
         tm_map(content_transformer(gsub), pattern = "said", replacement = "say") %>%
         tm_map(content_transformer(gsub), pattern = "lanka", replacement = "srilanka") %>% tm_map(content_transformer(gsub), pattern = "russian", replacement = "russia") %>%

######## DESTEMMING #######
         lapply(stemCompletion2, dictionary=docsCopy)
docs <- Corpus(VectorSource(docs))

#############################
# runnare ogni volta che si riesegue il destemming per qualsiasi motivo e PUSHARE il file docs aggiornato !!!
# save(docs, file=paste0(here(),'/rdata_files/docs.RData'))
#############################


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


##### DOLLAR CLOUD GENERATION: Reticulate Interface
names_freq <- names(freq_idf)
maskpath <- paste0(here(), 'graphs/dollar_sign.png')
wordcloud_output_path <- paste0(here(), '/graphs/dollarcloud.png')
PIL <- import('PIL')
np <- import('numpy')
builtins <- import_builtins()
py$tuple_dict <- builtins$dict(builtins$zip(names_freq, freq_idf))
py$mask <- np$array(PIL$Image$open(maskpath))
source_python(paste0(here(), '/python/wordcloud_generator.py'))

###########################
#calcoliamo la distanza 'coseno' e applichiamo l'algoritmo dei k-medoidi con k in base alla silhouette media
distanze <- dist(as.matrix(idf), method='cosine')
#'pamk' --> fornisce anche il k ottimale in funzione di quello che massimizza la silhouette media
set.seed(101)
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
qplot(sentiment, data=td_new, weight=count, geom="bar",fill=sentiment) + ggtitle("Sentiments")
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


#OSSERVAZIONE
#potremmo fare un back test attraverso la lettura di documenti che fungono da MEDOIDI:
cluster$pamobject$medoids #"1","273","164","160","438","434","186","498","196","265","392","209"
#dovremmo formalizzare la seguente idea: preso il doc. 1 confrontiamo la wordcloud
#con gli arg principali del testo verficando se, efffettivamente, essendo medoide (e quindi
#riducendo la dissimilarità media con tutte le oss. del proprio gruppo) racchiude tutte, o quasi, le
#parole della sua wordcloud. Se non mi so spiegato bene chiamami.
#P.S. potrebbe essere un'idea idiota.

#DOBBIAMO FARE LE ASSOCIAZIONI MA DIREI CHE LE POSSIAMO FARE GIOVEDI ASSIEME

###########################################################
## TOP 50 TERMS FOR EACH GROUP

get_top_words_from_dtm(gr1, 25)
get_top_words_from_dtm(gr2, 25)
get_top_words_from_dtm(gr3, 25)
get_top_words_from_dtm(gr4, 25)
get_top_words_from_dtm(gr5, 25)
get_top_words_from_dtm(gr6, 25)
get_top_words_from_dtm(gr7, 25)
get_top_words_from_dtm(gr8, 25)
get_top_words_from_dtm(gr9, 25)
get_top_words_from_dtm(gr10, 25)
get_top_words_from_dtm(gr11, 25)
get_top_words_from_dtm(gr12, 25)

topicslist_manual <- list(c('profit', 'sale', 'game', 'share', 'earn'), c('dollar', 'crude', 'deficit', 'bush', 'barrel'), c('yukos', 'russia', 'gazprom', 'court', 'auction'), c('fiat', 'italy', 'saab', 'opel', 'motor'), c('economy', 'growth', 'house', 'unemployed', 'inflation'), c('china', 'yuan', 'japan', 'israel', 'islam'), c('lanka', 'disaster', 'people', 'indonesia', 'tsunami'), c('airline', 'india', 'qantas', 'airbus', 'lufthansa'), c('börse', 'deutsche', 'euronext', 'takeover', 'shareholder'), c('retail', 'sale', 'store', 'christmas', 'lvmh'), c('ebbers', 'fraud', 'verizon', 'qwest', 'lawyer'), c('insurance', 'marsh', 'pension', 'investigation', 'plead'))

topics_manual_ngd <- numeric(12)
index <- 1
for (topics in topicslist_manual) {
  topics_manual_ngd[index] <- compute_NGD_for_combinations(topics)
  index <- index+1
}
topics_manual_ngd
# save(topics_manual_ngd, file=paste0(here(),'/rdata_files/topics_manual_ngd.RData'))


## LATENT DIRICHLET ALLOCATION
lda_model <- LDA(dtmr, k=12, control = list(seed = 1234))
ap_topics <- tidy(lda_model, matrix = "beta")
ap_topics <- ap_topics[-which(ap_topics$term=='list'),]
ap_topics <- ap_topics[-which(ap_topics$term=='character'),]

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>% print(n=Inf)

automatic_lda_topics <- list()

automatic_lda_topics [1] <- ap_top_terms[1:5,'term']
automatic_lda_topics [2] <- ap_top_terms[6:10,'term']
automatic_lda_topics [3] <- ap_top_terms[11:15,'term']
automatic_lda_topics [4] <- ap_top_terms[16:20,'term']
automatic_lda_topics [5] <- ap_top_terms[21:25,'term']
automatic_lda_topics [6] <- ap_top_terms[26:30,'term']
automatic_lda_topics [7] <- ap_top_terms[31:35,'term']
automatic_lda_topics [8] <- ap_top_terms[36:40,'term']
automatic_lda_topics [9] <- ap_top_terms[41:45,'term']
automatic_lda_topics [10] <- ap_top_terms[46:50,'term']
automatic_lda_topics [11] <- ap_top_terms[51:55,'term']
automatic_lda_topics [12] <- ap_top_terms[56:60,'term']

topics_automatic_ngd <- numeric(12)
index <- 1
for (topics in automatic_lda_topics) {
  topics_automatic_ngd[index] <- compute_NGD_for_combinations(topics)
  index <- index+1
}
topics_automatic_ngd

# save(topics_automatic_ngd, automatic_lda_topics, file=paste0(here(),'/rdata_files/topics_automatic_ngd.RData'))


## WE ADD OUR FIRST SELECTED TOPICS BY THE ALGORITHM BECAUSE WE CANNOTE ENSURE REPLICABILITY

topics_automatic_manually_added <- list(c('dollar', 'deficit', 'euro', 'budget', 'trade'), c('bank', 'companies', 'firm', 'deal', 'financial'), c('companies', 'firm', 'worldcom', 'ebbers', 'telecom'), c('russia', 'yukos', 'court', 'companies', 'firm'), c('airline', 'cost', 'report', 'fuel', 'india'), c('offer', 'deutsche', 'börse', 'share', 'london'), c('price', 'house', 'market', 'china', 'mortgage'), c('economic', 'growth', 'rate', 'rise', 'figure'), c('club', 'unit', 'glazer', 'invest', 'argentina'), c('economic', 'countries', 'govern', 'world', 'people'), c('profit', 'sale', 'share', 'market', 'companies'), c('companies', 'drug', 'call', 'firm', 'customer'))
ngd_auto_added <- numeric(12)
index <- 1
for (topics in topics_automatic_manually_added) {
  ngd_auto_added[index] <- compute_NGD_for_combinations(topics)
  index <- index+1
}
ngd_auto_added
