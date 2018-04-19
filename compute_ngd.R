library(RCurl)

getGoogleCount <- function(searchTerms=NULL, language="en", ...){
  require(RCurl)
  entry    <- paste(searchTerms, collapse="+")
  siteHTML <- getForm("http://www.google.com/search",
                      hl=language, lr="", q=entry,
                      btnG="Search", .opts=curlOptions(followlocation=T, httpheader=c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:56.0) Gecko/20100101 Firefox/56.0", 'Connection' = 'keep-alive', 'Accept' = '*/*','Accept-Charset' = 'GBK,utf-8;q=0.7,*;q=0.3', 'Cache-Control' = 'max-age=0')))
  print(siteHTML)
  Sys.sleep(5)

  write.table(siteHTML, file="tmp google.txt")
  indicatorWord <- "resultStats"
  posExtractStart <- gregexpr(indicatorWord, siteHTML,
                              fixed = TRUE)[[1]]
  stringExtract <- as.character(substring(siteHTML, first=posExtractStart[2]-30,
                                          last = posExtractStart[2] +50 ))
  count <- strsplit(stringExtract, 'resultStats')[[1]][2]
  count <- strsplit(count, split='results')[[1]][1]
  count <- strsplit(count, split='>')[[1]][2]
  if(length(strsplit(count, split=" ")[[1]])==2){
    count <- strsplit(count, split=" ")[[1]][2]
  }
  count <- as.numeric(gsub(",", "", count))
  return(count)
}

NGD <- function(x,y){
  xy <- getGoogleCount(c(x, y))
  x  <- getGoogleCount(c(x))
  y  <- getGoogleCount(c(y))

  xy <- as.numeric(gsub(",", "", xy))
  x  <- as.numeric(gsub(",", "", x ))
  y  <- as.numeric(gsub(",", "", y ))
  M <- 859000000
  dist <- (max(log(x), log(y)) - log(xy))/(log(M)-min(log(x), log(y)))
  return(dist)
}


compute_NGD_for_combinations <- function (topiclist) {
  NGD_vector <- numeric()
  count <- 1
  for (combination in combn(topiclist, 2, simplify = FALSE)) {
    if (!is.na(combination)){
    ngd <- NGD(combination[1], combination[2])
    print(ngd)
    NGD_vector[count] <- ngd
    count <- count+1
    }
  }
  mean(NGD_vector, na.rm=T)
}
