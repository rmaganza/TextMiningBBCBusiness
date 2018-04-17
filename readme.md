# R Project: Text Mining business texts

This project was part of our examination for the Data Mining course @ UNIMIB (Università degli Studi di Milano Bicocca).
We tried to extract information from news articles from the BBC archive regarding business.
Among the things we tried (and learned): clustering using k-medoids and assocation rules searching.

## Prerequisites
To run the R script you need R >= 3.4.0.
We also make use of the following packages:

- tm
- snowballC
- wordcloud
- wordcloud2
- cluster
- fpc
- proxy
- here

You may need to install them if you don't have them already. Unfortunately, the R language does not come with a reliable dependency manager.

## Corpus

Since there are a couple of computationally intensive tasks for what concerns the pre-processing of the text, we have bundled the pre-processed documents in an RData file. This includes removal of stopwords and stemming.
To load it, run:

```r
load('docs.RData')
```
