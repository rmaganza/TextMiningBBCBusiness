# R Project: Text Mining business texts

This project was part of our examination for the Data Mining course @ UNIMIB (Università degli Studi di Milano Bicocca).
We tried to extract information from news articles from the BBC archive regarding business.
The final aim was to compare the performance of a Latent Dirichlet Allocation topic modeling algorithm agains a baseline created ad hoc by the authors.

Among the things we learned: document clustering, topic modeling, semantic coherence, stemming algorithms, web scraping.

## Prerequisites
To run the R script you need R >= 3.4.0 and Python 2.7+.
We also make use of the following R packages:

- tm
- snowballC
- wordcloud
- syuzhet
- ggplot2
- topicmodels
- tidytext
- dplyr
- cluster
- fpc
- proxy
- here
- reticulate

and of the following Python packages:

- wordcloud
- Pillow
- numpy
- watson_developer_cloud

You may need to install them if you don't have them already. Unfortunately, the R language does not come with a reliable dependency manager.

We make use of the R package "reticulate" to communicate between the two languges. You will find chunks of Python code in R scripts and seemingly nonsense Python standalone files :) .

## Corpus

Since there are a couple of computationally intensive tasks for what concerns the pre-processing of the text, we have bundled the pre-processed documents in an RData file. This includes removal of stopwords, stemming, and stem completion.
To load it, run:

```r
load('rdata_files/docs.RData')
```


## Replicability

We've tried to ensure replicability of the project but despite setting seeds, various R modeling functions seem to run randomly. We will correct the code if we work it out.
