setwd("~/ShinyApps/debates")
#libraries sources
{
	library(tm)
	library(ggplot2)
	library(gdata)
	source("~/Documents/r/functions/sort.data.frame.R")
	source("source/preprocessDebateText.R")
}
specialWords <- c("anderson","applause")
optimal.spacing <- function(spaces) {
	if (spaces > 1) {
		spacing <- 1/spaces
		if (spaces%%2 > 0) {
			lim <- spacing * floor(spaces/2)
			return(seq(-lim, lim, spacing))
		} else {
			lim <- spacing * (spaces - 1)
			return(seq(-lim, lim, spacing * 2))
		}
	} else {
		return(0)
	}
}
#from fbettercloud
rightDoc <- republican
leftDoc <- democrat
docs <- c(rightDoc, leftDoc)
vs <- VectorSource(docs)
corpus <- Corpus(vs)
#custom corpus manipulation
{
	corpus <- tm_map(corpus, content_transformer(tolower))
	corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
	corpus <- tm_map(corpus, removePunctuation)
	corpus <- tm_map(corpus, PlainTextDocument)
	corpus <- tm_map(corpus, function(x) removeWords(x, specialWords))
#	corpus <- tm_map(corpus, stemDocument)
}
#termDocument  & scaling
{
	corpus.matrix <- TermDocumentMatrix(corpus)
	sink(file = "temp")
	corpus.df <- as.data.frame(inspect(corpus.matrix))
	sink()
	#corpus.df.scale <- scale(corpus.df, center=FALSE)
	}
	
source("source/fgraphing.R")