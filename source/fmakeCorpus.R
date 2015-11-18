# in text, out corpus
#text is either vectorsource or dirsource
makeCorpus <- function(text, stemComplete = FALSE){
	corpus <- Corpus(text)
	corpus <- tm_map(corpus, removePunctuation)
	corpus <- tm_map(corpus, tolower)
	corpus <- tm_map(corpus, removeNumbers)
	corpusUnstem <- corpus
	corpus <- tm_map(corpus, stemDocument, language = "english")
	corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
	if (stemComplete)
		corpus <- tm_map(corpus, stemCompletion, dictionary=corpusUnstem)
	return(corpus)
	}
	
	#example
	# text <- c("one", "two two", "three three three", "one two three four")
	# df <- data.frame(text, stringsAsFactors = FALSE)
	# corpus <- Corpus(VectorSource(df$text))
	# corpus.td <- TermDocumentMatrix(corpus)