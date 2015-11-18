makeDennogram <- function(corpus, sparse=0.85, freq=2){
	#dennogram
	corpus.td <- TermDocumentMatrix(corpus)
	corpus.dist <- dist(t(as.matrix(corpus.td)))
	
	#freq words
	freqTerms <- findFreqTerms(corpus.td, lowfreq=freq) 
	#remove sparse
	corpus2.td <- removeSparseTerms(corpus.td, sparse=sparse)
 	corpus2.df <- as.data.frame(inspect(corpus2.td))
	#dennogram
	corpus2.df.scale <- scale(corpus2.df)
	d <- dist(corpus2.df.scale, method = "euclidean") 
	# distance 	matrix
	fit <- hclust(d, method="ward")
	plot(fit) # display dendogram?
	
	return(corpus2.df.scale)
	#groups <- cutree(fit, k=5) # cut tree into 5 clusters
	# draw dendogram with red borders around the 5 clusters
	#rect.hclust(fit, k=5, border="red")
	}
	
