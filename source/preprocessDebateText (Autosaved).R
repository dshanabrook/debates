#debates
#http://www.presidency.ucsb.edu/debates.php
#preprocessing
democratOct13 <- "data/democratic13Oct2015.txt"
democratNov13 <- "data/democratic13nov2015.txt"


republicanAug6 <- "data/republican6Aug2015.txt"
republicanSept16 <- "data/republican16Sept2015.txt"
republicanOct28 <- "data/republican28Oct2015.txt"
republicanNov10 <- "data/republican10Nov2015.txt"

getSpeaker <- function(theSpeakers= democraticSpeakers, debate) {
	text <- debate[debate$speaker %in% theSpeakers,]$text
	text <- paste(text,collapse=" ")
	return(text)
}
getADebate <- function(theFile) {
	aDebate <- readLines(theFile)
	aDebate <- data.frame(text = aDebate, stringsAsFactors=F)
	return(aDebate)
}

findTheSpeakers <- function(debate){
	personPatt <- "^[A-Z']*:"
	x <- gregexpr(personPatt, debate$text)
	debate$speaker <- regmatches(debate$text, x)
	return(debate)
}
makeSpeakerColumn <- function(debate) {
		personPatt <- "^[A-Z']*:"
	debate$speaker <- lapply(debate$speaker, function(x) ifelse(is.null(x), 
		NA, x))
	debate$speaker <- unlist(debate$speaker)
	debate$text <- gsub(personPatt, "", debate$text)
	library(tidyr)
	debate <- fill(debate, speaker)
	debate$speaker <- as.factor(debate$speaker)
	return(debate)
}

debate <- getADebate(democratOct13)
debate1 <- getADebate(democratNov13)
debate2 <- getADebate(republicanAug6)
debate3 <- getADebate(republicanSept16)
debate4 <- getADebate(republicanOct28)
debate5 <- getADebate(republicanNov10)

debate <- rbind(debate,debate2,debate3,debate4,debate5)
debate <- findTheSpeakers(debate)
debate <- makeSpeakerColumn(debate)

#who are they
summary(debate$speaker)
democraticSpeakers <- c("CLINTON:","SANDERS:","DICKERSON:", "O'MALLEY:","WEBB:")
democrat <- getSpeaker(democraticSpeakers,debate)
republicanSpeakers <- c( "BUSH:", "CARSON:", "CHAFEE:", "CHRISTIE:", "COOPER:", "CRUZ:", "FIORINA:", "HUCKABEE:", "KASICH:",  "PAUL:", "PERRY:", "RUBIO:", "TRUMP:", "WALKER:")
republican <- getSpeaker(republicanSpeakers, debate)
write(democrat, file="data/democrat")
write(republican, file="data/republican")
