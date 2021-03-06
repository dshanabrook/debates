cutoffWordFr <- 20
outlierLimit <- 25
{
	#cutoffWordFr <- 10
	names(corpus.df) <- c("rightText", "leftText")
	sizeFactor <- sum(corpus.df$leftText)/sum(corpus.df$rightText)
	corpus.df <- subset(corpus.df, (rightText + leftText) > cutoffWordFr)
	corpus.df$rightText <- round(corpus.df$rightText * sizeFactor)
	corpus.df <- transform(corpus.df, freq.dif = rightText - leftText)
	corpus.df[(abs(corpus.df$freq.dif)<=1),]$freq.dif <- 0
	}
#OUTLIER PROBLEM
{
	outlierLimit <- 23
	table(corpus.df$freq.dif)
	corpus.df[(corpus.df$freq.dif < -outlierLimit), ]$freq.dif <- -outlierLimit
	corpus.df[(corpus.df$freq.dif > outlierLimit), ]$freq.dif <- outlierLimit
	table(corpus.df$freq.dif)
}
#spacing
{
maxDiff <- 0
minDiff <-0
rightTextdf <- subset(corpus.df, freq.dif > maxDiff) # Said more often by rightText
leftTextdf <- subset(corpus.df, freq.dif < minDiff) # Said more often by leftText
equaldf <- subset(corpus.df, (freq.dif >= maxDiff & freq.dif <= 
	minDiff)) # Said equally

rightText.spacing <- sapply(table(rightTextdf$freq.dif), function(x) optimal.spacing(x))
leftText.spacing <- sapply(table(leftTextdf$freq.dif), function(x) optimal.spacing(x))
equal.spacing <- sapply(table(equaldf$freq.dif), function(x) optimal.spacing(x))

# Add spacing to data frames
rightText.optim <- rep(0, nrow(rightTextdf))
for (n in names(rightText.spacing)) {
	rightText.optim[which(rightTextdf$freq.dif == as.numeric(n))]<-rightText.spacing[[n]]
	}
rightTextdf <- transform(rightTextdf, Spacing = rightText.optim)

leftText.optim <- rep(0, nrow(leftTextdf))
for (n in names(leftText.spacing)) {
	leftText.optim[which(leftTextdf$freq.dif == as.numeric(n))]<- leftText.spacing[[n]]
	}
leftTextdf <- transform(leftTextdf, Spacing = leftText.optim)

equaldf$Spacing <- as.vector(equal.spacing)
}
#labeling
{
	labelEqu <- "Said Equally"
	labelPos <- "Republican"
	labelNeg <- "Democrat"
	labTitle <- paste("Debate Cloud:", labelNeg, "vs", labelPos, sep = " ")
}
#plotting
{
	jitterParm <- position_jitter(w = 0.1, h = 0.1)
	p1 <- ggplot(rightTextdf, aes(x = freq.dif, y = Spacing)) +  
		geom_text(position = jitterParm, aes(size = rightText, label = row.names(rightTextdf), colour = freq.dif)) + 
		geom_text(data = leftTextdf, position = jitterParm, aes(x = freq.dif, y = Spacing, label = row.names(leftTextdf), size = leftText, color = freq.dif)) + 
		geom_text(data = equaldf, aes(x = freq.dif, y = Spacing, label = row.names(equaldf), size = rightText, color = freq.dif)) + scale_size(range = c(3, 13), name = "word frequency") + 
		scale_colour_gradient(low = "blue", high = "red", guide = "none") + 
		scale_x_continuous(breaks = c(min(leftTextdf$freq.dif), 0, max(rightTextdf$freq.dif)), labels = c(labelNeg, labelEqu, labelPos)) + 
		scale_y_continuous(breaks = c(0), expand=c(0,0.1),  labels = c("")) + 
		xlab("") + ylab("") + labs(title = labTitle) + xlab("") + 
		ylab("")

	p1 + theme_bw()  
#		p1 + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

}
