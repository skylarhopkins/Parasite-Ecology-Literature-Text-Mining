#Skylar Hopkins - October 2017

#code canibalized from two great sources
#https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html
#http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

#load libraries
library(bibliometrix) #for pulling text from web of science bibtex 
library(tm)  #for text mining
library(SnowballC) #for text stemming
library(wordcloud) #word-cloud generator 
library(RColorBrewer) #color palettes

#Outside of this script, I ran a Web of Knowledge search for
#papers containing the terms parasit* AND ecology. I retained every paper (N=410),
#and downloaded the titles and abstracts as a bibtex file
RawBibData <- readFiles("~/Documents/savedrecs.bib")
Data <- convert2df(RawBibData, dbsource = "isi", format = "bibtex")

#For fun, you can look at some general bibliometric stats
BibResults<-biblioAnalysis(Data, sep = ";")
BibResultsSummary<-summary(object = BibResults, k = 10, pause = FALSE)

#Pull the words in the title (TI) and abstract (AB) for text mining
#We turn this into a "Corpus" for text mining
Words <- VCorpus(DataframeSource(x=cbind(Data$TI, Data$AB)))

##We clean up the text to prepare for further analysis
# Convert the text to lower case
Words <- tm_map(Words, content_transformer(tolower))
# Remove numbers
Words <- tm_map(Words, removeNumbers)
# Remove english common stopwords
Words <- tm_map(Words, removeWords, stopwords("english"))
# specify your stopwords as a character vector
Words <- tm_map(Words, removeWords, c("can", "new", "one", "two", "also", "may", "three", "however", "will")) 
# Remove punctuations
Words <- tm_map(Words, removePunctuation)
# Eliminate extra white spaces
Words <- tm_map(Words, stripWhitespace)

# Text stemming - this removes suffixes so we're working w/ root words
#But stemCompletion isn't working - maybe package error? - so I'm skipping it
#Words.copy<-Words #make a copy to use for stemCompletion
#Words <- tm_map(Words, stemDocument)
#Words <- tm_map(Words, stemCompletion, dictionary=Words.copy)

#Build a word frequency table
dtm <- TermDocumentMatrix(Words)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
Table <- data.frame(word = names(v),freq=v)
head(Table, 200)

##I want parasite/parasites, host/hosts, plant/plants to be one word each
#Here's a clunky solution
Table$freq[Table$word=="parasites"]<-Table$freq[Table$word=="parasites"]+Table$freq[Table$word=="parasite"]
Table$freq[Table$word=="hosts"]<-Table$freq[Table$word=="hosts"]+Table$freq[Table$word=="host"]
Table$freq[Table$word=="plants"]<-Table$freq[Table$word=="plants"]+Table$freq[Table$word=="plant"]
Table$freq[Table$word=="individuals"]<-Table$freq[Table$word=="individuals"]+Table$freq[Table$word=="individual"]
Table$freq[Table$word=="populations"]<-Table$freq[Table$word=="populations"]+Table$freq[Table$word=="population"]
Table$freq[Table$word=="communities"]<-Table$freq[Table$word=="communities"]+Table$freq[Table$word=="community"]
Table$freq[Table$word=="ecosystems"]<-Table$freq[Table$word=="ecosystems"]+Table$freq[Table$word=="ecosystem"]
Table<-Table[Table$word!="parasite",]
Table<-Table[Table$word!="host",]
Table<-Table[Table$word!="plant",]
Table<-Table[Table$word!="individual",]
Table<-Table[Table$word!="population",]
Table<-Table[Table$word!="community",]
Table<-Table[Table$word!="ecosystem",]

#Build our word cloud
numwords<-115
#aspect ratio can only be variable if percent of rotate words = 0
wordcloud(words = Table$word, freq = Table$freq, min.freq = 1, max.words=numwords, random.order=FALSE, rot.per=0, fixed.asp=F, colors=brewer.pal(8, "Dark2"))

#Save the wordcloud to a high res .png
#if you don't make the dimensions wide enough, the words don't all fit 
png("WordCloud115WordsV2.png", width = 7, height= 3, units = "in", res = 1200)
par(mar = c(0, 0, 0, 0))
numwords<-150
wordcloud(words = Table$word, freq = Table$freq, min.freq = 1, max.words=numwords, random.order=FALSE, rot.per=0, fixed.asp=F, colors=brewer.pal(8, "Dark2"))
dev.off()

which(Table$word=="ecosystems") #ecosystems doesn't make it on
