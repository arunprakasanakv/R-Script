x <- c(1:5);y<-x
 par(pch = 22,col ="red")
 par(mfrow = c(2,4))
 opts = c("p","l","o","b","c","s","S","h")
 for(i in 1:length(opts)){
   heading = paste("type=",opts[i])
   plot(x,y,type = "n",main = heading)
   lines(x,y,type = opts[i])}

# dot chart
 dotchart(mtcars$mpg,labels = row.names(mtcars),cex = .7,main = "gas milage for car models",xlab = "Miles per gallon")

# dot chart with colored output 
 x <- mtcars[order(mtcars$mpg),]
  x$cyl <- factor(x$cyl)
  x$color[x$cyl == 4] <-"red"
   x$color[x$cyl == 6] <-"blue"
   x$color[x$cyl == 8] <-"green"
   dotchart(x$mpg,labels = row.names(x),cex = .7,groups = x$cyl,main = "gas milage by cyl",xlab = "miles per gallon",gcolor = "black",color = x$color)

#   pie chart
   pie(slice,label = lbls ,main = "pie chart")

# pie3D
   
   pie3D(slice,label = lbls ,explode =0.1, main = "pie chart")

   # boxplot
   boxplot(mpg~cyl,data=mtcars,main = "car milage",xlab = "number of cylinder", ylab ="gallons")

#violin plot
   library(vioplot)
   x1 <- mtcars$mpg[mtcars$cyl == 4]
   x2 <- mtcars$mpg[mtcars$cyl == 6]
   x3 <- mtcars$mpg[mtcars$cyl == 8]
   vioplot(x1,x2,x3,names = c("4 cyl","6 cyl","8 cyl"),col = "gold")
   title("Violin plots of miles per gallon")

# Bag plot
   library(aplpack)
   attach(mtcars)
   bagplot(wt,mpg,xlab ="Car weight",ylab="miles per gallon",main ="bagplot" )
  #simple histogram
   
   hist(mtcars$mpg)
   
   # histogram with color
   hist(mtcars$mpg,breaks = 12,col ="red")
   
   #density plot
   d <- density(mtcars$mpg)
   plot(d)
   
   #filed density plot
   d <- density(mtcars$mpg)
   plot(d,main="kernal desity")
   polygon(d,col = "red",border = "blue")
   
   #comparing
   library(sm)
   attach(mtcars)
   
   #comparing
   library(car)
   scatterplot(mpg~wt|cyl,data = mtcars,xlab="weight",ylab="miles",main ="enhanced",labels =row.names(mtcars))
   
   
   library("party")
   str(iris)
   iris_ctree <- ctree(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data = iris)
   print(iris_ctree)
   plot(iris_ctree)
   
   #correlation cor.test(x,y)
   attach(mtcars)
   cor.test(mpg,wt)
   # syntax with 
   with(mtcars,cor.test(mpg,wt))
   
   #Simple linear regression
   library(MASS)
   attach(cats)
   View(cats)
   lm(Hwt~Bwt,data=cats)
   lm.out = lm(Hwt~Bwt,data = cats)
   lm.out
   summary(lm.out)
   plot(Hwt~Bwt,data=cats)
   abline(lm.out,col="red")
   par(mfrow=c(1,1))
   plot(lm.out)
   
   #mutiple linear regression
   fit <- lm(clinically.important.brain.injury~age.65+basal.skull.fracture+loss.of.consciousness,data = head_injury)
   summary(fit)
   coefficients(fit)
   
   #Logistic regression
   
   attach(Titanic)
   train(1:1313)
   test(1:100)
   model <- glm(Survived ~ PClass+Age,data = Titanic)
   summary(model)
   plot(model)

#R with twitter
   library(twitteR)
   library(tm)
   library(wordcloud)
   setup_twitter_oauth(consumer_key="DmXz1exphGdeLO0HwFcbLDo2W",consumer_secret="nlyqdj6fcffE2yb1vA0022h1yYZvaSDeln8fBhiabdfUK0ZGUx",access_token="787902154883280896-DEcAyQIIJtxEssA9MyepL37CX6ELJ2O",access_secret="dsYHugfWDR1XdsIiwFyPJV06mer46BbYZwkwiSc7fh7wr")
   mach_tweets = searchTwitter("Donald trump",n=100,lang='en')
   mach_text = sapply(mach_tweets,function(x)x$getText())
   mach_corpus = Corpus(VectorSource(mach_text))
   tdn = TermDocumentMatrix(mach_corpus,control = list(removePunctuation = TRUE,stopwords=c("Donald","trump",stopwords("english")),removeNumbers=TRUE,tolower =TRUE))
   png("donaldtrump.png",width = 12,height = 8,units="in",res = 300)
   wordcloud(tdm$word,tdm$freq,random.order = FALSE,colors = brewer.pal(8,"Dark2"))
   dev.off()
   
