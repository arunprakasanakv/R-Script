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
