##Introduction to ggplot2
#it uses layering to do the plotting
library(ggplot2)

dia<-diamonds
?diamonds
##First visualise the cut variable in the diamonds dataset using a barchart.
ggplot(diamonds, aes(x=cut))+geom_bar()

#Yse the BOD dataset for a 2D set
?BOD
ggplot(BOD,aes(x=Time,y=demand))+geom_bar(stat="identity")
##Notice that there is gap at 6. This is because the time variable is continuous and not categorical.
str(BOD)
class(BOD$Time)
#class of time is numeric we need it to be a factor. so we use as.factor()

ggplot(BOD,aes(x=as.factor(Time),y=demand))+geom_bar(stat="identity",col = "red", fill = "blue")+xlab("Time")+ylab("Demand")

### Splitting the bars together or plitting etc.

ggplot(diamonds,aes(x=cut,fill = clarity))+geom_bar()
# Split the bar chart into different categories

#split the bar chart to multiple bar charts
ggplot(diamonds,aes(x=cut,fill = clarity))+geom_bar(col = "black",position = "dodge")


#if we want to keep a distance between two x's on the graph.
ggplot(diamonds,aes(x=cut,fill = clarity))+geom_bar(col = "black",position = position_dodge(1))


## Use our own colours to plot the chaarts
library(gcookbook)
?uspopchange
library(dplyr)
data<-arrange(uspopchange,desc(Change))
data[1:10,]

ggplot(uspopchange,aes(x=Region))+geom_bar()


##sir solution
bar_chart<-uspopchange%>%
  arrange(desc(Change))%>%
  slice(1:10)

#Create a bar chart: X axis = state, height of each bar = population change

ggplot(bar_chart,aes(x=reorder(Abb,Change),y=Change,fill=Region))+geom_bar(stat="Identity")
##To sort it by change we need to give the command reorder(Abb,change))


#Suppose we want to colour to change as per the region of colour. Since it is a variable we put it inside the aesthetic rather than in geom bar
#manually change the colour of the bars
ggplot(bar_chart,aes(x=reorder(Abb,Change),y=Change,fill=Region))+geom_bar(stat="Identity")+scale_fill_manual(values=c("Orange","yellow4"))


#Suppose you want to create a horzontal bar chart
ggplot(bar_chart,aes(x=reorder(Abb,Change),y=Change,fill=Region))+geom_bar(stat="Identity")+scale_fill_manual(values=c("Orange","yellow4"))+coord_flip()


#Changing order of statement in the code
ggplot(bar_chart,aes(x=reorder(Abb,Change),y=Change,fill=Region))+scale_fill_manual(values=c("Orange","yellow4"))+geom_bar(stat="Identity")+coord_flip()



###Create a dot plot and then we will have the break!!!!!!!
  ###Oh we had the break before itself


####BAseball dataset
library(gcookbook)
?tophitters2001
#we are focusing on the name of the player and the batting average of the player. We are creating a dot plot
### X axis = average score, Y axis = name of the player

data <- tophitters2001%>%
  select(name,avg)%>%
  slice(1:25)
data
ggplot(data,aes(x=avg,y=reorder(name,avg)))+geom_point()
