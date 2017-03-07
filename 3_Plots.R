# R teaching Workshops.
# Lotte Meteyard, 2016, University of Reading
# School of Psychology and Clinical Language Sciences

######################################################################
#Plots in R take a bit of getting used to
#but they are wonderful

# We will use ggplot because it is FABULOUS

library(ggplot2)


###### getting fitted values from a linear model ######

lm.1<-lm(RTms ~ TwitchFactor, data=dat1.corr.a)

# extract fitted values from model
a<-fitted.values(lm.1)
length(a)
# or use predict() function
y<-predict(lm.1)

z<-na.omit(dat1.corr.a) #remove missing values from original data set for plotting
dim(z)

#put fitted values alongside original data
z$fitted<-a
#or
z$fitted<-predict(lm.1)


###### plot RT by twitch factor strength #####

# ggplot builds layers

library(ggplot2)
plot <- ggplot(z,aes(x=TwitchFactor, y=fitted))
# make a line plot
plot <- plot + geom_bar(stat="identity")
# add axis labels
plot<-plot + xlab("Twitch Strength")
plot<-plot + ylab("RT (ms)")
# add title
plot<-plot + ggtitle("RT by Twitch Strength (fitted values)")
plot


###### using Effects and ggplot together #####

library(effects)

lm.1<-lm(RTms ~ Task + Congruence, data=dat1.corr.a)
eff<-allEffects(lm.1)

plot1<-eff$Congruence 
data.summary<-plot1$x
data.summary[,2]<-plot1$fit
data.summary[,3]<-plot1$lower
data.summary[,4]<-plot1$upper
data.summary[,5]<-plot1$se
names(data.summary)[2:5]<-c("fit","lower","upper","se")
data.summary

#make sure we have all the labels ready for plotting
data.summary$Congruence<-c("Congruent","Incongruent")
data.summary

# set up data
plot <- ggplot(data.summary,aes(x=as.factor(Congruence),y=fit))
plot <- plot + geom_bar(position=position_dodge(),stat="identity")
# add error bars with standard error
plot <- plot + geom_errorbar(aes(ymin=fit-se,ymax=fit+se),width=.2)
# add axis labels
plot<-plot + xlab("Flanker Congruence")
plot<-plot + ylab("RT (ms)")
# add title
plot<-plot + ggtitle("Effect of Congruence on RT difference")
# use theme
plot<-plot + theme_bw()
plot



###### Summarising data for ggplot #####

library(plyr)
# Extract subject data 
data.summary1 <- ddply(dat1.corr.a, c("SubNo", "Task", "Congruence"), summarise,
                       N    = length(RTms),
                       SubjRT = mean(RTms),
                       SD = sd(RTms)
                       #StErr = sqrt(var(RTms)/length(levels(as.factor(dat1.corr.a$SubNo))))
)

#check number of subjects for Standard error calculation
length(levels(as.factor(dat1.corr.a$SubNo)))
#or
length(unique(dat1.corr.a$SubNo))

data.summary <- ddply(data.summary1, c("Task", "Congruence"), summarise,
              Mean = mean(SubjRT),
              StErr = mean(SubjRT)/sqrt(20))


# set up data
plot <- ggplot(data.summary,aes(x=as.factor(Task),y=Mean, fill=Congruence))
dodge <- position_dodge(width=0.9)
plot <- plot + geom_bar(position=dodge,stat="identity",aes(fill=Congruence))
# add error bars with standard error/CI
plot <- plot + geom_errorbar(position=dodge,aes(ymin=Mean-(StErr*1.96),ymax=Mean+(StErr*1.96),width=.2))
# add axis labels
plot<-plot + xlab("Task")
plot<-plot + ylab("Reaction Time (ms)")
# add title
plot<-plot + ggtitle("RT by Task and Congruence")
plot

# use theme and colours to make grey scale
plot<-plot + theme_bw()
plot + scale_fill_grey()


#change axis (focus view)
plot <-  plot + coord_cartesian(ylim=c(200, 700))



######## Plot twitch strength x task ########

#ggplot will apply its own modelling to graph the data...
#see: http://docs.ggplot2.org/0.9.3.1/geom_smooth.html


library(plyr)
# Extract subject data for twitch rating x task
data.summary1 <- ddply(dat1.corr.a, c("SubNo", "Task", "Twitches"), summarise,
                       N    = length(RTms),
                       mean = mean(RTms),
                       SD = sd(RTms)
                       #StErr = sqrt(var(RT)/length(levels(dat_FLankera$SubNo)))
)
names(data.summary1)[5]<-"RT"
head(data.summary1,n=10)

# set up data
plot <- ggplot(data.summary1,aes(x=Twitches,y=RT))
# plot separate panels for Task
plot <- plot + facet_grid(~Task)
#method=loess :local polynomials, will show non-linearities if there
#otherwise, can use method=lm
plot <- plot + stat_smooth(method=lm, fill="light grey") #+ geom_point()
# add axis labels
plot<-plot + xlab("Twitch intensity rating")
plot<-plot + ylab("RTms")
# add title
plot<-plot + ggtitle("Effect of Twitches on RT by Task")




###### Other example plots for data checking #####

#plot individual subject RTs Diffs against qqplot to look at normality
require(languageR)
require(lattice)
qqmath(~RTms | SubNo, data = dat1)






