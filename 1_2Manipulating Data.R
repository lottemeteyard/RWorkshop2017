# R teaching Workshops.
# Lotte Meteyard, 2016, University of Reading
# School of Psychology and Clinical Language Sciences

######################################################################
# Once data is in R, most of your time will be spent getting it tidy
# and doing sanity checks

# you will get used to never looking at raw data spreadsheets

# NB: there are usually several ways to do the same thing in R

################## basic descriptives using pastecs ##########################

install.packages("pastecs")
require(pastecs)
stat.desc(data, basic=F)



################## plyr/dplyr ##########################

#We will look at two helpful packages for this
plyr/dplyr
reshape2

# install plyr and dplyr
library(plyr)
library(dplyr)

help(ddply)

# Remind ourselves what our data looks like
str(dat1)

#Look at RT data (is in seconds not ms)
summary(dat1$RT)

#look at how many scalp locations
#coded across both hemispheres
levels(dat1$HomLocation)

# Summarise some data (mean RT for each location collapsed across hemispheres)
# At the same time we are going to convert RT so ms not seconds (i.e. x1000)
data.summary <- ddply(dat1, .(HomLocation), summarise,
                       N    = length(RT),
                       mean = mean(RT*1000),
                       sd   = sd(RT*1000),
                       se   = sd / sqrt(N) )
head(data.summary, n=10)
# it calclates N depending on the grouping factor


# Summarise mean RT for each location x axis
# the function ddply returns a data frame
data.summary <- ddply(dat1, .(HomLocation, Axes), summarise,
                      N    = length(RT),
                      mean = mean(RT*1000),
                      sd   = sd(RT*1000),
                      se   = sd / sqrt(N) )
head(data.summary)

# the function daply returns an array
# how many rows of data do I have for each Axes?
daply(dat1, .(Axes), nrow)


#some further methods for exploring and summarising data
#See http://genomicsclass.github.io/book/pages/dplyr_tutorial.html


#show me first 3 rows of dataset
#nice because it only displays what fits onscreen
print(tbl_df(dat1), n = 3)

#filter()  filter rows
#give me data with RTs above 0.4s only and see how many trials I lose
summary<-filter(dat1, dat1$RT > 0.4)
nrow(dat1)-nrow(summary)

#Multiple criteria: select data for the right hemisphere, congruent conditions, Vertex/CZ
# Right hemisphere is coded as 1
summary<-filter(dat1, HomLocation == "CZ", Hemisphere == 1, Congruence == "Cong")
dim(summary)
head(summary)
tail(summary)

#select()  select columns
names(dat1)
summary<-select(dat1, c(SubNo:RT,Axes))
head(summary)

#arrange()  re-order or arrange rows
#desc puts values in descending order
#arrange data by subject (last to first) and RT (highest to lowest x subject)
summary<-arrange(dat1, desc(SubNo), desc(RT))
#compare the two 
print(tbl_df(dat1), n = 3)
print(tbl_df(summary), n = 3)

#mutate()	create new columns
summary<-mutate(summary, RTms = RT*1000)
head(summary)



#group_by()	allows for group operations in the "split-apply-combine" concept
#gives a different way of using summarise to calculate statistics for a group

#going to use the pipe operator here so we can read code left to right

summary <- dat1 %>% group_by(SubNo) %>%
  summarise(meanRT = mean(RT*1000), 
            maxRT = max(RT*1000), 
            minRT = min(RT*1000),
            total = n())
summary





################## reshape2 ##########################

#install rehsape2
library(reshape2)

# for a tutorial see http://seananderson.ca/2013/10/19/reshape.html

#the main functions in reshape2 are melt() and cast()

#in R we want data in long format, i.e. a row for every data point with 
#multiple columns that identify/code values for each data point in the rows 
#wide format data has a column for each variable and data in the cells

#melt() takes wide-format data and melts it into long-format data.

#look at the last set of summary data we created
summary
#let's melt it into long format
summary.long<-melt(summary)
#because we didn't specify id variables, R tells us what it is using by default
head(summary.long)
tail(summary.long)
#'variable' identifies the row for 'value' (the numeric data)

#let's melt summary into long format but keep SubNo and total as columns/id variables
summary.long<-melt(summary,id.vars = c("SubNo", "total"))
head(summary.long)

#cast() takes long-format data and casts it into wide-format data.

#let's work with a bigger data set here
#first let's select some data columns to work with
names(dat1)
congruence<-select(dat1, c(SubNo,Congruence,RT))
head(congruence,n=10)

#cast into wide format with a column for congruent/incongruent stimuli
#we will use dcast since we are working with a dataframe
congruence.wide<-dcast(congruence, SubNo ~ Congruence, value.var = "RT")
congruence.wide
#because it is trying to group by Subject Number, it has given us a count 
#of rows per subject/congruence condition (i.e. the default to length warning)

#we need to tell it how to group data by Subject Number
congruence.wide<-dcast(congruence, SubNo ~ Congruence, value.var = "RT", fun.aggregate = mean, 
na.rm = TRUE)
congruence.wide
#now we have mean RT by congruence condition by Subject


#let's repeat this with more variables
names(dat1)
congruence<-select(dat1, c(SubNo,Congruence,Axes,RT))
head(congruence,n=10)
congruence.wide<-dcast(congruence, SubNo ~ Congruence + Axes, value.var = "RT", fun.aggregate = mean, 
                       na.rm = TRUE)
#now we have mean RT by congruence and axes by Subject
congruence.wide

#we can also keep Axes and SubNo by rows...
congruence.wide<-dcast(congruence, SubNo + Axes ~ Congruence, value.var = "RT", fun.aggregate = mean, 
                       na.rm = TRUE)

#this can be useful if you are doing an analysis that requires wide format data (e.g. paired test)
#or you want summaries of data
#but usually in R we work with long format


#Finally, let's recover our old data for summary
names(summary.long)
summary2<-dcast(summary.long,SubNo + total ~ variable, value.var="value")
#inspect both
summary2
summary




################## tidying up names/cells/spaces/mislabelling ##########################


####### relabelling / recoding

library(plyr)

str(dat1$Hemisphere)
unique(dat1$Hemisphere)
# Lets relabel Hemisphere with left and right
# 1 = left
# 2 = right

#Change coding to a factor or text (otherwise revalue gets upset)
dat1$Hemisphere<-as.factor(dat1$Hemisphere)

dat1$Hemlabel<-revalue(dat1$Hemisphere, c("1" = "Left", "2" = "Right"))
head(dat1)
unique(dat1$Hemlabel)




####  Matching and replacement for cell values

dat2<-read.spss("SPSS_Data1.sav")

# We can use grep to find things, which works on text/character strings
help(grep)

# Example
txt<-c("arm","foot","foobar")
grep("foo",txt)   # find cells with "foo" text

# Changing text in cells
head(dat2)
levels(dat2$HAIRCOL)

# change "other" to "nohair"
x<-as.character(dat2$HAIRCOL) 
grep("other",x)  # Find cells with "other" as content

# Two alternatives that do the same thing
x[grep("other",x)]<-"nohair"
# or
x[x=="other"]<-"nohair"
x

dat2$HAIRCOL<-as.factor(x)  #put the data back into your larger data set



##### Get rid of spaces in cells with content/labels (common with excel imports)

# Install the gdata package, we'll use the 'trim' function
library(gdata)
help(trim)

# Lets make some data with annoying spaces

x<-c(" hair", "hair", " nohair", " hair", "hair ", " nohair", "nohair", "hair ", " hair", " nohair", "nohair ")
unique(x)

x<-trim(x)
unique(x)


################## The importance of NAs ##########################

# NAs are REALLY important in R. Most or all functions have easy options
# to manage NAs in your data, but it must be coded with NAs, not blank cells etc.

# More messy data
x<-c(" hair", "hair", " nohair", " ", "hair ", " ", "nohair", "hair ", "XXX", " nohair", "CCC")
y<-sample(1:20, 11)
x<-data.frame(cbind(x,y))
names(x)<-c("HairCut","Biscuits")
x


# check which cells are empty (convert to text before doing this if necessary)
which(x$HairCut==" ")

# Replace duff data rows with NAs, note the use of the same code above
x$HairCut[which(x$HairCut==" ")]<-NA  

x$HairCut[which(x$HairCut=="XXX")]<-NA 
x$HairCut[which(x$HairCut=="CCC")]<-NA 
x

which(x==" ")  # check again

# Recreate data 
x<-c(" hair", "hair", " nohair", " ", "hair ", " ", "nohair", "hair ", "XXX", " nohair", "CCC")
y<-sample(1:20, 11)
x<-data.frame(cbind(x,y))
names(x)<-c("HairCut","Biscuits")
x

# Check manually for NA (empty) cells
is.na(x)    # check whether all cells are NA or not (true/false)

# Replace whole row with NA if one value missing
x[which(x$HairCut==" "),]<-NA  
is.na(x)
x

# Check a specific column
is.na(x$HairCut) 





################# Saving work #################

# Save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")



