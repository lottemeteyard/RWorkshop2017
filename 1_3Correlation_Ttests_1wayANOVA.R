########################### some basic statistics

################## Correlation ##############
# http://www.statmethods.net/stats/correlations.html

require(foreign)
# Read in SPSS file
dat2<-read.spss("SPSS_Data1.sav")
str(dat2)

dat2<-data.frame(dat2)
str(dat2)

#check normality
shapiro.test(dat2$AGE)
shapiro.test(dat2$HEIGHT)

#eyeball data and get correlation coefficient
plot(dat2$AGE,dat2$HEIGHT)

# note the 'use' to specify what to do with missing data
cor(dat2$AGE, dat2$HEIGHT, method="spearman", use="pairwise.complete.obs")

# Install package to help plot results for multiple correlations
library(corrplot)
# Asking for correlation between first three columns of data
M <- cor(dat2[,c(1:3)],method="spearman",use="pairwise.complete.obs")
M

names(dat2)
rownames(M)<-names(dat2)[1:3]
colnames(M)<-names(dat2)[1:3]
corrplot(M, method = "number")

# set colours
wgb <- c("white","light grey","dark grey","black")
corrplot(M, order="AOE", addCoef.col="grey")
corrplot(M, method="shade", col=wgb,order = "AOE",
         addCoef.col="grey",tl.col="black",diag=FALSE,type="upper")


# Get significance levels for correlations
library(Hmisc)
# for this function we need to use a numeric matrix not a dataframe
temp<-data.matrix(dat2)

#NB: rcorr() uses pairwise deletion for missing values
rcorr(temp[,c(1:3)], type="spearman") 



################## Repeated measures t-test ##############

##### Boxplot paired data

#Let's return to our TMS data set
print(tbl_df(dat1), n = 3)
dat1$RTms <- dat1$RT*1000 #convert RT into ms for analysis

#analyse subset for correct trials only
library(dplyr)
dat1.corr<-filter(dat1, ACC == 1)

# prepare data for paired test

library(reshape2)

levels(dat1.corr$Congruence)

# using reshape to make paired columns
congruence<-dcast(dat1.corr, SubNo ~ Congruence, value.var = "RTms",
                  fun.aggregate = mean, na.rm = TRUE)
# this averages across other conditions for each subject
head(congruence)

# complete a t test for two levels of congruence
with(congruence,t.test(congruence$Cong,congruence$Incong,paired=TRUE))
# uses 'with' to parcel which function you want to apply to what data

# for reference: non-parametric paired test
help(wilcox.test)

boxplot(congruence$Cong, congruence$Incong)



################## Between subject t-test ##############
# http://www.statmethods.net/stats/ttest.html

str(dat1)

#let's compare RT for the two tasks (between subjects manipulation)
t.test(dat1.corr$RTms~dat1.corr$Task) 
#note that it takes the mean across the whole group (i.e. Subject is lost)

#Let's do this properly by averaging across subjects
library(reshape2)
temp<-dcast(dat1.corr, SubNo ~ Task, value.var = "RTms",
                  fun.aggregate = mean, na.rm = TRUE)
Task<-melt(temp,id.vars = c("SubNo"))
#tidy up data and rename
Task<-na.omit(Task)
names(Task)[2:3]<-c("Experiment","RTms")
head(Task)

t.test(Task$RTms~Task$Experiment) 



################## ANOVA #########

# We will compare RT differences for different TMS coil orientations

names(dat1.corr)

# Let's check the data meets assumptions for ANOVA

##### Homogeneity of variance tests ####

# Two tests come in the base package

# (1) Bartlett Test
# checks for k samples having equal variances
# sensitive to departues from normality
# so may just show non-normality
bartlett.test(RTms ~ Axes,data=dat1.corr)

# (2) Figner-Killeen Test of Homogeneity of Variances
# Very robust against departures from normality
# non-parametric test
fligner.test(RTms ~ Axes,data=dat1.corr)

# One familiar from SPSS:
# (3) Levene's test can be found in the package car
# so install car
require(car)
leveneTest(RTms ~ Axes,data=dat1.corr)



##### Normality ####

# plot normality formally
qqnorm(dat1.corr$RTms)
qqline(dat1.corr$RTms)

hist(dat1.corr$RTms) #can see the long tail / skew that is typical for RT data

# shapiro-wilk test
# example adapted from: 
# www.dummies.com/how-to/content/how-to-test-data-normality-in-a-formal-way-in-r.html
# tests null of whether sample comes from normal distribution

# test whole set of data
shapiro.test(dat1.corr$RTms)
# data set is too big, so we can apply the analysis to each condition

# test each cell (i.e. split by factor levels)
with(dat1.corr,tapply(RTms,Axes,shapiro.test))



############# Transform data, remove outliers #########

# Log transform usually good to normalise RTs
par(mfrow=c(1,2))
hist(dat1.corr$RTms)
hist(log(dat1.corr$RTms))

# quantile plots show less deviation
par(mfrow=c(1,2))
qqnorm(dat1.corr$RTms)
qqline(dat1.corr$RTms)
qqnorm(log(dat1.corr$RTms))
qqline(log(dat1.corr$RTms))


# Still have some outliers at the extreme end

#Look for outliers in DV

#Boxplot by Subject for outliers, store results
mybp <- boxplot(RTms ~ SubNo, data=dat1.corr)
#identify outliers row number in original data set
x<-which(dat1.corr$RTms %in% mybp$out, arr.in=TRUE)

#mark outliers in new column
dat1.corr$Out<-rep("FALSE")
for (i in 1:length(x))
{dat1.corr$Out[x[i]]<-"TRUE"}
dat1.corr$Out<-as.factor(dat1.corr$Out)
head(dat1.corr)

#look at number of outliers per subject
table(dat1.corr$SubNo,dat1.corr$Out)
#new data set with outliers removed 
dat1.corr.a<-dat1.corr[dat1.corr$Out=="FALSE",]  
#Calculate percentage of data lost
1-dim(dat1.corr.a)/dim(dat1.corr)


# plot log transform of data with outliers removed
hist(log(dat1.corr.a$RTms))
qqnorm(log(dat1.corr.a$RTms))
qqline(log(dat1.corr.a$RTms))



############# One way ANOVA (between & within subjects) #########

# It's best to store output from tests into variables

# ANOVAs in R can be tricky.
# But, it will make you understand ANOVA better.
#http://myowelt.blogspot.co.uk/2008/05/obtaining-same-anova-results-in-r-as-in.html

# Between-subjects ANOVA is straightforward in R, performing repeated measures (within-subjects)
# ANOVA is not so obvious. 
#  e.g https://blog.gribblelab.org/2009/03/09/repeated-measures-anova-using-r/

# ANOVA is GLM / linear models with factorial IVs only, so in R it is coded like a regression
# hence the formuala entry format

#We can start with a linear model 
dat1.lm <-lm(log(RTms) ~ Axes, data=dat1.corr.a)

summary(dat1.lm)
#note that R has used a reference level (E.W. - usually decided by alphabetic order)
# so the intercept is the mean value for E.W. and the other model esimates
# are the difference between other conditions and E.W.

# We can then do an ANOVA on this linear model NB: this is treating the 
# conditions as between subjects (will come back to this below)
anova(dat1.lm)

#eye ball the means
with(dat1.corr.a, tapply(log(RTms), Axes, mean))


# To do ANOVA correctly for repeated measures, we tell R to include a within subjects error term
# for each condition. This reflects that we have conditions nested within subjects
# That is, in principle, we can see the condition effect in every subject

# We will now use the aov function
help(aov)  # read through this help file, note the warnings about unbalanced data

aov.1 <-aov(log(RTms) ~ Axes + Error(SubNo/Axes), dat1.corr.a)
summary(aov.1)

#"SubNo" and "Axes" are our sources of variability. 
#The treatment we are interested in is "Axes" (that's what we want to see the effect of), 
#and this treatment effect is visible within each subject (i.e., nested within each subject). 

#The proper Error term is "SubNo/Axes", which is read as "Axes within subjects" 


############# Post-hoc pairwise tests (within & between subjects) #########


#pairwise t tests for post-hoc comparisons
pairwise.t.test(log(dat1.corr.a$RTms), dat1.corr.a$Axes, p.adjust.method="none", paired=T)
# we get an error because of missing data across different conditions

#let's generate summary data for the post-hoc tests
library(reshape2)

temp<-dcast(dat1.corr.a, SubNo ~ Axes, value.var = "RTms",
               fun.aggregate = mean, na.rm = TRUE)
temp
ph.test<-melt(temp,id.vars = "SubNo")
head(ph.test)

# try again with out post-hoc comparisons with balanced data
names(ph.test)
pairwise.t.test(log(ph.test$value), ph.test$variable, p.adjust.method="none", paired=T)
# the output is the p value

help(p.adjust)
pairwise.t.test(log(ph.test$value), ph.test$variable, p.adjust.method="bonferroni", paired=T)


# The package 'multcomp' has other methods for multiple comparisons
# nice summary here: www.pitt.edu/~njc23/Lecture10.pdf
require(multcomp)

# TukeyHSD for bewteen subjects (pretending Axes is between subjects here)
aov(dat1.lm)
glht(aov(dat1.lm), linfct = mcp(Axes = "Tukey"))

#you can also specify the contrast matrix directly
levels(dat1.corr.a$Axes) #note the order of output for labels
contr <- rbind("E.W - N.S" = c(-1, 1, 0, 0),
               "NE.SW - NW.SE" = c(0, 0, 1, -1), 
               "NW.SE - NE.SW" = c(0, 0, -1, 1))
glht(aov(dat1.lm), linfct = mcp(Axes = contr))

#to get significance, confidence internvals etc.
temp<-glht(aov(dat1.lm), linfct = mcp(Axes = contr))
summary(temp)
confint(temp)



################# Saving work #################

# Save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")







