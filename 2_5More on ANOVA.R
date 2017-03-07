# R teaching Workshops.
# Lotte Meteyard, 2014, University of Reading
# School of Psychology and Clinical Language Sciences


############# Mixed ANOVA #########


# Let's ratchet up the complexity for the ANOVA model

#Let's return to our TMS data set
library(dplyr)
print(tbl_df(dat1.corr.a), n = 3)

names(dat1.corr.a)
# Between: Task, Hemlabel (both were counterbalanced)
# Within: Axes, Twitches
# DV: RTms

# Fit a between subjects ANOVA model with Task and Hemisphere only
# As in SPSS we are going to look at main effects AND interactions
aov.1 <- aov(log(RTms) ~ (Task*Hemlabel), data=dat1.corr.a)
summary(aov.1)
# Print condition means or effects
model.tables(aov.1,"means") 
model.tables(aov.1,"effects")
# rep is the number of data points per cell
dim(dat1.corr.a)

# plot interactions for two factors from the above model
with(dat1.corr.a, interaction.plot(Task, Hemlabel, log(RTms), fun = mean))


# Fit a within subjects ANOVA with Axes and Twitches only ()
# Going to cheat here and first recode Twitches as a binary variable low/high
library(plyr)
dat1.corr.a$TwitchFactor<-revalue(as.character(dat1.corr.a$Twitches), 
                                  c("0" = "Low",
                                    "1" = "Low", "2" = "Low",
                                    "3" = "Low", "4" = "Low",
                                    "5" = "Low", "6" = "High",
                                    "7" = "High", "8" = "High",
                                    "9" = "High", "10" = "High"))
str(dat1.corr.a$TwitchFactor)
dat1.corr.a$TwitchFactor<-as.factor(dat1.corr.a$TwitchFactor)
str(dat1.corr.a$TwitchFactor)

aov.2 <- aov(log(RTms) ~ (Axes*TwitchFactor) + Error(SubNo/(Axes*Twitches)),data=dat1.corr.a)
summary(aov.2)
# plot interactions for two factors from the above model
with(dat1.corr.a, interaction.plot(Axes, TwitchFactor, log(RTms), fun = mean))



# Extend the model to be a mixed ANOVA with all IVs.

aov.3 <- aov(log(RTms) ~ (Axes*TwitchFactor*Task*Hemlabel)
             + Error(SubNo/(Axes*TwitchFactor)), data=dat1.corr.a)
#you must segregate between- and within-subjects variables
#above within-subjects factors are first.



#######################################################################
################## Important notes on ANOVA in R #########

# a.k.a all the things you never knew about ANOVA

# Did you know that the specific hypothese you test in ANOVA
# should affect the way the ANOVA is done?

# Reference reading:
# goanna.cs.rmit.edu.au/~fscholer/anova.php
# www.r-statistics.com/tag/unbalanced-design/
# www.personal.psu.edu/mar36/stat_461/unbalanced/unbalanced_two_factor_ANOVA.html
# sites.psu.edu/stat461psbsp2013/wp-content/uploads/sites/1906/2013/03/InteractionsAndTypesOfSS.pdf

# A quote:
# "You must realize that R is written by experts in statistics and statistical computing who, 
# despite popular opinion, do not believe that everything in SAS and SPSS is worth copying. 
# Some things done in such packages... trace their roots back to the days of punched cards 
# and magnetic tape, when fitting a single linear model may take several days because 
# your first 5 attempts failed due to syntax errors in the JCL or the SAS code. [So it] 
# reflects the approach of “give me every possible statistic that could be calculated 
# from this model, whether or not it makes sense". The approach taken in R is different. 
# The underlying assumption is that the useR is thinking about the analysis while doing it."

# Douglas Bates (in reply to the suggestion to include type III sums of squares and lsmeans in base R to make it more similar to SAS or SPSS), R-help (March 2007)
# From www.personal.psu.edu/mar36/stat_461/unbalanced/unbalanced_two_factor_ANOVA.html



###### IMPORTANT for unbalanced designs #########
# (and therefore, missing values for repeated measures) 

# The default anova in R (aov) reports Type I sums of squares (SS)
# These are sequential SS. 
# SPSS uses Type III SS.
# I don't know what software typically implements Type II

# When data is balanced, factors are orthogonal, and types I, II and III SS 
# all give the same results.


#######         BUT..

# Type I SS tests for a difference in the weighted marginal means.
# Weighted means assume the proportion of each factor level in the sample is equal to the proportion in the population. 
# i.e. values for means are weighted more heavily for higher n in the cell. 
# Unweighted means assume the proportion is the same for each level of the factor in the population


# If you have an UNBALANCED design (i.e. different n in each cell)
# the order you put things in the aov equation will matter
# The results are dependent on the realized sample sizes, 
# namely the proportions in the particular data set. 
# In other words, it is testing the first factor without controlling for the other factors

# great explanation here
# stat.ethz.ch/pipermail/r-help/2006-August/111854.html



# Lets work through an example
# From www.personal.psu.edu/mar36/stat_461/unbalanced/unbalanced_two_factor_ANOVA.html


####### weighted means example #######

unb<-read.csv("unbalanced.csv")
str(unb)
attach(unb)
# Data is rate of bone growth affected by hormone administration
# bone development is mild, moderate or severe
# plot the data
with(unb, interaction.plot(gender, boneDev, growth, fun = mean))

table(gender,boneDev)  #Frequency table to show cells are unbalanced

# individual cell means
with(unb, tapply(growth, boneDev:gender, mean))

# Weighted mean calculation using tapply for levels of bone development
# It weights towards female scores when F > M, and vice versa
with(unb, tapply(growth, boneDev, mean))

# Compare to unweighted means for Med level of boneDev
mean(2.1, 1.9)



####### implemented in aov (Type I SS)

# put to the test in R

# Test boneDev alone, and then gender after boneDev
aov.1 <- aov(growth ~ boneDev * gender, data = unb)
summary(aov.1)

# Test gender alone, and then boneDev after gender
aov.2 <- aov(growth ~ gender * boneDev, data = unb)
summary(aov.2)

# we see different results depending on the order of entry
 
# NB: DO NOT USE aov WITH UNBALANCED DATA



####### Type II and Type III SS

# Type II is best used when there is no interaction, 
# it tests each factor after the others, but ignores any interactions
# that is, it assumes any higher order interactions are zero
# see goanna.cs.rmit.edu.au/~fscholer/anova.php

# Type III SS takes into account all other factors and interactions
# This is, it gives you the UNIQUE variance for each effect/interaction
# This approach is valid in the presence of significant interactions.
# NB: in the presence of interactions, main effects are rarely interpretable
# When interactions are not present, Type II SS is a stronger test (see refs above)

# install the car package
require(car)
help(Anova)
# this demands a very explicit modelling procedure
# you explicitly state the contrasts for each factor
mod <- lm(growth ~ gender * boneDev, data = unb, contrasts = list(gender = contr.sum, boneDev = contr.sum))
# then an ANOVA test on that model

# Test each main effect (gender, boneDev), assume interaction is zero
Anova(mod, type = 2)

# Test all factors & interactions
Anova(mod, type = 3)

# You can see that boneDev is significant in both
# But more so in Type II 

# So, for unbalanced designs (e.g. missing values in repeated measures)
# it may be sensible to use Type III
# see onlinestatbook.com/2/analysis_of_variance/unequal.html
# for a more detailed discussion of when to use Type II


###### Multiple comparisons, contrasts and contrast coding #######

# For a good introduction to this, and from which examples below were taken
# please see: Crawley, M.J. (2005) Statistics: an introduction using R

# In the above model the contrasts were set up with contr.sum
# contrasts = single df comparisons (Crawley, 2005, Ch.7 excercises, p35-36)
# "There are two important points to understand about contrasts:
# • there are absolutely loads of possible contrasts
# • there are only k-1 orthogonal (statistically independent) contrasts..

#  Rules for constructing contrast coefficients are straightforward:
#  • treatments to be lumped together get like sign (plus or minus)
#  • groups of means to be to be contrasted get opposite sign
#  • factor levels to be excluded get a contrast coefficient of 0
#  • the contrast coefficients, c, must add up to 0"

load("ContrastData.RData")
# Computer experiment with 5 conditions and a response variable
# One condition acts as a control here.

attach(compexpt)
names(compexpt)
levels(Condition)

plot(Condition,Resp)
# Treatment conditions all higher than control condition
# confirm with values
tapply(Resp,Condition,mean)

# ombnibus ANOVA
mod<-aov(Resp~Condition)
summary(mod)


# Treatment contrasts: default in R
# compares conditions to a control NB: control = first alphabetically
options(contrasts=c("contr.treatment","contr.poly")) 
mod<-lm(Resp~Condition)
summary(mod)

# First coefficient is the mean of the factor level that comes first in 
# alphabetical order (Control mean in this example). 
# The remaining parameters are all differences between means 
# (the mean in question compared with the control mean). 
# So here we see the increase in Resp for each condition > control


# Orthogonal (Helmert) contrasts - these are successive / order matters
options(contrasts=c("contr.helmert","contr.poly"))
mod2<-lm(Resp~Condition)
summary(mod2)

# Intercept = overall mean
mean(tapply(Resp,Condition,mean))
# Successive estimates reflect the difference between average of means
# to average of successive means (e.g. Condition 2 estimate = difference betweeen
# average of first 3 conditions and average of first 2 conditions)


# Sum contrasts (contr.sum): compares conditions to grand mean
options(contrasts=c("contr.sum","contr.poly")) 
mod3<-lm(Resp~Condition)

# Example code for more complex ANOVA with specified sum contrasts
mod <- lm(DV ~ IV1 * IV2, data = dataobject, 
          contrasts = list(IV1 = contr.sum, IV2 = contr.sum))



# It is also possible to specify your own contrasts 
# (i.e. user defined contrasts)

# Create a column that codes the contrast you want
c1<-factor(1+(Condition=="control"))
c1
# We use the factor function to code one number for Control and 
# a different number for all other conditions

# This is then input as the basis for the ANOVA model
mod<-aov(Resp~c1)

## see also the 'contrasts' package for more options with lm fits
# and www.ats.ucla.edu/stat/r/modules/dummy_vars.htm


