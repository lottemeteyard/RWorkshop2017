# R teaching Workshops.
# Lotte Meteyard, 2016, University of Reading

#Thanks to Rob Davies at Lancaster Uni, UK
#http://www.lancaster.ac.uk/psychology/contact-and-getting-here/people/robert-davies

#######################################################################

############# Different options for mixed models n R

# see freshbiostats.wordpress.com/2013/07/28/mixed-models-in-r-lme4-nlme-both/

# Nice site to explain basic concepts
# onlinecourses.science.psu.edu/stat504/node/157


########################## Mixed models: lme4 & lmer ##########################
# We will use lme4, as it is generally better supported now
# and more widely used

# See
https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf

# install packages 
# We'll use two here, to help us interpret what we get
# lme4 does not come with significance tests
# or support the 'step' function

#Install lme4 and lmerTest

# lmerTest gives p values/significance for fixed effects
require(lmerTest)
require(lme4)


########################## Building a random intercepts LMM ##########################

# we are going to start with our data that has outliers removed already
# see 1_3Correlation_Ttests_1wayANOVA.R script for details
# this is something that would be done for regression/ANOVA to also applies to LMMs
str(dat1.corr.a)

# Let's do a bit of tidying up and make sure we have everything coded properly

# Change SubNo and ACC to factors/categories 
dat1.corr.a$SubNo<-as.factor(dat1.corr.a$SubNo)
dat1.corr.a$ACC<-as.factor(dat1.corr.a$ACC)
str(dat1.corr.a)

# Start with a model from the regression analysis yesterday

lm.1 <- lm(log(RTms) ~ Trial + Twitches + (Task*Congruence), data=dat1.corr.a)
summary(lm.1)


# modified to add random intercepts for subjects 
# now we use lmer()

lmer.1<-lmer(log(RTms) ~ Trial + Twitches + (Task * Congruence)
             + (1|SubNo), data = dat1.corr.a)
            
#this notation tells R to allow intercepts to vary for Subjects 
# if you wanted to add random intercepts for items/words it would be + (1|Items)

summary(lmer.1)
summary(lm.1)

# what has changed?


# Direct comparison: can't use anova anymore, use AIC or BIC instead
# the lower the score the better
AIC(lm.1,lmer.1)
BIC(lm.1,lmer.1)
# we have added in a parameter (the random effect) and this
# has made the model much better



# Diagnostic plot of residuals
plot(lmer.1)
# This looks OK, with some reduced spread at lower values



########################## Looking at random effects ##########################

# Let's check that random effects are significant (i.e. doing work)
help(rand)
# does the random effect make the model better / data more likely?
rand(lmer.1)

# to extract the random effects
x<-ranef(lmer.1)  # extract random effects
str(x)
# Plot random effects from model 
library(lattice)
dotplot(ranef(lmer.1, whichel = "SubNo", condVar = TRUE))
# now you can see the variation across subjects for log RT


########################## Looking at fixed effects / IVs ##########################

# to plot fixed effects lets make a new DV columns for log RT
dat1.corr.a$logRT<-log(dat1.corr.a$RTms)

lmer.1<-lmer(logRT ~ Trial + Twitches + (Task * Congruence)
             + (1|SubNo), data = dat1.corr.a)

#plot fixed effects & interactions
library(effects)
eff<-allEffects(lmer.1)
plot(allEffects(lmer.1))

#plot interaction of Congruence and Task
plot(effect(term="Task*Congruence",mod=lmer.1),ylab="logRT",xlab=" ",ci.style="lines")

# not so helpful to see, so let's re-plot 

# extract fitted values and put alongside data
# NB remove NAs from data first
temp<-na.omit(dat1.corr.a)
temp$fit<-fitted.values(lmer.1)

# attach temp so that I can call the columns directly
attach(temp)
#plot fitted values to look at interaction
interaction.plot(Congruence, Task, fit, fixed = TRUE)
detach(temp)


########################## Random intercepts & slopes ##########################

# modified to add random slopes
# This will allow the effect of Twitch strength to vary across subjects
plot(effect(term="Twitches",mod=lmer.1),ylab="logRT",xlab="Twitch strength",ci.style="lines")


# add in notation for Twitches slope to vary across subjects
# the 1+ means that we think the intercepts and slopes will be correlated

lmer.2<-lmer(log(RTms) ~ Trial + Twitches + (Task * Congruence)
             + (1 + Twitches|SubNo), data = dat1.corr.a)

summary(lmer.2)

AIC(lmer.1,lmer.2)
# this has improved our model a bit (AIC is more negative)

# likelihood ratio test (needs to fit with ML)
anova(lmer.1,lmer.2)

#simple plot of random effects (left panel is intercept and right is slope for Twitches)
dotplot(ranef(lmer.2, condVar = TRUE))

# see how fit for Twitches varies over Subjects
# http://www.statmethods.net/advgraphs/trellis.html
require(lattice)
lattice::xyplot(fitted(lmer.2) ~ Twitches|SubNo, data=dat1.corr.a, 
                main="Twitches by Subject Random Effects",ylab="Fitted log RT",xlab="Twitch strength")

# look at original data
lattice::xyplot(logRT ~ Twitches|SubNo, data=dat1.corr.a, 
                main="Twitches by Subject Random Effects",ylab="Fitted log RT",xlab="Twitch strength")



# "By default, lme4 assumes that all coefficients associated with the same random-effects term
# are correlated" (p. 7; Bates lme4 document in workshop folder)

# If you don't want intercepts and slopes will be correlated"
# use 0

lmer.3<-lmer(log(RTms) ~ Trial + Twitches + (Task * Congruence)
             + (0 + Twitches|SubNo), data = dat1.corr.a)


AIC(lmer.2, lmer.3)
# For this data, we need to include the correlation to best fit the data


######### Model building simple to complex & comparison with pbkrtest #########
#Thanks to Rob Davies at Lancaster Uni, UK
#http://www.lancaster.ac.uk/psychology/contact-and-getting-here/people/robert-davies

# to reduce collinearity and help interpretation, consider standardizing numeric predictors
# use scale function (creates z score of variables)
# Exampledata$zIV1 <- scale(Exampledata$IV1, scale = TRUE, center = TRUE)

# start with an intercept only model with random effects
# fit with maximum likelihood

lmer.0<-lmer(log(RTms) ~ (1|SubNo), data = dat1.corr.a, REML = FALSE)
summary(lmer.0)

# add predictors of trial and block

lmer.1<-lmer(log(RTms) ~ Block + Trial + (1|SubNo), data = dat1.corr.a, REML = FALSE)
summary(lmer.1)

# add predictors Task and Congruence

lmer.2<-lmer(log(RTms) ~ Block + Trial + Task + Congruence
             + (1|SubNo), data = dat1.corr.a, REML = FALSE)
summary(lmer.2)

# compare model fits:

BIC(lmer.0, lmer.1, lmer.2)
AIC(lmer.0, lmer.1, lmer.2)


# we can also use the likelihood ratio test comparison:-

anova(lmer.0, lmer.1)      
anova(lmer.1, lmer.2)	


# get confidence intervals for effects estimates for last model

summary(lmer.2)
confint(lmer.2, method = "Wald")  


# To do model comparisons with bootstrap methods
# PBmodcomp(largeModel, smallModel, nsim = 1000, ref = NULL, seed=NULL,
#          cl = NULL, details = 0)

require(pbkrtest)

PBmodcomp(lmer.2, lmer.0, nsim=1000)

#using update instead of separate models - for nested copmarisons
#  e.g. mod<-lmer(DV~IV1+IV2+(1|trial:Subject), data=dat1, REML=FALSE)
#       mod_no.IV2 <- update(mod, .~.-IV1)

lmer.2<-lmer(log(RTms) ~ Trial + Twitches + Task + Congruence
             + (1|SubNo), data = dat1.corr.a, REML = FALSE)
lmer.2.noCong <- update(lmer.2,.~.-Congruence)
#Likelihood ratio test
anova(lmer.2,lmer.2.noCong)




######## Building up and checking random effects #########
# Similar process here, but keep fixed effects constant now
# Now use REML = TRUE (will average across fe then calculate re)


lmer.0<-lmer(log(RTms) ~ (1|SubNo), data = dat1.corr.a, REML = TRUE)
summary(lmer.0)

lmer.1<-lmer(log(RTms) ~ (1|SubNo) + (1|HomLocation), data = dat1.corr.a, REML = TRUE)
summary(lmer.1)


#Likelihood ratio test
anova(lmer.0,lmer.1)
#Information criteria
AIC(lmer.0,lmer.1)
BIC(lmer.0,lmer.1)



######### Model checking #########
# see previous scripts on pre analysis data checks for Regression and ANOVA
# i.e. normality, outliers, collinearity etc.

# you can check these things by running regression models on the data (i.e. no random effects)
# if problems are present in these models, they will be present for LMMs.

# Plot residuals
qqnorm(resid(lmer.2))
qqline(resid(lmer.2))

hist(residuals(lmer.2))

# plot fitted values against residuals
plot(lmer.2)
plot(fitted(lmer.2), residuals(lmer.2),
     xlab = "Fitted Values", ylab = "Residuals")
#plot deviation of resiudals from normal
abline(h=0, lty=2)
lines(smooth.spline(fitted(lmer.2), residuals(lmer.2)))


### check fitted values look sensible for data
temp<-na.omit(dat1.corr.a)
temp$fit<-fitted.values(lmer.2)
# look by Subject for this data
with(temp,tapply(fit,SubNo,mean))
with(temp,tapply(logRT,SubNo,mean))


##### Convergence ########
# If your model fails to converge...

# From stats.stackexchange.com/questions/97929/lmer-model-fails-to-converge
# And stackoverflow.com/questions/21344555/convergence-error-for-development-version-of-lme4
# stats.stackexchange.com/questions/110004/how-scared-should-we-be-about-convergence-warnings-in-lme4

# see also: https://stat.ethz.ch/pipermail/r-sig-mixed-models/2014q2/022127.html

# Check for: zero variance estimates or +/- 1.0 correlation estimates
summary(lmer.2)

# Check gradient: should be <0.001
# https://github.com/lme4/lme4/issues/120
# Check gradient value
relgrad <- with(lmer.2@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

# Re run model with different optimizers
library(optimx)

lmer2_bobyqa <- update(lmer.2, control = lmerControl(optimizer="bobyqa"))

lmer2_NM <- update(lmer.2, control = lmerControl(optimizer="Nelder_Mead"))


# Extract number of predicter variables for a model
# Run this code for lmer.1 and lmer.2
nd<-get_all_vars(lmer.2,data=dat1.corr.a) 
ncol(nd)

# Compare parameter estimates across model fits
# Fixed effects
fixedEst<-matrix(nrow=length(fixef(lmer.2)),ncol=6)
rownames(fixedEst)<-names(fixef(lmer.2))
fixedEst[,1]<-fixef(lmer.2)
fixedEst[,2]<-fixef(lmer2_bobyqa)
fixedEst[,3]<-fixef(lmer2_NM)

# If they all look quite similar and seem sensible, then model is probably OK


######### Calculating significance for fixed effects & the model #########

# Time for a bit of reflection
# see glmm.wikidot.com/faq
help(pvalues)

# Confidence intervals for fixed effects
confint.merMod(lmer.2, method = "Wald")
# and what is Wald?
# en.wikipedia.org/wiki/Wald_test

#alternative that may take longer...
confint.merMod(lmer.2, method = "profile")

#package MuMIn to get R squared for LMMs
require(MuMIn)

lmer.0<-lmer(log(RTms) ~ (1|SubNo), data = dat1.corr.a, REML = FALSE)
lmer.2<-lmer(log(RTms) ~ Trial + Twitches + Task + Congruence
             + (1|SubNo), data = dat1.corr.a, REML = FALSE)

r.squaredLR(lmer.2, null=lmer.0)


# R2 
# Based on comparing model to intercept only model
# From glmm.wikidot.com/faq and Jarrett Byrnes
# with a line of code from me to print the model name
r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  print(c(deparse(substitute(m)), summary(lmfit)$r.squared))
}

# Compare the r2 values for the two models
r2.corr.mer(lmer.0)
r2.corr.mer(lmer.2)




################# Saving work #################

# Save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")




