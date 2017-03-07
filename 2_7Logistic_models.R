##################### logistic regression models ############################

# For a fuller work through example of mixed model logistic regression
# with nice examples of graphs. I've taken and adapted stuff from it here.
# www.ats.ucla.edu/stat/mult_pkg/glmm.htm
# www.ats.ucla.edu/stat/r/dae/melogit.htm

##################### Current data set: aphasia naming
# Data in Logistic_Reg_Examples.RData
# Experiment looking at effect of phonological and semantic cues
# For 10 individuals with aphasia, 175 pictures.
# Logistic regression models comparing: 
# Phonoligcal cue to Phonological control condition


#############################################################################
library(lmerTest)
library(lme4)

load("Logistic_Reg_Examples.RData")

# For logistic models you use glmer (generalised LMMs)
# and you then specify the link function for the binomial distribution
# That is, you tell R that the distribution of data for the model is binomial

PhCue.Full = glmer(Response ~ + Trial + (1|Patient) + (1|Word) + Condition*(PicPC + PhonPC + SemPC), 
                   data=dat_PhCue, family = "binomial")
summary(PhCue.Full)

# You can change the optimisation parameters in these models directly
# There is no clear guidance yet on which one to use
# Recommend running both and comparing - they should not be very different
# see: stat.ethz.ch/pipermail/r-sig-mixed-models/2013q3/020925.html

PhCue.Full1 = glmer(Response ~ Trial + (1|Patient) + (1|Word) + Condition*(PicPC + PhonPC + SemPC), 
                   control=glmerControl(optimizer = "bobyqa"), data=dat_PhCue, 
                   family = "binomial")

PhCue.Full2 = glmer(Response ~ Trial + (1|Patient) + (1|Word) + Condition*(PicPC + PhonPC + SemPC), 
                   control=glmerControl(optimizer = "Nelder_Mead"), data=dat_PhCue, 
                   family = "binomial")

summary(PhCue.Full1)
summary(PhCue.Full2)

# Print results of models, without correlations between fixed effects
print(PhCue.Full1, corr=FALSE)
print(PhCue.Full2, corr=FALSE)

# Plot random effects from model 
library(lattice)
dotplot(ranef(PhCue.Full1, whichel = "Patient", condVar = TRUE))
dotplot(ranef(PhCue.Full1, whichel = "Word", condVar = TRUE))


#plot fixed effects & interactions
library(effects)
plot(allEffects(PhCue.Full1))

#plot effect of Trial
plot(effect(term="Trial",mod=PhCue.Full1),ylab="Accuracy",xlab="Trial",ci.style="lines")

#plot interaction of cue by pic complexity
plot(effect(term="Condition:PicPC",mod=PhCue.Full1),ylab="Accuracy",xlab="Pic complexity / Name agreement",ci.style="lines")


# Long hand work through for plotting random effects
reff.PhCue<-ranef(PhCue.Full1)
reff.Word<-reff.PhCue[[1]]
names(reff.Word)="Intercept"
reff.Word$Items=row.names(reff.Word)
attach(reff.Word)
index<-with(reff.Word,order(Intercept))
reff.Word<-reff.Word[index,]
dotchart(sort(reff.Word[[1]]),labels=row.names(reff.Word),cex=.7,main="Word random intercepts", xlab="Intercept")
#plot random effects for words
reff.Patient<-reff.PhCue[[2]]
dotchart(sort(reff.Patient[[1]]),labels=row.names(reff.Patient),cex=.7,main="Patient random intercepts", xlab="Intercept")



############## Check collinearity of IVs ##############

# Plot of correlations of predictor variables

#install GGally
require(GGally)
ggpairs(dat_PhCue[, c("PhonPC", "SemPC", "PicPC", "Trial")])



################# Saving work #################

# Save the whole 'workspace' - includes everything in the Environment
save.image(file="Workshop_workspace.RData")  

#Save your command/coding history
savehistory(file="Workshop_history.Rhistory")

