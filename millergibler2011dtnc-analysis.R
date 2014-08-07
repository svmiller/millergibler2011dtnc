getwd()
setwd("/Users/stevenmiller/Dropbox/teaching/posc3410/millergibler2011dtnc") # Note, this works for me. Edit as necessary.
library(car)
library(lattice) #for histogram(). I like histogram() better than hist().
library(RCurl)


## Read the data from Github. Note, I wish I could still do this from Dropbox.
##############################################################################

data <- getURL("https://raw.githubusercontent.com/svmiller/millergibler2011dtnc/master/millergibler2011dtnc-data.csv")
Data <- read.csv(text = data)
summary(Data)

## Re-create the "anyweak" variable.
####################################

Data$weakb <- ifelse(Data$cap_1 > 4*(Data$cap_2), 1, 0)
with(Data, cor(weakb, anyweak))

## Re-create the "lncapratdir" variable.
########################################

Data$lncap <- log(Data$cap_1/Data$cap_2)
with(Data, cor(lncap, lncapratdir))

# Why log this variable? Let's look.
Data$caprat <- exp(Data$lncapratdir)
histogram(Data$caprat) #requires the lattice package. Use hist() instead if you don't want the lattice package.

# Notice that skew? Yuck. Now, let's do a histogram of the lncaprat variable
histogram(Data$lncaprat)

# Notice how much more "normal" that variable is distributed now? Logging variables with positive skew reduces the effect of outliers.


## Re-create the alliance variable.
## 1 = defense, 2 = neutrality, 3 = entente, 4 = no alliance.
## -9 = missing. Let's also take care of that.
#############################################################

Data$alliance2 <- ifelse(Data$alliance >= 1 & Data$alliance < 4, 1, 0)
Data$alliance2[Data$alliance == -9] <- NA
summary(Data$alliance2)
with(Data, cor(alliance2, allied, use = "complete.obs"))

## Re-create the democracy dummies.
###################################

Data$dema <- ifelse(Data$polity21 > 5, 1, 0)
Data$demb <- ifelse(Data$polity22 > 5, 1, 0)
Data$dema[is.na(Data$dema)] <- 0
Data$demb[is.na(Data$demb)] <- 0

Data$jointdem2 <- ifelse(Data$dema == 1 & Data$demb == 1, 1, 0)

## If we did it right, M1 and M1a should be the same.
#####################################################


M1 <- glm(NEGOC ~ demdA + demdB + jointdem, data=Data, family=binomial(link="logit"))
M1a <- glm(NEGOC ~ dema + demb + jointdem2, data=Data, family=binomial(link="logit"))
summary(M1)
summary(M1a)


## Let's do everything else.
############################

M2 <- glm(NEGOC ~ demdA + demdB + jointdem + terrmid + terrcontig + anyweak + contig + allied + lncapratdir + hegemon, data=Data, family=binomial(link="logit"))
summary(M2)

M3 <- glm(NEGOC ~ demdA + demdB + jointdem + anyweak + contig + allied + lncapratdir + hegemon, data=subset(Data, Data$terrmid == 1), family=binomial(link="logit"))
summary(M3)

M4 <- glm(NEGOC ~ demdA + demdB + jointdem + anyweak + contig + allied + lncapratdir + hegemon, data=subset(Data, Data$terrmid == 0), family=binomial(link="logit"))
summary(M4)
