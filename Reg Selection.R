#############################################################################################

# This script deals with variable selection algorithms for 
# linear regression.  We investigate best subsets, forward selection,
# and backward elimination.

library (readxl) # Read Excel files
library (tidyverse) # Basic data manipulation

library (leaps) #Regression Selection Library
library (forecast) #Accuracy measures

?regsubsets
?step
?accuracy

Boston_Housing <- read_excel("Boston_Housing.xlsx")
Boston <-select (Boston_Housing, -'CAT. MEDV') # Eliminate categorical variable
rm (Boston_Housing) #remove original import

#############################################
#  Best Subsets  
#
# Best subsets will find the nbest model(s) for 1 variable, 2 variables, etc
# up to the number of variables set in nvmax

# Find the best model for each variable size up to full set of 13
regfitbs <- regsubsets(MEDV ~ . , Boston, nbest=1, nvmax = 13)
rs<-summary (regfitbs) # Get results summary
rs
rsvars<-rs$which # get list of which variables were selected
View(rsvars)


# Examine r2 and r2 adjusted from models
plot (rs$rsq )
plot (rs$adjr2 )
rs$rsq 
rs$adjr2
rs$bic

bmnum <- which.max(rs$adjr) #get number of best model as defined as best r2 adjusted
bmnum <- which.min(rs$bic) #get number of best model as defined as best bic

bmc <- coef(regfitbs,bmnum) # Get variables from best r2 adjusted model
View(bmc)

bestlm  <- lm (MEDV ~CRIM + ZN + CHAS+ NOX+ RM + DIS + RAD + TAX + PTRATIO +B + LSTAT , Boston) #re-create 'manually'
summary(bestlm)


# Here I will select best variables programitically  
selvars <- rsvars[bmnum,2:14] # get variables to use
BostonBest <- Boston[ ,c(selvars, TRUE)] # create data frame with x-variables + MEDV
bestlm2  <- lm (MEDV ~., BostonBest) #re-create 'manually'
summary(bestlm2)

##################################################
#Backward Elimination

# In backward elimination we start with all variables and eliminate them one by one


blm <- lm(MEDV ~., Boston) #create the full model
regfitbe <-  step(blm, direction = "backward")
summary (regfitbe)
accuracy (regfitbe) # get basic fit metrics

##################################################
#Forward Selection

# In forward selection we start with an empty model (intercept only)
# We add variables one by one

blmstart <- lm(MEDV ~ 1, Boston) # define the intercept only starting model
regfitfs <-  step(blmstart, direction = "forward", scope = formula (blm))
summary(regfitfs)
accuracy(regfitfs)

