###
###  Emiliano Calvo Alcaniz
#######################################################
#                  Ch 2                               #
##    Sim data and modeling of ch. 2 following        #
##    what we do in the stat rethinking book club     #
#######################################################


## My working directory: 
# setwd("C:/Users/wjwor/OneDrive/Desktop/gitrepos/experience-contests")

# install.packages('dplyr')
# install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
# devtools::install_github("rmcelreath/rethinking")
library(rethinking)
library("coda")
library("mvtnorm")
library("devtools")
library("loo")
library("dagitty")
library("shape")

#### Creating the data frame to use --------------

data <- data.frame(
    contest_id = character(0)
  , colony_id = character(0)
  , previous_experience = factor(character(0))
  , total_mortality = integer(0))


#### Model time --------
# contest success inexp = 
#     u (mean) + 
#     d (effect size / ie strength of arrow for experience) * experience(n)
#     error around mean (which is the noise((??)))


# contest success exp =
#     u (mean) + 
#     d (effect size / ie strength of arrow for experience) * experience(y)
#     error around mean (which is the noise((??)))



sim_mort <- function(sample_size, base_mortality, effect_of_experience, spread_around_mean){
  experience <- sample(c(0, 1), sample_size, replace = TRUE)
  expected_avg_mortality <- base_mortality + effect_of_experience * experience 
  specific_colony_outcome <- rnorm(sample_size, expected_avg_mortality, spread_around_mean)
  norm_response <- (specific_colony_outcome - mean(specific_colony_outcome))/sd(specific_colony_outcome)
  simdata <- data.frame(experience, expected_avg_mortality, specific_colony_outcome, norm_response)
return(simdata)
  }

# test <- sim_mort(50, 5, 2, 1)   # testing that the function does function

### Make the parameters
sample_size <- 40

# Inexperienced colony
inexp_base_mortality <- 2
inexp_effect_of_experience <- 1
inexp_spread_around_mean <- 1

# Experienced colony
exp_base_mortality <- 5
exp_effect_of_experience <- 1
exp_spread_around_mean <- 1


### Testing or whatever
inexp_sim <- sim_mort(inexp_sample, inexp_mort_mean, inexp_mort_sd)
hist(inexp_sim)

exp_sim <- sim_mort(exp_sample, exp_mort_mean, exp_mort_sd)
hist(exp_sim)

# Set aesthetics
par(mfrow=c(1,2))
ymin <- -5
ymax <- 5
# Plot the sim data I just made
boxplot(inexp_sim, col = "white", xlab = "Inexperienced\nColonies", ylab = "Z-Score"
        , ylim = c(ymin, ymax))
stripchart(inexp_sim
           , vertical = TRUE
           , method = "jitter"
           , add = TRUE
           , col = "black"
           , pch = 20)
boxplot(exp_sim, col = "white", xlab = "Experienced\nColonies", ylab = "Z-Score"
        , ylim = c(ymin, ymax))
stripchart(exp_sim
           , vertical = TRUE
           , method = "jitter"
           , add = TRUE
           , col = "black"
           , pch = 20)







## Fixing aesthetic parameter
par(mfrow=c(1,1))


