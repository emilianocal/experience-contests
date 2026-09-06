# Emiliano Calvo Alcaniz, Anna Dornhaus
# (c) 2026
# What is the role of experience in determining collective contest success?

### GETTING STARTED -------------------------------------------------------
# Set WD by opening project

# Load in packages
library(rethinking)

# Set seed for consistency
set.seed(123)

# Set aesthetics for the plots
ymin <- -5    # y minimum for plots
ymax <- 5     # y maximum for plots

aes_col <- c("0" = "#EBF5DF", "2" = "#8EBA73")      # pretty colors for experience levels
stripchart_col <- c("0" = "#93A083", "2" = "#2F3D26") # for the stripcharts

### DATA FORMATTING -------------------------------------------------------
# (blank for now)


### SIMULATING DATA -------------------------------------------------------

# Create the data frame that I'll be using
data <- data.frame(
  contest_id = character(0)
  , colony_id = character(0)
  , previous_experience = factor(character(0))
  , total_mortality = integer(0))

## Model time 
# Mortality (Response variable) = 
#     u (mean mortality) + 
#     d (effect size of having experience) * experience(n) (0 or 2) +
#     error around mean (the noise)

# Function that simulates the data
## Takes an input of sample size, mortality, experience, and error
sim_mort <- function(sample_size, base_mortality, effect_of_experience, spread_around_mean){
  # set the experience for each simulated line, either 0 or 2
  experience <- sample(c(0, 2), sample_size, replace = TRUE)  
# calculate the expected average mortality based on the u + d * exp above
  expected_avg_mortality <- base_mortality + effect_of_experience * experience 
# create a normal distribution of outcomes for individual colonies, adding the error
  specific_colony_outcome <- rnorm(sample_size, expected_avg_mortality, spread_around_mean)
# get the z score for each outcome
  norm_response <- (specific_colony_outcome - mean(specific_colony_outcome))/sd(specific_colony_outcome)
# populate the data frame with the simulated numbers
  simdata <- data.frame(experience, expected_avg_mortality, specific_colony_outcome, norm_response)
  return(simdata)
}

# test <- sim_mort(50, 5, 2, 1)   # Test that the function does actually function

## Parameterization time...
sample_size <- 40
base_mortality <- 2
effect_of_experience <- 1
spread_around_mean <- 1

# Put it all together
sim_data <- sim_mort(sample_size, base_mortality
                     , effect_of_experience, spread_around_mean)



### STATISTICAL ANALYSIS ---------------------------------------------------









### FIGURE CREATION ---------------------------------------------------

# Plotting the sim data for MORTALITY
boxplot(norm_response ~ experience
        , data = sim_data
        , col = aes_col
        , main = "[SIMULATED]\nEffect of previous experience on contest mortality"
        , xlab = "Previous contest experience of colonies"
        , ylab = "Z-Score"
        , ylim = c(ymin, ymax)
        , range = 0)

stripchart(norm_response ~ experience
           , data = sim_data
           , vertical = TRUE
           , method = "jitter"
           , add = TRUE
           , col = stripchart_col
           , pch = 20)


# Plotting the sim data for BROOD LOSS


































