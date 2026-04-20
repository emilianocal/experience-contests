###
###  Emiliano Calvo Alcaniz
#######################################################
#                  Ch 2 
##     What is the role of experience                ##
##      in determining contest success?              ##
#           Feb-2026
#######################################################

### README!!!! ---------------------------------------
# For ease of reading, I've tried to stick to a code:

###  3 HASHTAGS MEANS SECTION TITLE OR OVERVIEW

##   2 hashtags is an intermediate comment

#    1 hashtag is a comment explaining what the line(s) of code is/are doing
#   -> 1 hashtag + an arrow means this comment is a continuation of the one above

### SETTING UP THE SCRIPT (WORKING DIR, LIBRARIES, ETC) ----------------
## My working directory: 
# setwd("C:/Users/wjwor/OneDrive/Desktop/gitrepos/experience-contests")

# install.packages('dplyr')
library("dplyr")
library("tidyr")

### SEED, FUNCTIONS, & PARAMETERS FOR SIMULATING COLONY IMAGES ------------
# set the seed for consistency
set.seed(123)

# Set the collection sites, each of which corresponds to one of the following:
# Willow Canyon, Mount Lemmon, Madera Canyon, Jack Mountain
locations <- c("WC", "ML", "MC", "JM")

### HELPER FUNCTION #1
#   Create a function to make the colony IDs, which are structured as 
#   Collection date (YYMMDD) Location _ Order, ex: 260221JM_A
make_colony_id <- function(date_str, location, order_letter) {
  # paste0() is a special function that links vectors and converts them into characters
  paste0(date_str, location, order_letter)
}

### HELPER FUNCTION #2
#   Create a function to make the contest IDs, which are structured as
#   -> Contest date (YYMMDD) _ Order, ex: 260201_A
#   For simplicity's sake, I'm simulating as if I'll only do one contest daily
make_contest_id <- function(date_str, suffix = "A") {
  paste0(date_str, "_", suffix)
}

# Set start date for the collection and contests
# We'll just pretend it all started on January 1st of this year
start_date <- as.Date("2026-01-01")

### SET UP THE POOL OF COLONY CODES TO USE
#   20 experienced colonies (will have experience of 0, 1, & 2 previous contests)
#   20 naive colonies (appear once, experience = 0, vs final experienced colonies)

# Set colony collection dates, which will be said to occur daily for 40 days 
# -> starting on January 1st
## This _does_ mean that some colonies will be implied to have been caught
## -> after another colony that doesn't actually exist. Don't ask. I only just caught this
colony_dates     <- format(start_date + 0:39, "%y%m%d")

# Set colony collection sites, which will be randomly sampled from one of the
# -> collection sites I set before, and will get a random letter to denote
# -> in which order they were collected
colony_locations <- sample(locations, 40, replace = TRUE)
colony_orders    <- sample(LETTERS[1:8], 40, replace = TRUE)

# Put together the colony IDs based on the site, date, and location info set before
colony_ids       <- mapply(make_colony_id,
                     colony_dates,
                     colony_locations,
                     colony_orders)

# Assign the colonies we just simulated to either the experienced or naive group
experienced_colonies <- colony_ids[1:20]   # will fight 3 contests total
naive_colonies       <- colony_ids[21:40]  # will fight 1 contest

### SET THE QUEEN NUMBER
## If this isn't done, you get colonies who shift 
## --> between 1 and 20 queens sometimes
queen_distribution <- round(rexp(40, rate = 0.3))+1
queen_lookup <- setNames(
  # here, we have r pick between 1 and 20 queens, where the probability of having
  # -> 1 queen is 25%, and any of the other 9 options are randomly chosen from the remaining 75%
  sample(queen_distribution, 40, replace = FALSE,)
  , colony_ids
)

#  Set the contest dates, they obviously have to take place after the collection dates
contest_dates <- format(start_date + 40 + 0:39, "%y%m%d")

### PAIR UP THE EXPERIENCED COLONIES FOR EACH CONTEST
##  To do this, we'll be creating a round-robin function to pair up the colonies

# First, define a function called round_robin that takes a single argument: colonies
# -> "colonies" is the vector of 20 experienced colony IDs
# n gets set as the number of colonies, and can then be used to control the
# -> length of the loop so it stops once we're out of colonies
round_robin <- function(colonies) {
  n     <- length(colonies)
  # set the first colony as "fixed" so the round robin algorithm can rotate the
  # -> other colonies around it. The remaining colonies are set as "rotating"
  fixed <- colonies[1]
  rotating <- colonies[-1]
  # create an empty list with 19 spaces, so the colonies each face 
  # -> the other colonies ONLY ONCE
  rounds <- vector("list", n - 1)
  ## This is the hard part :'(
  #  Below, we tell r to loop through a sequence of the number of colonies MINUS 1
  #  -> that way, we don't pair up colonies against a colony they already fought
  #  "round_cols" is an assembled sequence of the colonies, with the fixed colony first
  #  -> Reminder: in a round-robin, you want one fixed individual that the others rotate around
  ## I'm going to break down the "pairs" line in extreme detail bc it's hard
  #  seq(1, n, by = 2) creates a sequence of numbers, 1:n, 
  #  -> where n = the number of colonies (basically, n = 20)
  #  -> this creates a sequence of the LEFT-SIDE colony in each pair (1, 3, 5, ... up to 19)
  #  lapply(X, FUN) returns a list of X that has had a function (FUN) applied to it
  #  -> (IN OUR CASE, X = the sequence of left-side colony pairings we just made)
  #     -> Reminder: we want to create a list of paired up colonies from which
  #     -> we simulate contests, that's the round-robin thing we've been doing
  #  -> the function below, function(i) says for each iteration (i)
  #     -> (IN OUR CASE, i = the LEFT SIDE COLONY OF THE PAIR set by the sequence)
  #     -> we want to create a pairing, where the colony i is paired with
  #     -> the colony i + 1
  ##    -> WHAT DOES THIS MEAN????
  #        -> VERY SIMPLY: we are creating a list of pairs where colonies 1&2 are paired,
  #        -> colonies 3&4 are paired, 5&6, 7&8, etc
  ##    -> IS THIS THE EASIEST WAY TO DO THIS? 
  ##       -> My dad said no but I don't know
  #   Okay, now we have our pairs, we need to store them, which we do into "rounds[[r]]"
  #   -> we have to do this with the [[]] because this allows us to store them in 
  #   -> a way where they won't get overwritten AND we can retrieve them later
  #   -> The rounds had to be broken down into blocks (rounds[1], rounds[2], rounds[3])
  #   -> because there's going to be 3 pairing events where the colonies cannot fight
  #   -> against a repeat colony
  #  Lastly, the rotating line exists to shift the colony pairs over, to create new pairings
  #  -> we create a list where the current LAST colony becomes the new FIRST colony
  #  -> "rotating[length(rotating)] is the same as "rotating[19]"
  #  -> "rotating[-length(rotating)]" takes the list of colonies except for the final one
  #  ->  and leaves them in the same order as before
  #      -> This allows the next iteration of the loop to assign new pairings without
  #      -> accidentally having repeat contests between colonies
  #  After all this, we close the loop
  
  for (r in seq_len(n - 1)) {
    round_cols <- c(fixed, rotating)
    pairs <- lapply(seq(1, n, by = 2), function(i) c(round_cols[i], round_cols[i + 1]))
    rounds[[r]] <- pairs
    rotating <- c(rotating[length(rotating)], rotating[-length(rotating)])
  }
  
  # "rounds" below now gives us the final list of rounds, which has the 19 rounds of 10 pairings
  rounds
}

# Here, we combine all the lists of rounds from before 
all_rounds <- round_robin(experienced_colonies)

# Now, we create the shuffled pairings for the first 2 rounds of experience contests
block1_pairs <- all_rounds[[1]] # Round 1 → Block 1 pairs (experience = 0)
block2_pairs <- all_rounds[[2]] # Round 2 → Block 2 pairs (experience = 1)

# Round 3 pairings: experienced vs naive colonies
naive_shuffled <- sample(naive_colonies) # shuffle the colonies to create random pairing
block3_pairs   <- lapply(1:20, function(i) c(experienced_colonies[i], naive_shuffled[i]))


### SET UP DISTRIBUTIONS FOR STUFF THAT ISN'T HYPOTHESIS-SPECIFIC
# For brood, set the mean and standard deviation
brood_mean <- 50
brood_sd <- 20
brood_num <- round(rnorm(40, mean = brood_mean, sd = brood_sd))

# For workers, set the mean and standard deviation
worker_mean <- 50
worker_sd <-15
worker_num <- round(rnorm(40, mean = worker_mean, sd = worker_sd))


### SIMULATED COLONY IMAGES FOR H0 ----------------------------------

# Creating an empty df for h0 to use for simulated data, matching my datasheet
sim_images_h0_df <- data.frame(
      contest_id = character(0)
  ,   colony_id = character(0)
  ,   worker_number = integer(0)
  ,   brood_number = integer(0)
  ,   queen_number = integer(0)
  ,   contest_mortality = integer(0)
  ,   hr_mortality = integer(0)
  ,   total_mortality = integer(0)
  ,   previous_experience = factor(character(0))
  ,   brood_loss = integer(0)
  ,   notes = character(0)
)

# For mortalities
# Set as an exponential distribution where the majority of the values
# -> are very low and a small amount are larger
contest_mortality_distribution_h0 <- round(rexp(40, rate = 0.33)) + 1
hr_mortality_distribution_h0 <- round(rexp(40, rate = 0.31)) + 1


### NOW WE ADD THE ROWS TO THE DATA FRAME WE MADE
# Here's another function, this one adding information to the df
make_rows <- function(contest_id, colony_id, experience) {
# the number of rows should correspond to the number of colonies
  n_rows <- length(colony_id)
  data.frame(
# the contest_id column should replicate the contest ids set up the number of times
# -> that we have rows
    contest_id           = rep(contest_id, n_rows)
  ,  colony_id            = colony_id
# the worker and brood number should be randomly sampled from a set of values
  ,  worker_number        = sample(worker_num,  n_rows, replace = TRUE)
  ,  brood_number         = sample(brood_num,  n_rows, replace = TRUE)
# the queen number is just based on the sample we made before
  ,  queen_number         = queen_lookup[colony_id]
# contest and hr mortality are randomly sampled
  ,  contest_mortality    = sample(contest_mortality_distribution_h0, n_rows)
  ,  hr_mortality         = sample(hr_mortality_distribution_h0, n_rows)
# total mortality is left blank and will be calculated at the end by adding up 
# -> the previous two lines
  ,  total_mortality      = NA_integer_
# whether the colonies have previous experience depends on the block AND 
# -> the colony type (ie. experienced or naive)
  ,  previous_experience  = factor(experience, levels = c("0", "1", "2"))
# randomly sampled
  ,  brood_loss = round((rexp(n_rows, rate = .7)*rexp(n_rows, rate = 0.7)))
# put in a small chance of random notes, not really important tbh
  ,  notes                = sample(
      c(""), n_rows, replace = TRUE,
      prob = 1)
  ,  stringsAsFactors = FALSE
  )
}

### BLOCK 1: 10 CONTESTS WHERE EXPERIENCE = 0
# seq_along(block1_pairs) creates a sequence along which to iterate the contests
# -> This sequence is then applied to the row-making function below
# -> In this row-making function, r creates the contest id by taking each 
# -> iteration of contest date ("contest_dates[i]") and then converting it into 
# -> a contest id using the contest making function from like 200 lines ago
# The colony_id is determined by retrieving each colony ID from the block pairs list
# -> Because this is a list, we have to use the [[double brackets]] to extract
# -> the actual pair names from the list, not just a new list that [single brackets] 
# -> would give. the "make_rows" function needs the object names themselves
# Experience is 0,0 because both colonies are experiencing their first contest
block1 <- do.call(rbind, lapply(seq_along(block1_pairs), function(i) {
  make_rows(
    contest_id = make_contest_id(contest_dates[i]),
    colony_id  = block1_pairs[[i]],
    experience = c("0", "0")
  )
}))

### BLOCK 2: 10 CONTESTS WHERE EXPERIENCE = 1 
# This is done the same as above but with 3 changes:
# -> 1) We use block2_pairs instead of block1_pairs (bc this is block 2)
# -> 2) The colonies have previous contest experience
# -> 3) The contest ids are pushed back 10 days, to be after the previous contests end
block2 <- do.call(rbind, lapply(seq_along(block2_pairs), function(i) {
  make_rows(
    contest_id = make_contest_id(contest_dates[10 + i]),
    colony_id  = block2_pairs[[i]],
    experience = c("1", "1")
  )
}))

### BLOCK 3: 20 CONTESTS WHERE EXPERIENCE IS EITHER 0 OR 2
# Same comments as above: we update the block_pairs, the experience, and the dates
block3 <- do.call(rbind, lapply(seq_along(block3_pairs), function(i) {
  make_rows(
    contest_id = make_contest_id(contest_dates[20 + i]),
    colony_id  = block3_pairs[[i]],
    experience = c("2", "0")
  )
}))


### DERIVING AND INPUTTING THE TOTAL MORTALITY COLUMN
# rbind() sticks together the data frames that we just put together above
sim_images_h0_df <- rbind(block1, block2, block3)
# This part is easy, just fill the "total_mortality" column 
# with the total mortality added up
sim_images_h0_df$total_mortality <- sim_images_h0_df$contest_mortality +
  sim_images_h0_df$hr_mortality
# rownames(sim_images_h0_df) <- NULL


View(sim_images_h0_df)

### SIMULATED COLONY IMAGES FOR H1 ----------------------------------
# Creating an empty df for h1 to use for simulated data, matching my datasheet
sim_images_h1_df <- data.frame(
  contest_id = character(0)
  ,   colony_id = character(0)
  ,   worker_number = integer(0)
  ,   brood_number = integer(0)
  ,   queen_number = integer(0)
  ,   contest_mortality = integer(0)
  ,   hr_mortality = integer(0)
  ,   total_mortality = integer(0)
  ,   previous_experience = factor(character(0))
  ,   brood_loss = integer(0)
  ,   notes = character(0)
)

### MORTALITY CALCULATIONS FOR H1
# Separated into naive and experienced categories
# Naive has more of a rightward skew, with some major mortality events
contest_mortality_dist_h1_naive <- round(rexp(40, rate = 0.25))
hr_mortality_dist_h1_naive <- round(rexp(40, rate = 0.26))

# Experienced has much less of a skew
contest_mortality_dist_h1_exp <- round(rexp(40, rate = 0.45))
hr_mortality_dist_h1_exp <- round(rexp(40, rate = 0.46))


### NOW WE ADD THE ROWS TO THE DATA FRAME WE MADE
## Since this is the same as before, it won't have detailed comments
## --> EXCEPT where I added something
  make_rows_h1 <- function(contest_id, colony_id, experience) {
    n_rows <- length(colony_id)
# is_exp is a group where experience doesn't equal 0, it's either 1 or 2
    is_exp <- experience != "0"
# Using ifelse() and based on experience, we're gonna have r pick from the mortality
# -> distributions we just outlined above. If the colony is experienced (is_exp),
# -> then the rows funciton will pick from the first mortality distribution sample 
# -> Otherwise, it'll pick from the second mortality distribution sample
      contest_mort <- ifelse(
      is_exp,
      sample(contest_mortality_dist_h1_exp,  n_rows, replace = TRUE),
      sample(contest_mortality_dist_h1_naive, n_rows, replace = TRUE)
    )
# Here, we're doing the exact same thing as above, but with the mortality after
# -> 24 hours (which is what "hr_mort" stands for)
    hr_mort <- ifelse(
      is_exp,
      sample(hr_mortality_dist_h1_exp,  n_rows, replace = TRUE),
      sample(hr_mortality_dist_h1_naive, n_rows, replace = TRUE)
    )
    
# Brood loss: experienced colonies have a higher rate (more concentrated near 0)
    brood_rate <- ifelse(is_exp, 1.4, 0.7)
    brood_loss <- round(rexp(n_rows, rate = brood_rate) * rexp(n_rows, rate = brood_rate))

# Here's what's going in each line of the data frame that I made before
    data.frame(
      contest_id          = rep(contest_id, n_rows),
      colony_id           = colony_id,
      worker_number       = sample(worker_num, n_rows, replace = TRUE),
      brood_number        = sample(brood_num,  n_rows, replace = TRUE),
      queen_number        = queen_lookup[colony_id],
      contest_mortality   = contest_mort,
      hr_mortality        = hr_mort,
      total_mortality     = NA_integer_,
      previous_experience = factor(experience, levels = c("0", "1", "2")),
      brood_loss          = brood_loss,
      notes               = sample(c(""), n_rows, replace = TRUE, prob = 1),
      stringsAsFactors    = FALSE
    )
  }

### BLOCK 1: 10 CONTESTS WHERE EXPERIENCE = 0
block1_h1 <- do.call(rbind, lapply(seq_along(block1_pairs), function(i) {
  make_rows(
    contest_id = make_contest_id(contest_dates[i]),
    colony_id  = block1_pairs[[i]],
    experience = c("0", "0")
  )
}))

### BLOCK 2: 10 CONTESTS WHERE EXPERIENCE = 1 
block2_h1 <- do.call(rbind, lapply(seq_along(block2_pairs), function(i) {
  make_rows(
    contest_id = make_contest_id(contest_dates[10 + i]),
    colony_id  = block2_pairs[[i]],
    experience = c("1", "1")
  )
}))

### BLOCK 3: 20 CONTESTS WHERE EXPERIENCE IS EITHER 0 OR 2
# Same comments as above: we update the block_pairs, the experience, and the dates
block3_h1 <- do.call(rbind, lapply(seq_along(block3_pairs), function(i) {
  make_rows_h1(
    contest_id = make_contest_id(contest_dates[20 + i]),
    colony_id  = block3_pairs[[i]],
    experience = c("2", "0")
  )
}))


### DERIVING AND INPUTTING THE TOTAL MORTALITY COLUMN
# rbind() sticks together the data frames that we just put together above
sim_images_h1_df <- rbind(block1_h1, block2_h1, block3_h1)
# This part is easy, just fill the "total_mortality" column 
# with the total mortality added up
sim_images_h1_df$total_mortality <- sim_images_h1_df$contest_mortality +
  sim_images_h1_df$hr_mortality

View(sim_images_h1_df)
### SIMULATED COLONY IMAGES FOR H2 ----------------------------------
# Creating an empty df for h0 to use for simulated data, matching my datasheet
sim_images_h2_df <- data.frame(
  contest_id = character(0)
  ,   colony_id = character(0)
  ,   worker_number = integer(0)
  ,   brood_number = integer(0)
  ,   queen_number = integer(0)
  ,   contest_mortality = integer(0)
  ,   hr_mortality = integer(0)
  ,   total_mortality = integer(0)
  ,   previous_experience = factor(character(0))
  ,   brood_loss = integer(0)
  ,   notes = character(0)
)

### MORTALITY CALCULATIONS FOR H2
# Naive has LESS of a rightward skew
contest_mortality_dist_h2_naive <- round(rexp(40, rate = 0.96))
hr_mortality_dist_h2_naive <- round(rexp(40, rate = 0.95))

# Experienced has much MORE of a skew, with mortality events
contest_mortality_dist_h2_exp <- round(rexp(40, rate = 0.32))
hr_mortality_dist_h2_exp <- round(rexp(40, rate = 0.33))


### NOW WE ADD THE ROWS TO THE DATA FRAME WE MADE
## Since this is the same as before, it won't have detailed comments
## --> The only real change here is that I'm using my "h2" distributions that
## --> we just made above
make_rows_h2 <- function(contest_id, colony_id, experience) {
  n_rows <- length(colony_id)
  is_exp <- experience != "0"
  contest_mort <- ifelse(
    is_exp,
    sample(contest_mortality_dist_h2_exp,  n_rows, replace = TRUE),
    sample(contest_mortality_dist_h2_naive, n_rows, replace = TRUE)
  )
  
  hr_mort <- ifelse(
    is_exp,
    sample(hr_mortality_dist_h2_exp,  n_rows, replace = TRUE),
    sample(hr_mortality_dist_h2_naive, n_rows, replace = TRUE)
  )
  
  # Brood loss: experienced colonies have a higher rate (more concentrated near 0)
  brood_rate <- ifelse(is_exp, 0.57, 1.33)
  brood_loss <- round(rexp(n_rows, rate = brood_rate) * rexp(n_rows, rate = brood_rate))
  
  data.frame(
    contest_id          = rep(contest_id, n_rows),
    colony_id           = colony_id,
    worker_number       = sample(worker_num, n_rows, replace = TRUE),
    brood_number        = sample(brood_num,  n_rows, replace = TRUE),
    queen_number        = queen_lookup[colony_id],
    contest_mortality   = contest_mort,
    hr_mortality        = hr_mort,
    total_mortality     = NA_integer_,
    previous_experience = factor(experience, levels = c("0", "1", "2")),
    brood_loss          = brood_loss,
    notes               = sample(c(""), n_rows, replace = TRUE, prob = 1),
    stringsAsFactors    = FALSE
  )
}

### BLOCK 1: 10 CONTESTS WHERE EXPERIENCE = 0
block1_h2 <- do.call(rbind, lapply(seq_along(block1_pairs), function(i) {
  make_rows_h2(
    contest_id = make_contest_id(contest_dates[i]),
    colony_id  = block1_pairs[[i]],
    experience = c("0", "0")
  )
}))

### BLOCK 2: 10 CONTESTS WHERE EXPERIENCE = 1 
block2_h2 <- do.call(rbind, lapply(seq_along(block2_pairs), function(i) {
  make_rows_h2(
    contest_id = make_contest_id(contest_dates[10 + i]),
    colony_id  = block2_pairs[[i]],
    experience = c("1", "1")
  )
}))

### BLOCK 3: 20 CONTESTS WHERE EXPERIENCE IS EITHER 0 OR 2
# Same comments as above: we update the block_pairs, the experience, and the dates
block3_h2 <- do.call(rbind, lapply(seq_along(block3_pairs), function(i) {
  make_rows_h2(
    contest_id = make_contest_id(contest_dates[20 + i]),
    colony_id  = block3_pairs[[i]],
    experience = c("2", "0")
  )
}))


### DERIVING AND INPUTTING THE TOTAL MORTALITY COLUMN
# rbind() sticks together the data frames that we just put together above
sim_images_h2_df <- rbind(block1_h2, block2_h2, block3_h2)
# This part is easy, just fill the "total_mortality" column 
# with the total mortality added up
sim_images_h2_df$total_mortality <- sim_images_h2_df$contest_mortality +
  sim_images_h2_df$hr_mortality

View(sim_images_h2_df)

### SIMULATING DATA OF VIDEO ANALYSIS ----------------------------------
simulated_video_data_df <- data.frame(
      contest_id = character(0)
  ,   file_name = character(0)
  ,   contest_id = character(0)
  ,   contest_start_time = numeric(0)
  ,   time_stamp = numeric(0)
  ,   colony_id = character(0)
  ,   number_workers_out = integer(0)
  ,   number_workers_pinning = integer(0)
  ,   number_workers_guarding = integer(0)
  ,   brood_care_in_arena = integer(0)
  ,   tandem_runs = character(0)
  ,   notes = character(0)
)









### SIMULATING DATA COLLECTIVE CONTEST ----------------------------------
simulated_notes_data_df <- data.frame(
      contest_id = character(0)
  ,   date = character(0)
  ,   time_start = character(0)
  ,   colony_1_id = character(0)
  ,   colony_2_id = character(0)
  ,   workers_removed_from_colony_box = numeric(0)
  ,   notes = character(0)
)







### ACTUAL DATA IMPORTING ----------------------------------
# For now.... empty





### DATA WRANGLING  -----------------------------------
## Take 1, but it breaks once I actually try to run the test
dfh0 <- sim_images_h0_df[41:80,]
dfh0$relative_colony_code <- c("colony_1", "colony_2")

df_mortality_h0 <- pivot_wider(dfh0
                       , names_from = relative_colony_code,
                       , values_from = total_mortality
                       , id_cols = contest_id)

wilcox.test(df_mortality_h0$colony_1, df_mortality_h0$colony_2, paired = TRUE)


dfh1 <- sim_images_h1_df[41:80,]
dfh1$relative_colony_code <- c("colony_1", "colony_2")

df_mortality_h1 <- pivot_wider(dfh1
                            , names_from = relative_colony_code,
                            , values_from = total_mortality
                            , id_cols = contest_id)

wilcox.test(df_mortality_h1$colony_1, df_mortality_h1$colony_2, paired = TRUE)


dfh2 <- sim_images_h2_df[41:80,]
dfh2$relative_colony_code <- c("colony_1", "colony_2")

df_mortality_h2 <- pivot_wider(dfh2
                               , names_from = relative_colony_code,
                               , values_from = total_mortality
                               , id_cols = contest_id)

wilcox.test(df_mortality_h2$colony_1, df_mortality_h1$colony_2, paired = TRUE)






### ANALYSIS -------------------------------------------------------------
### H0 PLOTS ----------

# Renaming the variables I'm plotting for convenience
broodlossh0 <- sim_images_h0_df$brood_loss[41:80]
mortalityh0 <- sim_images_h0_df$total_mortality[41:80]
n_workerh0 <- sim_images_h0_df$worker_number[41:80]
exph0 <- sim_images_h0_df$previous_experience[41:80]

# Have to set the color before cause r hates me specifically
exp_colors <- c("0" = "maroon", "1" = "white", "2" = "steelblue1")

# Create the plot
plot(n_workerh0, jitter(mortalityh0)
     , main = "[SIMULATED]  [H0]\nWorker Mortality and Worker Number \nColored by Experience"
     , xlab = "Number of Workers in a Colony"
     , ylab = "Total Worker Mortality"
     , pch = 19
     , col = exp_colors[exph0]
     )

# For some reason this is the only way I found to actually get rid of the 
# -> experience level of 1 in the legend, since this plot only shows the 
# -> fully experienced vs fully naive colonies
# The droplevels() function gets rid of unused levels from a factor

present_levels <- levels(droplevels(exph0))
legend("topright"
       , legend = c("Naive", "Experienced")
       , col    = exp_colors[present_levels]
       , pch    = 19
)


### BOX PLOTS OF WORKER MORTALITY GROUPED BY EXPERIENCE
boxplot(mortalityh0 ~ previous_experience
        , data = droplevels(sim_images_h0_df[41:80, ])
        , main = "[SIMULATED]  [H0]\nTotal Worker Mortality Grouped by Experience"
        , names = c("Naive", "Experienced")
        , xlab = "Prevous Experience"
        , ylab = "Total Worker Mortality"
        , col = exp_colors[present_levels]
)

### SHOW THE RAW DATA POINTS ON TOP
stripchart(mortalityh0 ~ previous_experience,
           data   = droplevels(sim_images_h0_df[41:80, ])
           , add    = TRUE
           , method = "jitter"
           , pch    = 19
# adjustcolor() gives the option to make the points transparent
           , col    = adjustcolor("gray23", alpha.f = 0.4)
           , vertical = TRUE
)

### STATISTICAL TEST
# Non-parametric test, seeing if the two groups have a 
# -> statistically significant difference
wilcox.test(mortalityh0 ~ exph0, data = sim_images_h0_df)


### BOX PLOTS OF BROOD LOSS GROUPED BY EXPERIENCE
boxplot(broodlossh0 ~ previous_experience
        , data = droplevels(sim_images_h0_df[41:80, ])
        , main = "[SIMULATED]  [H0]\nTotal Brood Loss Grouped by Experience"
        , names = c("Naive", "Experienced")
        , xlab = "Prevous Experience"
        , ylab = "Brood Loss"
        , col = exp_colors[present_levels]
        , range = 0
)

### SHOW THE RAW DATA POINTS ON TOP
stripchart(broodlossh0 ~ previous_experience,
           data   = droplevels(sim_images_h0_df[41:80, ])
           , add    = TRUE
           , method = "jitter"
           , pch    = 19
           # adjustcolor() gives the option to make the points transparent
           , col    = adjustcolor("gray23", alpha.f = 0.4)
           , vertical = TRUE
)

### STATISTICAL TEST

# Non-parametric test, seeing if the two groups have a 
# -> statistically significant difference
wilcox.test(broodloss ~ exp, data = sim_images_h0_df)

### H1 PLOTS ----------
broodlossh1 <- sim_images_h1_df$brood_loss[41:80]
mortalityh1 <- sim_images_h1_df$total_mortality[41:80]
n_workerh1 <- sim_images_h1_df$worker_number[41:80]
exph1 <- sim_images_h1_df$previous_experience[41:80]
pres_levels_h1 <- levels(droplevels(exph1))


### BOX PLOTS OF WORKER MORTALITY GROUPED BY EXPERIENCE
boxplot(mortalityh1 ~ previous_experience
        , data = droplevels(sim_images_h1_df[41:80, ])
        , main = "[SIMULATED]  [H1]\nTotal Worker Mortality Grouped by Experience"
        , names = c("Naive", "Experienced")
        , xlab = "Prevous Experience"
        , ylab = "Total Worker Mortality"
        , range = 0
        , col = exp_colors[pres_levels_h1]
)

### SHOW THE RAW DATA POINTS ON TOP
stripchart(mortalityh1 ~ previous_experience,
           data   = droplevels(sim_images_h1_df[41:80, ])
           , add    = TRUE
           , method = "jitter"
           , pch    = 19
           , col    = adjustcolor("gray23", alpha.f = 0.4)
           , vertical = TRUE
)

### STATISTICAL TEST
# Non-parametric test, seeing if the two groups have a 
# -> statistically significant difference
wilcox.test(mortalityh1 ~ exph1, data = sim_images_h1_df)



### BOX PLOTS OF BROOD LOSS GROUPED BY EXPERIENCE
boxplot(broodlossh1 ~ previous_experience
        , data = droplevels(sim_images_h1_df[41:80, ])
        , main = "[SIMULATED]  [H1]\nTotal Brood Loss Grouped by Experience"
        , names = c("Naive", "Experienced")
        , xlab = "Prevous Experience"
        , ylab = "Brood Loss"
        , col = exp_colors[pres_levels_h1]
        , range = 0
)

### SHOW THE RAW DATA POINTS ON TOP
stripchart(broodlossh1 ~ previous_experience,
           data   = droplevels(sim_images_h1_df[41:80, ])
           , add    = TRUE
           , method = "jitter"
           , pch    = 19
           , col    = adjustcolor("gray23", alpha.f = 0.4)
           , vertical = TRUE
)

### STATISTICAL TEST

# Non-parametric test, seeing if the two groups have a 
# -> statistically significant difference
wilcox.test(broodlossh1 ~ exph1, data = sim_images_h1_df)



### H2 PLOTS --------
broodlossh2 <- sim_images_h2_df$brood_loss[41:80]
mortalityh2 <- sim_images_h2_df$total_mortality[41:80]
n_workerh2 <- sim_images_h2_df$worker_number[41:80]
exph2 <- sim_images_h2_df$previous_experience[41:80]
pres_levels_h2 <- levels(droplevels(exph2))


### BOX PLOTS OF WORKER MORTALITY GROUPED BY EXPERIENCE
boxplot(mortalityh2 ~ previous_experience
        , data = droplevels(sim_images_h2_df[41:80, ])
        , main = "[SIMULATED]  [H2]\nTotal Worker Mortality Grouped by Experience"
        , names = c("Naive", "Experienced")
        , xlab = "Prevous Experience"
        , ylab = "Total Worker Mortality"
        , range = 0
        , col = exp_colors[pres_levels_h2]
)

### SHOW THE RAW DATA POINTS ON TOP
stripchart(mortalityh2 ~ previous_experience,
           data   = droplevels(sim_images_h2_df[41:80, ])
           , add    = TRUE
           , method = "jitter"
           , pch    = 19
           , col    = adjustcolor("gray23", alpha.f = 0.4)
           , vertical = TRUE
)

### STATISTICAL TEST
# Non-parametric test, seeing if the two groups have a 
# -> statistically significant difference
wilcox.test(mortalityh2 ~ exph2, data = sim_images_h2_df)



### BOX PLOTS OF BROOD LOSS GROUPED BY EXPERIENCE
boxplot(broodlossh2 ~ previous_experience
        , data = droplevels(sim_images_h2_df[41:80, ])
        , main = "[SIMULATED]  [H2]\nTotal Brood Loss Grouped by Experience"
        , names = c("Naive", "Experienced")
        , xlab = "Prevous Experience"
        , ylab = "Brood Loss"
        , col = exp_colors[pres_levels_h2]
        , range = 0
)

### SHOW THE RAW DATA POINTS ON TOP
stripchart(broodlossh2 ~ previous_experience,
           data   = droplevels(sim_images_h2_df[41:80, ])
           , add    = TRUE
           , method = "jitter"
           , pch    = 19
           , col    = adjustcolor("gray23", alpha.f = 0.4)
           , vertical = TRUE
)

### STATISTICAL TEST

# Non-parametric test, seeing if the two groups have a 
# -> statistically significant difference
wilcox.test(broodlossh2 ~ exph2, data = sim_images_h2_df)





### END :) -------------------------------------------------------------




