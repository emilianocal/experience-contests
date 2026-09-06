## Emiliano Calvo Alcaniz, Anna Dornhaus
## (c) 2026
## What is the role of experience in determining collective contest success?

## Packages ----------------------------
library("googledrive")
library("tidyverse")

## Connect to google drive -------------------------
# Run this code, then log into google drive in order to access the data sheet
drive_auth()

## Import data sheets -----------------------------
# Download images datasheet
drive_download("images-collective-contests", type = "csv", overwrite = TRUE)
# rename to something easier to use
# imagesdf contains colony information, mortality rate, and brood loss
imagesdf <- read.csv("images-collective-contests.csv")

# Download behavior datasheet
drive_download("video-analysis-collective-contest", type = "csv", overwrite = TRUE)
# rename to easier name
# contains behaviors at different time points
behaviordf <- read.csv("video-analysis-collective-contest.csv")

## Data wrangling for imagesdf ----------------------------------
### Factorize
# Turn the experience column into a factor instead of characters
factorimagesdf <- imagesdf |>
  mutate(previous_experience = factor(previous_experience
                                      , levels = c(0, 2)
                                      , labels = c("Naive", "Experienced")))

### Mortality data wrangling --------------------------------
# Remove totally unnecessary columns
mortdf <- factorimagesdf |>
  dplyr::select(!c(color, notes))

# Filter out the contests where individuals have some experience 
mortdf <- mortdf %>%
  group_by(contest_id) %>%
  filter(
    !any(previous_experience == "Some Experience") &
      !all(previous_experience == "Naive")
  ) %>%
  ungroup()

## Data wrangling for behaviordf -----------------------------------

# Remove unnecessary columns
behaviordf <- behaviordf|> 
  dplyr::select(!c(frame_number, secondary_workers_in_primary_nest
                   , time_stamp..min., contest_start_time, notes, brood_care_in_arena)) |>
  rename(colony_id = primary_colony_id)


# Create a df with just the information that will be used to join to behaviordf
prev_xp_columns <- imagesdf |>
  dplyr::select(c(contest_id, colony_id, previous_experience, total_mortality, worker_number))

# Add experience column to behavior df
behaviordf <- left_join(behaviordf, prev_xp_columns, by = c("colony_id", "contest_id"))

# Remove the intermediate contests
behaviordf <- behaviordf |>
  dplyr::filter_out(previous_experience == "1")


# collapse all the individual observances into an overall average for the contest
behaviordf <- behaviordf |>
  # group by file name to make the next conversions easier
  dplyr::group_by(file_name) |>
  # collapse the observances into one row based on the average of each integer
  dplyr::summarise(
    # leave contest_id untouched
    contest_id = dplyr::first(contest_id)
    # sum tandem_runs instead of averaging it
    # average all other integer columns, excluding tandem_runs
    , tandem_runs = sum(tandem_runs, na.rm = TRUE)
    , `grappling_.pairs.` = sum(`grappling_.pairs.`, na.rm = TRUE)
    # across(where(is.integer)) applies the mean function to all number columns
    # ~mean is shorthand to create an averaging function, where .x is a placeholder
    # for whichever of the columns is being actively calculated
    # NAs get removed so the averaging doesn't explode
    , dplyr::across(
      where(is.integer) & !dplyr::any_of(c("tandem_runs", "grappling_.pairs."))
      , ~mean(.x, na.rm = TRUE)
    )
  )


# remove the part of the file_name that's the contest code
behaviordf <- behaviordf |>
  dplyr::mutate(file_name = substr(file_name, 10, nchar(file_name)))

# rename to primary_colony so the next transformation is a little more intuitive
behaviordf <- behaviordf |>
  dplyr::rename(primary_colony = file_name)


# secondary-perspective stats, relabeled so we can join them to the "other" colony's row
other <- behaviordf |>
  dplyr::select(contest_id, primary_colony,
                number_of_secondary_workers_out,
                number_secondary_workers_pinning,
                number_sec_workers_pinned) |>
  dplyr::rename(
    other_colony                    = primary_colony,
    other_secondary_workers_out     = number_of_secondary_workers_out,
    other_secondary_workers_pinning = number_secondary_workers_pinning,
    other_sec_workers_pinned        = number_sec_workers_pinned
  )

behaviordf <- behaviordf |>
  dplyr::left_join(other, by = "contest_id", relationship = "many-to-many") |>
  dplyr::filter(primary_colony != other_colony) |>
  dplyr::mutate(
    number_of_primary_workers_out  = (number_of_primary_workers_out  + other_secondary_workers_out)     / 2
    , number_primary_workers_pinning = (number_primary_workers_pinning + other_secondary_workers_pinning)  / 2
    , number_pri_workers_pinned      = (number_pri_workers_pinned      + other_sec_workers_pinned)         / 2
  ) |>
  dplyr::select(-other_colony, -other_secondary_workers_out, -other_secondary_workers_pinning
                , -other_sec_workers_pinned, -number_of_secondary_workers_out
                , -number_secondary_workers_pinning, -number_sec_workers_pinned)


#### Combine/ collaps all the colonies information so each colony only has one line
behaviordf <- behaviordf |>
  dplyr::mutate(
    dplyr::across(
      c(number_of_primary_workers_out, number_primary_workers_pinning
        , number_pri_workers_pinned, number_workers_guarding)
      , ~ .x / worker_number # this line takes the mean so you get a proportion
    )
  )


# Convert experience to a factor for grouping
behaviordf$previous_experience <- factor(behaviordf$previous_experience
                                          , levels = c(0, 2)
                                          , labels = c("Naive", "Experienced"))

# Rename all the columns to easier names
behaviordf <- behaviordf |>
  dplyr::rename(prop_workers_out = number_of_primary_workers_out) |>
  dplyr::rename(prop_workers_pinning = number_primary_workers_pinning) |>
  dplyr::rename(prop_workers_pinned = number_pri_workers_pinned) |>
  dplyr::rename(prop_workers_guarding = number_workers_guarding) |>
  dplyr::rename(grappling = grappling_.pairs.)

# Add up the grappling pairs (grouped by contests) and then divide by 2
# (since half of the grappling pairs should be from each colony)
# and then divide by number of workers to get the proportion
behaviordf <- behaviordf |>
  dplyr::group_by(contest_id) |>
  dplyr::mutate(
    grappling = (sum(grappling, na.rm = TRUE) / 2) / worker_number
  ) |>
  dplyr::ungroup()

# rename the grappling to the proportion of grapplers in each colony
behaviordf <- behaviordf |> 
  dplyr::rename(prop_grappling = grappling)


behaviordf_graph <- behaviordf |> 
  dplyr::select(c(contest_id, prop_workers_out, prop_workers_pinning, prop_workers_pinned
                   , prop_workers_guarding, tandem_runs, previous_experience))

# Whoops forgot to remove the contests where both are naive
behaviordf_graph <- behaviordf_graph |>
  dplyr::group_by(contest_id) |>
  dplyr::filter(!all(previous_experience == "Naive")) |>
  dplyr::ungroup()




## Figures ----------------------------------
# Set parameters for the figures
 # set plots to have a transparent background
   # par(bg = "transparent")
 # set font for plots 
   # par(family = "serif")

### Mortality figure ------------------------------
boxplot(total_mortality ~ previous_experience, data = mortdf,
        range = 0
        , col = c("blue", "green")
        , xlab = "Experience Level" 
        , ylab = "Worker Mortality Count" 
        , main = "Worker Mortality Following Contest"
        , cex.main = 2
        , cex.lab = 1.3)
stripchart(total_mortality ~ previous_experience, data = mortdf
           , method = "jitter"
           , jitter = 0.15
           , vertical = TRUE
           , pch = 19
           , col = rgb(0, 0, 0, 0.5)
           , add = TRUE)

### Brood loss figure -------------------------------

boxplot(brood_loss ~ previous_experience, data = imagesdf
        , range = 0
        , col = c("blue", "green")
        , xlab = "Experience Level"
        , ylab = "Number of Brood Lost positive = loss negative = gain"
        , main = "Change in Brood After Contest"
        , cex.main = 1
        , cex.lab = 1)

stripchart(brood_loss ~ previous_experience, data = imagesdf
           , method = "jitter", jitter = 0.15
           , vertical = TRUE, pch = 19
           , col = rgb(0, 0, 0, 0.5)
           , add = TRUE)


### Behavior boxplot figure --------------------------------
# identify numeric columns to plot (excluding the grouping variable itself)
numeric_cols <- names(behaviordf_graph)[sapply(behaviordf_graph, is.numeric)]
numeric_cols <- setdiff(numeric_cols
                        , c("previous_experience", "worker_num"
                            , "grappling_.pairs.", "tandem_runs"))



# create readable labels: replace underscores with spaces, title case
label_map <- c(
  number_of_primary_workers_out  = "Workers \nin Arena"
  , number_primary_workers_pinning = "Workers Pinning \nOpponents"
  , number_pri_workers_pinned = "Workers \nBeing Pinned"
  , number_workers_guarding        = "Workers Guarding \nNest Entrance"
)

# apply the mapping to numeric_cols, in order
metric_labels <- label_map[numeric_cols]

# variables you want to compare (renamed from vars because it broke when i didn't)
plot_vars <- c("prop_workers_out", "prop_workers_pinning",
               "prop_workers_pinned", "prop_workers_guarding")

# Make long to compare more easily
long_behaviordf_graph <- data.frame(
  value    = unlist(behaviordf_graph[plot_vars]),
  variable = rep(plot_vars, each = nrow(behaviordf_graph)),
  group    = rep(behaviordf_graph$previous_experience, times = length(plot_vars))
)
par(mar = c(5, 4, 4, 2))


boxplot(value ~ group + variable
        , data = long_behaviordf_graph
        , range = 0
        , col = c("#C6808A", "#861657")
        , xaxt = "n"
        , xlab = ""
        , ylab = "Proportion of Workers"
        , main = "Worker Behaviors by Experience Level"
        , cex.main = 1.5)

stripchart(value ~ group + variable,
           data = long_behaviordf_graph,
           vertical = TRUE,
           method = "jitter",
           jitter = 0.15,
           pch = 16,
           col = rgb(0, 0, 0, 0.5),
           add = TRUE)

# calculate label positions (centered under each metric's pair of boxes)
metric_positions <- seq(1.5, by = 2, length.out = length(numeric_cols))

# draw angled labels manually using text() instead of axis()
text(x = metric_positions,
     y = par("usr")[3] - 0.08 * diff(par("usr")[3:4]),
     labels = label_map,
     srt = 0,        # rotation angle in degrees
     adj = .5,         # right-justify so label ends point at the tick
     xpd = TRUE,       # allow drawing outside the plot region
     cex = 1.2)

legend("topright", legend = levels(behaviordf_graph$previous_experience),
       x.intersp = 0.5,
       fill = c("#C6808A", "#861657"))

par(mar = c(5, 4, 4, 2) + 0.1)  # reset margins to default


### Behavior v outcome figures --------------------------------













## Statistics ----------------------------------
# Mortality
kruskal.test(total_mortality ~ previous_experience, data = mortdf)

# Brood loss
kruskal.test(brood_loss ~ previous_experience, data = imagesdf)




## END ----------------------------