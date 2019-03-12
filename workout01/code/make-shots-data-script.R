#############################
# title: make shots data script
# description: This script is used to contain the required variables for visualization
# input: no inputs required
# output: summary .txt files for each player and all players together in the output directory, and a csv file of all players data together in the data directory 
#############################

##################download all of the files
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)

##################add player column with their names
curry$name <- rep('Stephen Curry', length(curry$team_name))
iguodala$name <- rep('Andre Iguodala', length(iguodala$team_name))
green$name <- rep('Draymond Green', length(green$team_name))
thompson$name <- rep('Klay Thompson', length(thompson$team_name))
durant$name <- rep('Kevin Durant', length(durant$team_name))

###################change shot_made_flag to shot_no and shot_yes
curry$shot_made_flag[curry$shot_made_flag == 'n'] <- 'shot_no'
curry$shot_made_flag[curry$shot_made_flag == 'y'] <- 'shot_yes'

iguodala$shot_made_flag[iguodala$shot_made_flag == 'n'] <- 'shot_no'
iguodala$shot_made_flag[iguodala$shot_made_flag == 'y'] <- 'shot_yes'

green$shot_made_flag[green$shot_made_flag == 'n'] <- 'shot_no'
green$shot_made_flag[green$shot_made_flag == 'y'] <- 'shot_yes'

thompson$shot_made_flag[thompson$shot_made_flag == 'n'] <- 'shot_no'
thompson$shot_made_flag[thompson$shot_made_flag == 'y'] <- 'shot_yes'

durant$shot_made_flag[durant$shot_made_flag == 'n'] <- 'shot_no'
durant$shot_made_flag[durant$shot_made_flag == 'y'] <- 'shot_yes'

##################add minute column
curry$minute <- curry$period * 12 - curry$minutes_remaining
iguodala$minute <- iguodala$period * 12 - iguodala$minutes_remaining
green$minute <- green$period * 12 - green$minutes_remaining
thompson$minute <- thompson$period * 12 - thompson$minutes_remaining
durant$minute <- durant$period * 12 - durant$minutes_remaining

#################sink summaries to the output directory

##curry
sink('../output/stephen-curry-summary.txt')
summary(curry)
sink()

##iguodala
sink('../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()

##green
sink('../output/draymond-green-summary.txt')
summary(green)
sink()

##thompson
sink('../output/klay-thompson-summary.txt')
summary(thompson)
sink()

##durant
sink('../output/kevin-durant-summary.txt')
summary(durant)
sink()

######################stack tables
make_shots_data <- rbind(iguodala, green, durant,thompson, curry)

#####################export this table to data directory
shots_data <- write.csv(make_shots_data, '../data/shots-data.csv')

#####################export the summary of shots_data to data directory
sink('../output/shots-data-summary.txt')
summary(make_shots_data)
sink()
