#############################
# title: make shots chart script
# description: This script is used to create shot charts
# input: no inputs required
# output: summary .txt files for each player in the output directory
#############################

##load libraries
library(jpeg)
library(grid)
library(ggplot2)
library(dplyr)

##read in each player's information
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
shot_data <- read.csv("../data/shots-data.csv", stringsAsFactors = FALSE)

##change the shot_made_flag to more specific values
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

##get rid of all the players' shots that are past y=420
curry <- filter(curry, y <= 420)
iguodala <- filter(iguodala, y <= 420)
green <- filter(green, y <= 420)
thompson <- filter(thompson, y <= 420)
durant <- filter(durant, y <= 420)
#do the same for shot_data
shot_data <- filter(shot_data, y <= 420)


##court image to be used
court_file <- '../images/nba-court.jpg'
##create raste object
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, 'npc'),
  height = unit(1, 'npc')
)

###################### 4.1: create shot charts for each player
##klay
make_klay_shot_chart <- ggplot(data=thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()
#save this chart as a pdf with size specifications in the images directory
klay_shot_chart <- ggsave('../images/klay-thompson-shot-chart.pdf', plot = make_klay_shot_chart, device = 'pdf', width = 6.5, height = 5, units = 'in')

##curry
make_curry_shot_chart <- ggplot(data=curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()
#save this chart as a pdf with size specifications in the images directory
curry_shot_chart <- ggsave('../images/stephen-curry-shot-chart.pdf', plot = make_curry_shot_chart, device = 'pdf', width = 6.5, height = 5, units = 'in')

##green
make_green_shot_chart <- ggplot(data=green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()
#save this chart as a pdf with size specifications in the images directory
green_shot_chart <- ggsave('../images/draymond-green-shot-chart.pdf', plot = make_green_shot_chart, device = 'pdf', width = 6.5, height = 5, units = 'in')

##durant
make_durant_shot_chart <- ggplot(data=durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()
#save this chart as a pdf with size specifications in the images directory
durant_shot_chart <- ggsave('../images/kevin-durant-shot-chart.pdf', plot = make_durant_shot_chart, device = 'pdf', width = 6.5, height = 5, units = 'in')

##iguodala
make_iguodala_shot_chart <- ggplot(data=iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()
#save this chart as a pdf with size specifications in the images directory
durant_shot_chart <- ggsave('../images/andre-iguodala-shot-chart.pdf', plot = make_iguodala_shot_chart, device = 'pdf', width = 6.5, height = 5, units = 'in')


##################### 4.2: create faceted shot chart
##create the facet grid
faceted_grid <- ggplot(shot_data) + 
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ggtitle('Shot Charts: GSW (2016 season)') +
  theme_minimal() +
  facet_wrap(.~name)
##save as a pdf in images/ directory
gsw_shot_chart <- ggsave('../images/gsw-shot-chart.pdf', plot = faceted_grid, device = 'pdf', width = 8, height = 7, units = 'in')
##save as png in images/ directory
gsw_shot_chart_png <- ggsave('../images/gsw-shot-chart.png', plot = faceted_grid, device = 'png', width = 8, height = 7, units = 'in')
