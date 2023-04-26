library(tidyverse)
#setwd("~/R/Second Serve Article BSA")

double_fault_per_second_serve <- read.csv('double_fault_per_second_serve.csv')
second_serve_speed <- read.csv('second_serve_speed.csv')
# Joining the dataframes
second_serve_speed_df_rate <- inner_join(double_fault_per_second_serve, 
                                         second_serve_speed,
                                         by = c("name", "country_name", "country_id"))
# Selecting DF Rate/Second Serve and Serve Speed Columns
second_serve_speed_df_rate <- second_serve_speed_df_rate %>% select(name, country_name, value.x, value.y) %>%
  rename("df_per_second_serve" = value.x, "serve_speed" = value.y)
# Changing DF Rate/Second Serve into Numeric Column
second_serve_speed_df_rate$df_per_second_serve <-
  as.numeric(gsub("%$", "", second_serve_speed_df_rate$df_per_second_serve))
# Changing Serve Speed into Numeric Column
second_serve_speed_df_rate[[4]] <-
  as.numeric(gsub(" km/h", "", second_serve_speed_df_rate[[4]]))

summary(lm(df_per_second_serve ~ serve_speed, second_serve_speed_df_rate))

write.csv(second_serve_speed_df_rate, file = "second_serve_speed_df_rate.csv")

# Make graph showing linear relationship between serve speed and df rate
ggplot(second_serve_speed_df_rate, aes(x = serve_speed, 
                                       y = df_per_second_serve)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(x = "Second Serve Speed (km/hr)", y = "Double Fault % Per Second Serve", 
       title = "Serve Speed is Related to Double Fault Rate") +
  theme(plot.title=element_text(hjust=0.5))

mean(second_serve_speed_df_rate$serve_speed)

cor(second_serve_speed_df_rate$df_per_second_serve, 
    second_serve_speed_df_rate$serve_speed)


# First serve analysis
first_serve_won_percentage <- read.csv('first_serve_won_percentage.csv')
first_serve_percentage <- read.csv('first_serve_percentage.csv')
second_serve_won_percentage <- read.csv('second_serve_won_percentage.csv')
# renaming columns
first_serve_won_percentage <- first_serve_won_percentage %>% 
    rename("first_serve_won" = value) %>%
    select(name, first_serve_won)
first_serve_percentage <- first_serve_percentage %>%
  rename("first_serve_made" = value) %>%
  select(first_serve_made)
second_serve_won_percentage <- second_serve_won_percentage %>%
  rename("second_serve_won" = value) %>%
  select(second_serve_won)

serve_optimization <- cbind(first_serve_won_percentage, 
                           first_serve_percentage, 
                         second_serve_won_percentage)
# changing all the percentages to numeric
for (i in 2:4) {
  serve_optimization[, i] <- as.numeric(
    gsub("%", "", serve_optimization[[i]])
  )
}

# Create column of predicted second serve percentage if two first serves
serve_optimization <- serve_optimization %>%
  mutate(pred_second_serve = (first_serve_won * first_serve_made * 0.01))

# Which players should serve harder?
players_to_serve_harder <- serve_optimization[serve_optimization$pred_second_serve >
                                         serve_optimization$second_serve_won, ]
serve_optimization <- serve_optimization %>%
  mutate(serve_harder = pred_second_serve > second_serve_won)

# Create plot of second serve vs predicted second serve with two firsts
ggplot(serve_optimization, aes(x = second_serve_won, 
                               y = pred_second_serve)) +
  geom_point() + 
  geom_point(data = players_to_serve_harder,
             aes(x = second_serve_won, 
                 y = pred_second_serve),
             col = 'red',
             size = 2) +
  geom_text(data = players_to_serve_harder,
            label = players_to_serve_harder$name,
            nudge_x = -2.5,
            check_overlap = TRUE
            ) +
  geom_abline(intercept = 0, slope = 1, col = 'blue', linetype = "dashed") +
  xlim(30, 60) +
  labs(x = "Actual % of Points Won on Second Serve",
       y = "% of Points Won with Another First Serve",
       title = "Should Players Hit Two First Serves?",
       subtitle = "Most players would be drastically worse off") +
  theme(plot.title= element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  scale_fill_manual(values = c('green', 'red'))

# Function to scale values from 0 to 1
my_scale <- function(x) {
  (x - min(x))/(max(x) - min(x))
}

my_scale(serve_optimization$first_serve_won)  
my_scale(serve_optimization$first_serve_made)

second_serve_speed_df_rate <- second_serve_speed_df_rate %>%
  mutate(second_serve_made = 100 - df_per_second_serve)

second_serve_made_df <- select(second_serve_speed_df_rate, c(second_serve_made, name))
serve_optimization <- inner_join(serve_optimization,
                                 second_serve_made_df,
                                 by = c("name"))

# Import data for percentage of pts won on first serves
serve_in_play <- read.csv("in_play_pts_won.csv")
serve_in_play <- serve_in_play %>%
  rename("pts_won_in_play" = value) %>%
  select(name, pts_won_in_play)
serve_in_play$pts_won_in_play <- as.numeric(
  gsub("%", "", serve_in_play$pts_won_in_play))

# Join data from both dataframe
serve_optimization <- inner_join(serve_optimization,
                                 serve_in_play,
                                 by = c("name"))
# Make plot depicting points won by first serves made
ggplot(serve_optimization) +
  geom_point(aes(y = my_scale(serve_optimization$pts_won_in_play),
                 x = my_scale(serve_optimization$first_serve_made))) +
  geom_vline(xintercept = 0.5, linetype = "dashed", col = 'red') +
  geom_hline(yintercept = 0.5, linetype = "dashed", col = 'red') +
  labs(x = "Scaled % of First Serve Made", y = "Scaled % of Points in Play Won",
       title = "% of First Serves Made by % of Points in Play Won") +
  theme(plot.title=element_text(hjust=0.5))


# Get coordinates of every players in chart
get_coords <- function(x, vecx, y, vecy) {
  coords <- numeric(2)
  coords[1] <- (x - min(vecx))/(max(vecx) - min(vecx))
  coords[2] <- (y - min(vecy))/(max(vecy) - min(vecy))
  coords
}

# Get the players that should serve harder and make a table out of them
fourth_quad <- character(0)
for (i in seq_along(serve_optimization$name)) {
  values <- get_coords(serve_optimization[i, 3], serve_optimization[, 3], 
                   serve_optimization[i, 8], serve_optimization[, 8])
  if (values[1] > 0.5 && values[2] < 0.5) {
    fourth_quad <- c(fourth_quad, serve_optimization[i, 1])
  }
}

fq_df <- as.data.frame(fourth_quad)
view(fq_df)
library(scales)
gt(fq_df) %>%
  tab_header(title = "Players in 4th Quadrant") %>%
  tab_options(table.align = "left", column_labels.hidden = TRUE,
              table.width = 600)
  





