# Get the working directory of the enviornment.
getwd()

library(plyr); library(dplyr)
library(ggplot2)
library(scales)
library(data.table)
library(RColorBrewer)
library(stringr)
library(viridis)
library(plotly)
library(tidyr)
library(cowplot)
library(ggpubr)
library(flexdashboard)


# Assign and store data location and read into dataframe
library("data.table")
freethrows_loc = "/Users/nicholascampa/Desktop/IS460/R_datafiles/free_throws.csv"
freethrows_df = fread(freethrows_loc)

# Let's separate the Final Score and Score during free throws, for each respective team;
# we'll also split the teams into a home and away column to make sorting by team easier. 
library(stringr)
library(tidyr)
freethrows_df = tidyr::separate(freethrows_df, end_result, c("End-Away", "End-Home"), 
                                sep = "-", remove = TRUE)
freethrows_df = tidyr::separate(freethrows_df, game, c("Away-Team", "Home-Team"), 
                                sep = "-", remove = TRUE)
freethrows_df = tidyr::separate(freethrows_df, score, c("Score-Away", "Score-Home"), 
                                sep = "-", remove = TRUE)
freethrows_df = tidyr::separate(freethrows_df, time, c("Minutes_Remaining", "Seconds_Remaining"), 
                                sep = ":", remove = TRUE)

# get to know data
str(freethrows_df)
dim(freethrows_df) # There are 618,019 rows and 14 columns

# we can typecast the scores to int to calculate averages and summary statistics
freethrows_df$`End-Away` = as.numeric(freethrows_df$`End-Away`)
freethrows_df$`End-Home` = as.numeric(freethrows_df$`End-Home`)
freethrows_df$`Score-Away` = as.numeric(freethrows_df$`Score-Away`)
freethrows_df$`Score-Home` = as.numeric(freethrows_df$`Score-Home`)
freethrows_df$Minutes_Remaining = as.numeric(freethrows_df$Minutes_Remaining)
freethrows_df$Seconds_Remaining = as.numeric(freethrows_df$Seconds_Remaining)
freethrows_df$season = as.factor(freethrows_df$season)


summary(freethrows_df)

length(unique(freethrows_df$season)) # There are 10 unique seasons, 2006-2007 -- 2015-2016
length(unique(freethrows_df$player)) # There are 1,098 unique players

# --------------------------------- Bar chart for top 10 players -------------------------------------------

library(viridis)
library(plyr); library(plyr)
library(ggplot2)

ft_count = data.frame(plyr::count(freethrows_df$player))
colnames(ft_count) = c("Player", "FTA")
ft_count = ft_count[order(-ft_count$FTA), ]

p1 = ggplot(ft_count[1:20,], aes(x = reorder(Player, FTA), y = FTA, fill = as.factor(reorder(Player, -FTA)))) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) + 
  theme_light() +
  labs(title = "Top 20 Free Throw Attempts 06-16", x= "", y = "FTA") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() + 
  scale_fill_viridis_d(option = "E", direction = 1) + 
  guides(fill = guide_legend(title = "Player Name")) + 
  scale_y_continuous(labels = scales::comma)

# --------------------FT% by minutes remaining line chart for playoffs / non-playoffs -----------------------------------------

# get regular season made ft counts by mins remaining
regular_madefts = freethrows_df %>%
  filter(playoffs == "regular" & shot_made == 1 & Minutes_Remaining < 13) %>%
  select(Minutes_Remaining, shot_made) %>%
  group_by(Minutes_Remaining) %>%
  dplyr::summarise(MadeFT = length(Minutes_Remaining), .groups = 'keep') %>%
  data.frame()
# get reason miss ft counts by mins remaining
regular_missfts = freethrows_df %>%
  filter(playoffs == "regular" & shot_made == 0 & Minutes_Remaining < 13) %>%
  select(Minutes_Remaining, shot_made) %>%
  group_by(Minutes_Remaining) %>%
  dplyr::summarise(MissFT = length(Minutes_Remaining), .groups = 'keep') %>%
  data.frame()
# Rename MadeFTs Dataframe object
regular_season_fts = regular_madefts
# Add Miss Column to Make DF
regular_season_fts$MissFT = regular_missfts$MissFT
regular_season_fts$Percentage = regular_season_fts$MadeFT / (regular_season_fts$MadeFT + regular_season_fts$MissFT)
# Sort by Mins Remaining descending
regular_season_fts = regular_season_fts[order(regular_season_fts$Minutes_Remaining, decreasing = TRUE), ]

# get playoffs season made ft counts by mins remaining
playoffs_madefts = freethrows_df %>%
  filter(playoffs == "playoffs" & shot_made == 1 & Minutes_Remaining < 13) %>%
  select(Minutes_Remaining, shot_made) %>%
  group_by(Minutes_Remaining) %>%
  dplyr::summarise(MadeFT = length(Minutes_Remaining), .groups = 'keep') %>%
  data.frame()
# get reason miss ft counts by mins remaining
playoffs_missfts = freethrows_df %>%
  filter(playoffs == "playoffs" & shot_made == 0 & Minutes_Remaining < 13) %>%
  select(Minutes_Remaining, shot_made) %>%
  group_by(Minutes_Remaining) %>%
  dplyr::summarise(MissFT = length(Minutes_Remaining), .groups = 'keep') %>%
  data.frame()
# Rename MadeFTs Dataframe object
playoffs_fts = playoffs_madefts
# Add Miss Column to Make DF
playoffs_fts$MissFT = playoffs_missfts$MissFT
playoffs_fts$Percentage = playoffs_fts$MadeFT / (playoffs_fts$MadeFT + playoffs_fts$MissFT)
# Sort by Mins Remaining descending
playoffs_fts = playoffs_fts[order(playoffs_fts$Minutes_Remaining, decreasing = TRUE), ]

# Add Playoffs FT% To Regular Season Dataframe
regular_season_fts$playoffs_pct = playoffs_fts$Percentage
# Rename Dataframe for Clarity, usability
minutes_remaining_percents = regular_season_fts
colnames(minutes_remaining_percents) = c("Minutes_Remaining", "MadeFT", "MissFT", "Regular_pct", "Playoffs_pct")
# Typecast Minutes Remaining to Factor 
minutes_remaining_percents$Minutes_Remaining = as.factor(minutes_remaining_percents$Minutes_Remaining)

minutes_remaining_percents

p2 = ggplot(minutes_remaining_percents, aes(x = Minutes_Remaining, group = 1)) + 
  geom_line(aes(y = Regular_pct, color = "Regular Season"), stat = "identity", group = 1) +
  geom_line(aes(y = Playoffs_pct, color = "Playoffs"), stat = "identity", group = 1) +
  geom_point(aes(y = Regular_pct), color = "steelblue", size = 2) + 
  geom_point(aes(y = Playoffs_pct), color = "salmon",size = 2) + 
  scale_x_discrete(limits = rev(levels(minutes_remaining_percents$Minutes_Remaining))) +
  scale_y_continuous(labels = scales::percent) + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank()) +
  labs(title = "Multiple Line Plot - FT% by Minutes Remaining", 
       x = "Minutes Remaining in Period", 
       y = "FT%") 
p2
  
# ---------------------------------------Heat Map for Free throws ---------------------------------------

freethrows_df$period = as.numeric(freethrows_df$period)
freethrows_df$Minutes_Remaining = as.numeric(freethrows_df$Minutes_Remaining)
heatmap_df = freethrows_df %>%
  select(period, Minutes_Remaining) %>%
  filter(period < 5 & Minutes_Remaining) %>%
  group_by(period, Minutes_Remaining) %>%
  dplyr::summarise(n=length(period), .groups = 'keep')

heatmap_df$period = as.numeric(heatmap_df$period)
heatmap_df$Minutes_Remaining = as.numeric(heatmap_df$Minutes_Remaining)
heatmap_df = heatmap_df[1:50, ]
heatmap_df = heatmap_df[-c(25,38), ]
missing_row = c("period" = 1,"Minutes_Remaining" = 12, "n" = 0)
heatmap_df = rbind(heatmap_df, missing_row)


heatmap_df$period = as.factor(heatmap_df$period)
heatmap_df$Minutes_Remaining = as.factor(heatmap_df$Minutes_Remaining)

p3 = ggplot(heatmap_df, aes(x=period, y=Minutes_Remaining, fill=n)) + 
  geom_tile(color="darkblue") + 
  geom_text(aes(label = scales::comma(n))) + 
  coord_equal(ratio = 0.3) + 
  labs(title = "Heatmap: Free Throw Attempts by Minutes Remaining in Period", 
       x="Period", y="Minutes Remaining in Period", fill = "FTA") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_continuous(low="white", high="green")
p3

# ------------------- Lattice x-Bar chart for FTA by Period by Season ------------------------------------ 

periods_by_seasons$period = as.numeric(periods_by_seasons$period)

periods_by_seasons = freethrows_df %>%
  select(period, season) %>%
  filter(period <= 4) %>%
  group_by(period, season) %>%
  dplyr::summarise(totFT = length(season), .groups = 'keep') %>%
  data.frame() %>%
  arrange(season)

periods_by_seasons$period = as.factor(periods_by_seasons$period)

p4 = ggplot(periods_by_seasons, aes(x = period, y = totFT, fill = season)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Multiple Bar Charts - Total Free Throws by Period by Season", 
       x = "Period", y = "FTA", fill = "Season") +
  scale_fill_brewer(palette = "Spectral") +
  facet_wrap(~season, nrow = 2)
p4



# -----------------------------------pie chart for FT% of entire dataset---------------------------------------

total_ft_pct = freethrows_df %>% group_by(shot_made) %>% tally() %>% data.frame()
total_ft_pct$shot_made = c("Miss", "Made")
colnames(total_ft_pct) = c("FT_Result", "n")
total_ft_pct

p5 = plot_ly(total_ft_pct, labels = ~FT_Result, values = ~n, type = 'pie', 
        textposition = "outside", textinfo = "label + percent") %>%
  layout(title = "Pie Chart - NBA FT% (2006-2016)")
p5
  
# Histogram for FTA entire dataset 

ft_pergame_count = data.frame(plyr::count(freethrows_df$game_id))
ft_pergame_count

ggplot(ft_pergame_count, aes(freq)) +
  geom_histogram(binwidth = 5, color = "darkgoldenrod2", fill = "cornflowerblue") + 
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Histogram - Free Throws Per Game", 
        x = "FTA", 
        y = "Frequency") +
  scale_y_continuous(labels = scales::comma)
  
  

# -----------------------------------------------Dashboards-------------------------------------------------------

p1 = ggplot(ft_count[1:20,], aes(x = reorder(Player, FTA), y = FTA, fill = as.factor(reorder(Player, -FTA)))) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) + 
  theme_light() +
  labs(title = "Top 20 Free Throw Attempts 06-16", x= "", y = "FTA") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() + 
  scale_fill_viridis_d(option = "E", direction = 1) + 
  guides(fill = guide_legend(title = "Player Name")) + 
  scale_y_continuous(labels = scales::comma)

p2 = ggplot(non_playoff_percents, aes(x=Minutes_Remaining, group = 1)) + 
  geom_line(aes(y=percentage, colour = "FT% (Reg. Season)"), stat = "identity", group = 1) + 
  geom_line(aes(y=playoff_pct, colour = "FT% (Playoffs)"), stat = "identity", group = 1) + 
  geom_point(aes(y=percentage), size=2, color = "steel blue") +
  geom_point(aes(y=playoff_pct), size=2, color = "darkred") +
  scale_x_discrete(limits = rev(levels(non_playoff_percents$Minutes_Remaining))) +
  scale_y_continuous(labels = scales::percent, 
                     limits = c(.73, .78)) +
  theme_light() +
  labs(title = "FT% by Minutes Remaining in Period", 
       x = "Minutes Remaning in Period", 
       y = "Free Throw", 
       color = "Legend") +
  theme(plot.title = element_text(hjust = 0.5))

p3 = ggplot(heatmap_df, aes(x=period, y=Minutes_Remaining, fill=n)) + 
  geom_tile(color="darkblue") + 
  geom_text(aes(label = scales::comma(n))) + 
  coord_equal(ratio = 0.3) + 
  labs(title = "Heatmap: Free Throw Attempts by Minutes Remaining in Period", 
       x="Period", y="Minutes Remaining in Period", fill = "FTA") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.2)) + 
  scale_fill_continuous(low="white", high="orange")

p4 = ggplot(periods_by_seasons, aes(x = period, y = totFT, fill = season)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Multiple Bar Charts - Total Free Throws by Period by Season", 
       x = "Period", y = "FTA", fill = "Season") +
  scale_fill_brewer(palette = "Spectral") +
  facet_wrap(~season, nrow = 2)

# ggplot
# plot_type
# scale x
# scale y 
# theme
# theme settings
# labs
# guides
# scale fill brewer


p1
p2
p3
p4
p5

library(cowplot)

plot_grid(p1, p2, p3, p4,
          nrow = 2)

library(ggpubr)

ggarrange(p1, p2, p3, p4, nrow = 2, ncol=2) 

library(flexdashboard)




 



