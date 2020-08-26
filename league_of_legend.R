#This .csv file contains approx. 10k ranked games (from high Diamond to low Master). 
#Each row is a unique game and features have been collected after the first 10 minutes of
#the game. Column gameId can be used to collect more attributes from Riot API, 
#column blueWins is the target.
install.packages("ggcorrplot")
library(tidyverse)
library(ggcorrplot)

games <- read_csv("high_diamond_ranked_10min.csv")

#A.--------------------------Understand dataset and varibles--------------------------------

#blueWins:           1 if the blue team has won, 0 otherwise. (target)
#blueWardsPlaced:    Number of warding totems placed by the blue team on the map
#blueWardsDestroyed: Number of enemy warding totems the blue team has destroyed
#blueFirstBlood:     First kill of the game. 1 if the blue team did the first kill, 0 otherwise
#blueKills:          Number of enemies killed by the blue team
#blueDeaths:         Number of deaths (blue team)
#blueAssists:        Number of kill assists (blue team)
#blueEliteMonsters:  Number of elite monsters killed by the blue team (Dragons and Heralds)
#blueDragons:        Number of dragons killed by the blue team
#blueHeralds:        Number of heralds killed by the blue team
#blueTowersDestroyed:Number of structures destroyed by the blue team (towers...)
#blueTotalGold:      Blue team total gold
#blueAvgLevel:       Blue team average champion level
#blueTotalExperience:   Blue team total experience
#blueTotalMinionsKilled:    Blue team total minions killed (CS)
#blueTotalJungleMinionsKilled:    Blue team total jungle monsters killed
#blueGoldDiff:       Blue team gold difference compared to the enemy team
#blueExperienceDiff: Blue team experience difference compared to the enemy team
#blueCSPerMin:       Blue team CS (minions) per minute
#blueGoldPerMin:     Blue team gold per minute
#redWardsPlaced:     Number of warding totems placed by the red team on the map
#redWardsDestroyed:  Number of enemy warding totems the red team has destroyed
#redFirstBlood:      First kill of the game. 1 if the red team did the first kill, 0 otherwise
#redKills:           Number of enemies killed by the red team
#redDeaths:          Number of deaths (red team)
#redAssists:         Number of kill assists (red team)
#redEliteMonsters:   Number of elite monsters killed by the red team (Dragons and Heralds)
#redDragons:         Number of dragons killed by the red team
#redHeralds:         Number of heralds killed by the red team
#redTowersDestroyed: Number of structures destroyed by the red team (towers...)
#redTotalGold:       Red team total gold
#redAvgLevel:        Red team average champion level
#redTotalExperience: Red team total experience
#redTotalMinionsKilled:   Red team total minions killed (CS)
#redTotalJungleMinionsKilled:   Red team total jungle monsters killed
#redGoldDiff:        Red team gold difference compared to the enemy team
#redExperienceDiff:  Red team experience difference compared to the enemy team
#redCSPerMin:        Red team CS (minions) per minute
#redGoldPerMin:      Red team gold per minute


#B.-----------------------------------------Data Preparation--------------------------------------
# Drop repeated columns
games <- games %>% 
  select(-c(gameId,redGoldDiff,redExperienceDiff))


# Convert data types
games <- games %>% 
  mutate(blueWins = factor(blueWins,
                           levels = c(0,1)),
         blueFirstBlood = factor(blueFirstBlood, 
                                 levels = c(0,1)),
         redFirstBlood = factor(redFirstBlood, 
                                 levels = c(0,1)))

#check for NA values
sum(is.na(games)) 
str(games)



#C.---------------------------Univariate Analysis: Analyze varibles one by one--------------------------

#distributions of all continuous varibles
games %>% 
  keep(is.numeric) %>%                     
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +     # In separate panels
  geom_density()                           # as density


games %>% 
  keep(is.factor) %>%                     
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +     # In separate panels
  geom_bar()+                              # as bar plots
  geom_text(stat = "count",aes(label=..count..), vjust=-0.25)




#D.---------------------Bivariate Analysis: Explore relationship between varibles----------------------
#***Pair Grids to check relationships between parameters of blue teams



#***Correlation matrix analysis
corr <- games %>% 
  select(-c(blueFirstBlood,redFirstBlood,blueWins)) %>% 
  cor() %>% 
  round(1)

# plot heatmap of correlation
ggcorrplot(corr, 
           hc.order = TRUE,          # Using hierarchical clustering
           type = "lower",           # Keep the lower triangle 
           outline.col = "white")    # Outliner color white



#*** Dive deeper in some relationships

# How amount of gold and kills determine the result
games %>% 
  ggplot(mapping = aes(x=blueTotalGold, y=blueKills))+ 
    geom_point(aes(col=blueWins, alpha=1/10))+
    ggtitle("how amount of gold and kills at minute 10 determine the result")
    #questions: 


# How first kill affect the end result
games %>% 
  ggplot(mapping = aes(x=blueFirstBlood))+ 
    geom_bar(aes(fill=blueWins))+ 
    ggtitle("how first kill before first 10 mins affect the end result")


# How blueDeaths affect the end result
games %>% 
  ggplot(mapping = aes(x=blueDeaths))+ 
    geom_bar(aes(fill=blueWins))+ 
    ggtitle("how blueDeaths before first 10 mins affect the end result")
  #=> the more deaths, the likelier of losing.


# Blue Deaths and Red Deaths
games %>%
  count(blueDeaths,redDeaths) %>% 
  ggplot(mapping = aes(x=blueDeaths, y=redDeaths))+ 
  geom_tile(aes(fill=n))+
  ggtitle("Red deaths, Blue Deaths") 
  #suggest a pattern?


games %>%
  count(blueAssists,redDeaths) %>% 
  ggplot(mapping = aes(x=blueAssists, y=redDeaths))+ 
  geom_tile(aes(fill=n))+
  ggtitle("Blue Assists and Red Deaths relationship")
  #=> the more blue assist, the more deaths to red team, this is obvious but there's insights:
  #   trend 1: low deaths and a lot assists => blueteam work collaborately to kill an enemy
  #   trend 2: high deaths and a few assists => some good players kill enemies by themselves without help from team
  #   trend 3: a lot dedaths, a lot assists => team-work works very well, collaborations to defear enemies.
  #   the light blue color indicates that there's not a lot of observations showing relatively low death and low assist
  #=> feature engineering: divide observations into combat style based on assists and deaths counts


games %>% #structures destroyed by the blue team in the first 10 mins affect the result
  ggplot(aes(blueTowersDestroyed))+
  geom_bar(aes(fill=blueWins))+
  #coord_cartesian(ylim = c(0,500))+
  ggtitle("structures destroyed by the blue team in the first 10 mins affect the result")
  #=> there's a high chance of losing if no tower were destroyed in the first 10 mins,
  #   if blue team managed to destroy more than 1 tower in the first 10 mins, there's higher chance
  #   of winning for blue team. However, with 10000 observations, only a few dozens observations show
  #   that blueteam manage to destroy >1 towers in the first 10 mins.
  #todo: put percentage of wining and losing in each bar


games %>% #aggregration on mean minion farmed, and mean gold earned by blue team.
  group_by(blueWins) %>% 
  summarise(B_avg_minion_farmed = mean(blueTotalMinionsKilled),
            B_avg_gold = mean(blueTotalGold),
            R_avg_minion_farmed = mean(redTotalMinionsKilled),
            R_avg_gold = mean(redTotalGold))


games %>% #The amount of gold distributions
  ggplot(aes(x=blueTotalGold))+
    geom_histogram(binwidth = 100)+
    geom_boxplot()+
    facet_wrap(~blueWins)+
    ggtitle("The amount of gold distributions")
    
games %>% 
  ggplot(aes(x=blueGoldDiff))+
    geom_histogram(binwidth = 100)+
    facet_wrap(~blueWins)+
    ggtitle("How gold difference affect the result")

games %>% #aggregration on mean minion farmed, and mean gold earned by blue team.
  group_by(blueWins) %>% 
  summarise(avg_Blue_goldDiff = mean(blueGoldDiff),
            n = n())
  #confidence interval of gold diff resulted in a winning for blueteam?



