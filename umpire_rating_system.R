### INITIAL SETUP ###

#install & load libraries
install.packages("tidyverse")
library(tidyverse)

  install.packages("dplyr")
  library(dplyr)
  
  install.packages("plyr")
  library(plyr)
  
  install.packages("scales")
  library(scales)

  install.packages("ggplot2")
  library(ggplot2)
  
  install.packages("factoextra")
  library(factoextra)

#setup functions
pct_func <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#load datatables
pitches <- do.call(rbind,
                   lapply(c("./Data/mlb_pitch_data_2015-2018/pitches.csv", "./Data/mlb_pitch_data_2019/pitches.csv"), read.csv))
atbats <- do.call(rbind,
                   lapply(c("./Data/mlb_pitch_data_2015-2018/atbats.csv", "./Data/mlb_pitch_data_2019/atbats.csv"), read.csv))
games <- do.call(rbind.fill,
                  lapply(c("./Data/mlb_pitch_data_2015-2018/games.csv", "./Data/mlb_pitch_data_2019/games.csv"), read.csv))

#join tables
pfx_dat <- inner_join(games, atbats, by="g_id")
pfx_dat <- inner_join(pfx_dat, pitches, by="ab_id")
pfx_dat$date <- as.Date(pfx_dat$date)

#add pitchfxzones to 2019 data
pfx_dat_2019 <- subset(pfx_dat, date > "2018-12-31")
pfx_dat_2019 <- pfx_dat_2019 %>% select(-zone)
pfx_dat_2019 <- PitchFXZones(pfx_dat_2019)
pfx_dat_2019 <- pfx_dat_2019 %>%
  select(
    -c(sz_ht, sz_ht_3, sz_ht_tz1, sz_ht_tz2, sz_wd, sz_wd_3)
    ) %>%
  relocate(
    zone, .after = nasty
  )

#combine 2019 back with full data
pfx_dat <- rbind(
  subset(pfx_dat, date < "2019-01-01"),
  pfx_dat_2019
)

#generate full list of variables based on each data table
library(tidyverse)
lst(pitches, atbats, games) %>% 
  map(names) %>%
  enframe %>% 
  unnest

variables <- stack(lapply(mget(c('pitches', 'atbats', 'games', 'ejections', 'player_names')), names))[2:1]
write.csv(variables, file.path("./variables.csv"), row.names=FALSE)

#sample table of pitches data
pitches_sample <- head(pitches, 100)
write.csv(pitches_sample, file.path("./pitches_sample.csv"), row.names=FALSE)


### SUBSET DATA ###

#join tables
fulldataset <- inner_join(pitches, atbats, by="ab_id")
fulldataset <- inner_join(fulldataset, games, by="g_id")

atbat_games <- inner_join(atbats, games, by="g_id")

#filter by year
gamedata <- fulldataset
gamedata_2018 <- filter(fulldataset, as.Date(date) > "2017-12-31") #Selecting all data from 2018 season
gamedata_2017 <- filter(fulldataset, as.Date(date) < "2018-01-01" & as.Date(date) > "2016-12-31") #Selecting all data from 2017 season
gamedata_2016 <- filter(fulldataset, as.Date(date) < "2017-01-01" & as.Date(date) > "2015-12-31") #Selecting all data from 2016 season
gamedata_2015 <- filter(fulldataset, as.Date(date) < "2016-01-01" & as.Date(date) > "2014-12-31") #Selecting all data from 2015 season

gamedata <- filter(fulldataset, as.Date(date) > "2017-12-31") #Selecting all data from 2018 season
gamedata <- filter(fulldataset, as.Date(date) < "2018-01-01" & as.Date(date) > "2016-12-31") #Selecting all data from 2017 season
gamedata <- filter(fulldataset, as.Date(date) < "2017-01-01" & as.Date(date) > "2015-12-31") #Selecting all data from 2016 season
gamedata <- filter(fulldataset, as.Date(date) < "2016-01-01" & as.Date(date) > "2014-12-31") #Selecting all data from 2015 season

gamedata["year"] <- substring(gamedata$date, 0, 4)

game_atbat_data <- atbat_games
game_atbat_data <- filter(atbat_games, as.Date(date) > "2017-12-31") #Selecting all data from 2018 season
game_atbat_data <- filter(atbat_games, as.Date(date) < "2018-01-01" & as.Date(date) > "2016-12-31") #Selecting all data from 2017 season
game_atbat_data <- filter(atbat_games, as.Date(date) < "2017-01-01" & as.Date(date) > "2015-12-31") #Selecting all data from 2016 season
game_atbat_data <- filter(atbat_games, as.Date(date) < "2016-01-01" & as.Date(date) > "2014-12-31") #Selecting all data from 2015 season

games <- games
games <- filter(games, as.Date(date) > "2017-12-31") #Selecting all data from 2018 season
games <- filter(games, as.Date(date) < "2018-01-01" & as.Date(date) > "2016-12-31") #Selecting all data from 2017 season
games <- filter(games, as.Date(date) < "2017-01-01" & as.Date(date) > "2015-12-31") #Selecting all data from 2016 season
games <- filter(games, as.Date(date) < "2016-01-01" & as.Date(date) > "2014-12-31") #Selecting all data from 2015 season


### SUMMARY PITCH STATS ###

#pitch stats full dataset
totalpitches <- count(gamedata)
totalstrikes <- count(subset(gamedata, type =='S'))
totalballs <- count(subset(gamedata, type =='B'))
totalcalledstrikes <- count(subset(gamedata, code =='C'))

ball_pct <- percent(as.numeric(totalballs/totalpitches))
strike_pct <- percent(as.numeric(totalstrikes/totalpitches))
calledstrike_pct <- percent(as.numeric(totalcalledstrikes/totalpitches))

#pitch stats by zone
pitches_by_zone <- gamedata %>%   
                      group_by(zone) %>%
                      summarise(freq=n())
pitches_by_zone <- data.frame(pitches_by_zone)

balls_by_zone <- subset(gamedata, type == "B") %>%   
  group_by(zone) %>%
  summarise(freq=n())
balls_by_zone <- data.frame(balls_by_zone)

strikes_by_zone <- subset(gamedata, type == "S") %>%   
  group_by(zone) %>%
  summarise(freq=n())
strikes_by_zone <- data.frame(strikes_by_zone)

calledstrikes_by_zone <- subset(gamedata, code == "C") %>%   
  group_by(zone) %>%
  summarise(freq=n())
calledstrikes_by_zone <- data.frame(calledstrikes_by_zone)

calledpitches_by_zone <- subset(gamedata, code == "B" | code == "C") %>%   
  group_by(zone) %>%
  summarise(freq=n())
calledpitches_by_zone <- data.frame(calledpitches_by_zone)

tot_pitches_by_zone <- cbind(pitches_by_zone, balls_by_zone$freq, strikes_by_zone$freq, calledstrikes_by_zone$freq) #this cbind assumes each pitch type (Total, Ball, Strike, Called Strike originally has a count in each zone from 1-14)
colnames(tot_pitches_by_zone) <- c("zone", "tp", "tb", "ts", "tcs") #(tp=total pitches, tb=total balls, ts=total strikes, tcs=total called strikes)
tot_pitches_by_zone["tb%"] <- pct_func(tot_pitches_by_zone$tb/tot_pitches_by_zone$tp)
tot_pitches_by_zone["ts%"] <- pct_func(tot_pitches_by_zone$ts/tot_pitches_by_zone$tp)
tot_pitches_by_zone["tcs%"] <- pct_func(tot_pitches_by_zone$tcs/tot_pitches_by_zone$tp)
tot_pitches_by_zone["total_called_pitches"] <- calledpitches_by_zone$freq
  
  incorrectcalls_by_zone <- totalincorrectcalls %>%
    group_by(zone) %>%
    summarise(freq=n())
  incorrectcalls_by_zone <- rbind(incorrectcalls_by_zone, 0)
  
tot_pitches_by_zone["total_incorrect_calls"] <- incorrectcalls_by_zone$freq

tot_pitches_by_zone["error_rate_bytp"] <- tot_pitches_by_zone$total_incorrect_calls/tot_pitches_by_zone$tp
tot_pitches_by_zone["error_rate_bycp"] <- tot_pitches_by_zone$total_incorrect_calls/tot_pitches_by_zone$total_called_pitches


### STATS ON PITCHES CALLED BY UMP ###

#called pitches (pitches umpire had a part in determining outcome of ball or strike, leaving out foul tips and swinging strikes which umpire may miscall a checked swing on)
totalcalledpitches <- subset(gamedata, code == "B" | code == "C")
totalcalledpitches_count <- count(totalcalledpitches)
calledpitchesoftotal_pct <- percent(as.numeric(totalcalledpitches_count/totalpitches))

#pitches called strikes outside zone
called_strike_outside_zone <- subset(gamedata, code == "C" & zone > 9)
called_strike_out_count <- count(called_strike_outside_zone)
called_strike_out_pct <- percent(as.numeric(called_strike_out_count/totalcalledpitches_count))

#pitches called ball inside zone
called_ball_inside_zone <- subset(gamedata, code == "B" & zone <= 9)
called_ball_inside_count <- count(called_ball_inside_zone)
called_ball_inside_pct <- percent(as.numeric(called_ball_inside_count/totalcalledpitches_count))

#total pitches called in error
totpitchescalledincorr <- called_ball_inside_count + called_strike_out_count
totpitchescalledincorr_rate <- (as.numeric(totpitchescalledincorr)/as.numeric(totalcalledpitches_count))
totpitchescalledincorr_pct <- percent(as.numeric(totpitchescalledincorr/as.numeric(totalcalledpitches_count)))

#add error variable to called pitches datatable
totalcalledpitches["correct_call"] <- ifelse(totalcalledpitches$code == "C" & totalcalledpitches$zone > 9, "NO", ifelse(totalcalledpitches$code == "B" & totalcalledpitches$zone <= 9, "NO", "YES"))
total_miscalls_count <- subset(totalcalledpitches, correct_call == "NO")  

#miscalls by HP umpire
totalcalledpitches["correct_call_code"] <- ifelse(totalcalledpitches$code == "C" & totalcalledpitches$zone > 9, 1, ifelse(totalcalledpitches$code == "B" & totalcalledpitches$zone <= 9, 1, 0))
totalincorrectcalls <- subset(totalcalledpitches, correct_call_code == 1)

total_calls_byump <- totalcalledpitches %>%
                        group_by(umpire_HP) %>%
                        summarize(n())

colnames(total_calls_byump)[colnames(total_calls_byump)=="n()"] <- "total_calls"

incorrect_calls_byump <- totalincorrectcalls %>% 
                            group_by(umpire_HP) %>%
                            summarize(n())

incorrectcalls_by_zone <- totalincorrectcalls %>%
  group_by(zone) %>%
  summarise(freq=n())
incorrectcalls_by_zone <- rbind(incorrectcalls_by_zone, 0)

colnames(incorrect_calls_byump)[colnames(incorrect_calls_byump)=="n()"] <- "incorrect_calls"

calls_byump <- inner_join(total_calls_byump, incorrect_calls_byump, by="umpire_HP")
calls_byump["err_rate"] <- (calls_byump$incorrect_calls/calls_byump$total_calls)
calls_byump["err_rate_vs_avg"] <- totpitchescalledincorr_rate
calls_byump["err_rate_vs_avg"] <- percent(calls_byump$err_rate - calls_byump$err_rate_vs_avg)
calls_byump["err_rate"] <- percent(calls_byump$incorrect_calls/calls_byump$total_calls)

#determining ump outliers from dataset
sdoferr <- sd(calls_byump$incorrect_calls/calls_byump$total_calls)

calls_byump["sd"] <- percent(sd(calls_byump$incorrect_calls/calls_byump$total_calls))
calls_byump["two_sd"] <- percent(sd(calls_byump$incorrect_calls/calls_byump$total_calls)+sd(calls_byump$incorrect_calls/calls_byump$total_calls))
calls_byump["err_rate_vs_avg_abs"] <- percent(abs((calls_byump$incorrect_calls/calls_byump$total_calls) - totpitchescalledincorr_rate))
calls_byump["is_two_sd"] <- ifelse(calls_byump$err_rate_vs_avg_abs > (calls_byump$two_sd), "YES", "NO")

calls_byump["err_rate_vs_sd"] <- ((calls_byump$incorrect_calls/calls_byump$total_calls) - totpitchescalledincorr_rate) - sdoferr

#called pitches by zone
calledpitches_by_zone <- subset(gamedata, code == "B" | code == "C") %>%   
  group_by(zone) %>%
  summarise(freq=n())
calledpitches_by_zone <- data.frame(calledpitches_by_zone)

calledballs_by_zone <- subset(gamedata, code == "B") %>%   
  group_by(zone) %>%
  summarise(freq=n())
calledballs_by_zone <- data.frame(calledballs_by_zone)

calledstrikes_by_zone <- subset(gamedata, code == "C") %>%   
  group_by(zone) %>%
  summarise(freq=n())
calledstrikes_by_zone <- data.frame(calledstrikes_by_zone)

tot_calledpitches_by_zone <- cbind(calledpitches_by_zone, calledballs_by_zone$freq, calledstrikes_by_zone$freq) #this cbind assumes each pitch type (Total, Ball, Strike, Called Strike originally has a count in each zone from 1-14)
colnames(tot_calledpitches_by_zone) <- c("zone", "tcp", "tcb", "tcs") #(tp=total called pitches, tb=total called balls, tcs=total called strikes)
tot_calledpitches_by_zone["tcb%"] <- pct_func(tot_calledpitches_by_zone$tcb/tot_calledpitches_by_zone$tcp)
tot_calledpitches_by_zone["tcs%"] <- pct_func(tot_calledpitches_by_zone$tcs/tot_calledpitches_by_zone$tcp)

final_byzone <- cbind(tot_pitches_by_zone, tot_calledpitches_by_zone)

#total pitches called by ump by zone
totalcalls_byump_byzone <- totalcalledpitches %>% 
  group_by(umpire_HP, zone) %>%
  summarize(n())
colnames(totalcalls_byump_byzone) <- c("umpire_HP", "zone", "n_calls")

#pitches called strikes outside zone by zone
calledstrikes_outsidezone_byzone <- subset(gamedata, code == "C" & zone > 9) %>%   
  group_by(zone) %>%
  summarise(freq=n())
calledstrikes_outsidezone_byzone <- data.frame(calledstrikes_outsidezone_byzone)

calledballs_insidezone_byzone <- subset(gamedata, code == "B" & zone <= 9) %>%   
  group_by(zone) %>%
  summarise(freq=n())
calledballs_insidezone_byzone <- data.frame(calledballs_insidezone_byzone)

tot_errorcalledpitches <- rbind(calledballs_insidezone_byzone, calledstrikes_outsidezone_byzone)

#pitches called strikes outside zone by ump by zone
incorrect_calls_byump_byzone <- totalincorrectcalls %>% 
  group_by(umpire_HP, zone) %>%
  summarize(n())
colnames(incorrect_calls_byump_byzone) <- c("umpire_HP", "zone", "calls_in_error")

called_balls <- subset(totalcalledpitches, code == "B")
totalballs_byump_byzone <- called_balls %>%
  group_by(umpire_HP, zone) %>%
  summarize(n())

called_strikes <- subset(totalcalledpitches, code == "C")
totalcalledstrikes_byump_byzone <- called_strikes %>%
  group_by(umpire_HP, zone) %>%
  summarize(n())

final_calledpitches_byumpbyzone <- merge(totalcalls_byump_byzone, totalballs_byump_byzone, by=c("umpire_HP", "zone"), sort = TRUE)
final_calledpitches_byumpbyzone <- merge(final_calledpitches_byumpbyzone, totalcalledstrikes_byump_byzone, by=c("umpire_HP", "zone"), sort = TRUE)
final_calledpitches_byumpbyzone <- merge(final_calledpitches_byumpbyzone, incorrect_calls_byump_byzone, by=c("umpire_HP", "zone"), sort = TRUE)
final_calledpitches_byumpbyzone["error_rate"] <- final_calledpitches_byumpbyzone$calls_in_error/final_calledpitches_byumpbyzone$n_calls
colnames(final_calledpitches_byumpbyzone) <- c("umpire_HP", "zone", "n_calls", "n_balls", "n_strikes", "calls_in_error", "error_rate")


### GAME STATS AND DATA ###

#game stats
ab_by_game <- game_atbat_data %>%
  group_by(g_id) %>%
  summarize(n())

bb_by_game <- subset(game_atbat_data, event=="Walk") %>%
  group_by(g_id) %>%
  summarize(n())

k_by_game <- subset(game_atbat_data, event=="Strikeout") %>%
  group_by(g_id) %>%
  summarize(n())

hr_by_game <- subset(game_atbat_data, event=="Home Run") %>%
  group_by(g_id) %>%
  summarize(n())

awayscore_by_game <- aggregate(away_final_score ~ g_id, data = games, sum)
homescore_by_game <- aggregate(home_final_score ~ g_id, data = games, sum)
runs_by_game <- inner_join(awayscore_by_game, homescore_by_game, by="g_id")
runs_by_game["total"] <- runs_by_game$away_final_score + runs_by_game$home_final_score

game_stats <- inner_join(inner_join(inner_join(inner_join(
  ab_by_game, bb_by_game, by="g_id"),
  k_by_game, by="g_id"),
  hr_by_game, by="g_id"),
  runs_by_game, by="g_id")
game_stats <- subset(game_stats, select= -c(away_final_score, home_final_score))
colnames(game_stats) <- c("g_id", "at_bats",  "walks", "strikeouts", "homeruns", "tot_runs")

#game stats by ump
games_by_ump <- games %>%
  group_by(umpire_HP) %>%
  summarize(n())

pitches_by_ump <- subset(gamedata, code == "B" | code == "C") %>% 
  group_by(umpire_HP) %>%
  summarize(n())

pitches_inside <- subset(gamedata, code == "B" & zone <= 9 | code == "C" & zone <= 9) %>%
  group_by(umpire_HP) %>%
  summarise(n())

pitches_outside <- subset(gamedata, code == "B" & zone > 10 | code == "C" & zone > 10) %>%
  group_by(umpire_HP) %>%
  summarise(n())
                         
ab_by_ump <- game_atbat_data %>%
  group_by(umpire_HP) %>%
  summarize(n())

bb_by_ump <- subset(game_atbat_data, event=="Walk") %>%
  group_by(umpire_HP) %>%
  summarize(n())

k_by_ump <- subset(game_atbat_data, event=="Strikeout") %>%
  group_by(umpire_HP) %>%
  summarize(n())

hr_by_ump <- subset(game_atbat_data, event=="Home Run") %>%
  group_by(umpire_HP) %>%
  summarize(n())

awayscore_by_ump <- aggregate(away_final_score ~ umpire_HP, data = games, sum)
homescore_by_ump <- aggregate(home_final_score ~ umpire_HP, data = games, sum)
runs_by_ump <- inner_join(awayscore_by_ump, homescore_by_ump, by="umpire_HP")
runs_by_ump["total"] <- runs_by_ump$away_final_score + runs_by_ump$home_final_score

ump_game_stats <- inner_join(inner_join(inner_join(inner_join(inner_join(inner_join(inner_join(inner_join(
  games_by_ump, pitches_by_ump, by="umpire_HP"),
  pitches_inside, by="umpire_HP"),
  pitches_outside, by="umpire_HP"),
  ab_by_ump, by="umpire_HP"),
  bb_by_ump, by="umpire_HP"),
  k_by_ump, by="umpire_HP"),
  hr_by_ump, by="umpire_HP"),
  runs_by_ump, by="umpire_HP")
ump_game_stats <- subset(ump_game_stats, select= -c(away_final_score, home_final_score))
colnames(ump_game_stats) <- c("umpire_HP", "games_called", "pitches_called", "pitches_inside", "pitches_outside", "at_bats",  "walks", "strikeouts", "homeruns", "tot_runs")

ump_game_stats["bb_per_ab"] <- ump_game_stats$walks/ump_game_stats$at_bats
ump_game_stats["k_per_ab"] <- ump_game_stats$strikeouts/ump_game_stats$at_bats
ump_game_stats["hr_per_ab"] <- ump_game_stats$homeruns/ump_game_stats$at_bats
ump_game_stats["tot_runs_per_game"] <- ump_game_stats$tot_runs/ump_game_stats$games_called


### UMPIRE SEGMENTATION ###

#bb/ab vs k/ab analysis
summary(ump_game_stats)
ump_game_stats_plot <- data.frame(umpire_HP=ump_game_stats$umpire_HP[-c(7, 61)])
ump_game_stats_plot <- merge(ump_game_stats, ump_game_stats_plot, by=c("umpire_HP"), sort = TRUE)
summary(ump_game_stats_plot)

ggplot(ump_game_stats, aes(walks, strikeouts, color = umpire_HP)) + geom_point()
ggplot(ump_game_stats_plot, aes(bb_per_ab, k_per_ab)) + geom_point() + geom_text(aes(label=umpire_HP),hjust=0, vjust=0) + geom_smooth(method = "lm")
ggplot(ump_game_stats_plot, aes(bb_per_ab, k_per_ab)) + geom_point() + geom_smooth(method = "lm")

cor.test(ump_game_stats_plot$bb_per_ab, ump_game_stats_plot$k_per_ab, method = "pearson", conf.level = 0.95)
#ump_game_stats_plot <- ump_game_stats

#outsidezone errors vs insidezone errors analysis
incorrect_calls_byump_inzone <- subset(incorrect_calls_byump_byzone, zone < 10)
incorrect_calls_byump_inzone <- incorrect_calls_byump_inzone[-c(2)]
incorrect_calls_byump_inzone <- aggregate(incorrect_calls_byump_inzone$calls_in_error, by=list(umpire_HP=incorrect_calls_byump_inzone$umpire_HP), FUN=sum)

incorrect_calls_byump_outzone <- subset(incorrect_calls_byump_byzone, zone > 10)
incorrect_calls_byump_outzone <- incorrect_calls_byump_outzone[-c(2)]
incorrect_calls_byump_outzone <- aggregate(incorrect_calls_byump_outzone$calls_in_error, by=list(umpire_HP=incorrect_calls_byump_outzone$umpire_HP), FUN=sum)

incorrect_calls_byump_inorout <- merge(incorrect_calls_byump_inzone, incorrect_calls_byump_outzone, by=c("umpire_HP"))
colnames(incorrect_calls_byump_inorout) <- c("umpire_HP", "incorrect_inside", "incorrect_outside")
incorrect_calls_byump_inorout <- data.frame(umpire_HP=incorrect_calls_byump_inorout$umpire_HP[-c(7, 19)])
incorrect_calls_byump_inorout <- merge(incorrect_calls_byump_inorout, incorrect_calls_byump_inzone, by=c("umpire_HP"))
incorrect_calls_byump_inorout <- merge(incorrect_calls_byump_inorout, incorrect_calls_byump_outzone, by=c("umpire_HP"))
colnames(incorrect_calls_byump_inorout) <- c("umpire_HP", "incorrect_inside", "incorrect_outside")

ggplot(incorrect_calls_byump_inorout, aes(incorrect_inside, incorrect_outside)) + geom_point() + geom_smooth(method = "lm")
cor.test(incorrect_calls_byump_inorout$incorrect_inside, incorrect_calls_byump_inorout$incorrect_outside, method = "pearson", conf.level = 0.95)
summary(incorrect_calls_byump_inorout)

#clustering
kmeans_umpdata <- merge(ump_game_stats_plot, incorrect_calls_byump_inorout, by="umpire_HP")
kmeans_umpdata["inside_error_rate"] <- kmeans_umpdata$incorrect_inside/kmeans_umpdata$pitches_inside
kmeans_umpdata["outside_error_rate"] <- kmeans_umpdata$incorrect_outside/kmeans_umpdata$pitches_outside
  
kmeans_umpdata2 <- data.frame(umpire_HP=kmeans_umpdata$umpire_HP[-c(57)])
kmeans_umpdata2 <- merge(kmeans_umpdata2, kmeans_umpdata, by=c("umpire_HP"), sort = TRUE)
kmeans_umpdata2 <- kmeans_umpdata[-c(2:10, 15:16)]

kmeans_umpdata3 <- kmeans_umpdata2[,-1]
rownames(kmeans_umpdata3) <- kmeans_umpdata2[,1]

kmeans_umpdata4 <- na.omit(kmeans_umpdata3)
kmeans_umpdata4 <- data.frame(scale(kmeans_umpdata3))

fviz_nbclust(kmeans_umpdata4, kmeans, method = "wss")
fviz_nbclust(kmeans_umpdata4, kmeans, method = "silhouette")

k1 <- kmeans(kmeans_umpdata4, centers = 2, nstart = 25)
fviz_cluster(k1, data = kmeans_umpdata4)
k1

cluster_list <- data.frame(k1$cluster)
kmeans_umpdata <- kmeans_umpdata[,-1]
rownames(kmeans_umpdata) <- kmeans_umpdata2[,1]
kmeans_umpdata5 <- merge(kmeans_umpdata, cluster_list, by='row.names')

  #clustering v2
  kmeans_umpdata <- merge(ump_game_stats_plot, incorrect_calls_byump_inorout, by="umpire_HP")
  kmeans_umpdata["inside_error_rate"] <- kmeans_umpdata$incorrect_inside/kmeans_umpdata$pitches_inside
  kmeans_umpdata["outside_error_rate"] <- kmeans_umpdata$incorrect_outside/kmeans_umpdata$pitches_outside
  
  kmeans_umpdata2 <- data.frame(umpire_HP=kmeans_umpdata$umpire_HP[-c(57)])
  kmeans_umpdata2 <- merge(kmeans_umpdata2, kmeans_umpdata, by=c("umpire_HP"), sort = TRUE)
  kmeans_umpdata2 <- kmeans_umpdata[-c(2:16)]
  
  kmeans_umpdata3 <- kmeans_umpdata2[,-1]
  rownames(kmeans_umpdata3) <- kmeans_umpdata2[,1]
  
  kmeans_umpdata4 <- na.omit(kmeans_umpdata3)
  kmeans_umpdata4 <- data.frame(scale(kmeans_umpdata3))
  
  fviz_nbclust(kmeans_umpdata4, kmeans, method = "wss")
  fviz_nbclust(kmeans_umpdata4, kmeans, method = "silhouette")
  
  k1 <- kmeans(kmeans_umpdata4, centers = 10, nstart = 25)
  fviz_cluster(k1, data = kmeans_umpdata4)
  k1
  
  cluster_list <- data.frame(k1$cluster)
  kmeans_umpdata <- kmeans_umpdata[,-1]
  rownames(kmeans_umpdata) <- kmeans_umpdata2[,1]
  kmeans_umpdata5 <- merge(kmeans_umpdata, cluster_list, by='row.names')


### EXPORT DATA TABLES ###
write.csv(tot_pitches_by_zone, file.path("./totalpitches_byzone.csv"),row.names=FALSE)
write.csv(final_calledpitches_byumpbyzone, file.path("./calledpitches_byump_byzone.csv"),row.names=FALSE)
write.csv(game_stats, file.path("./gamestats.csv"), row.names = FALSE)
write.csv(ump_game_stats, file.path("./gamestats_byump.csv"), row.names = FALSE)
write.csv(kmeans_umpdata5, file.path('./clustering_output.csv'))

write.csv(game_atbat_data, file.path("./game_atbat_data.csv"), row.names=FALSE)

#-------------
filter(totalcalledpitches, correct_call_code==1)
count(filter(totalcalledpitches, correct_call_code==1))

total_games_byump <- gamedata %>%
  group_by(umpire_HP, g_id) %>%
  summarize(n())

