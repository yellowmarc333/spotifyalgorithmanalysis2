options(scipen = 999)
library(data.table)
library(checkmate)
library(wdl)
library(ggplot2)
library(labels)
library(xlsx)
library(clipr)
library(xgboost)

# width
width <- 1000
height <- 400

source("02_code/02_clean/clean.R")

# how many streams do we need to maintain the popularity index with
# average algorithmic streams/listeners

# todo: releasedate checken

# dt_raw <- fread("01_data/2025_04_27_Spotify Algorithm Analysis (Responses) - Form Responses 3.csv")
# dt_raw <- fread("01_data/2025_04_17_Spotify Algorithm Analysis (Responses) - Form Responses 2.csv")
# dt_raw <- fread("01_data/2025_05_25_Spotify Algorithm Analysis (Responses) - Form Responses 4.csv")

dt_raw <- data.table(read.xlsx("01_data/2025_05_29_Spotify Algorithm Analysis add data manual cleaned.xlsx",
                    sheetIndex = 1))

sapply(dt_raw, class)
names(dt_raw)

dt_raw[, `Column.27` := NULL]
dt <- clean(dt_raw)[!is.na(PopularityIndex)]


dt_own_data <- dt[ArtistName %in% c("Marc Schmieder", "Kid Korea"), 
                  .(Timestamp,SongName, PopularityIndex, StreamsLast28Days, ListenersLast28Days,
                    RadioStreamsLast28Days, RadioStreamsLast7Days,
                    DaysSinceRelease)][order(Timestamp)]


length(unique(dt$ArtistName))
unique(dt[ArtistName != "Anon", .(ArtistName, SongName)])
table(dt$AreYouIndependentOrSignedToALabel)
# todo, make analysis on discovery mode
table(dt$IsThisSongOptedIntoSpotifyDiscoveryMode)


prop.table(table(dt$WhatIsTheDominantWayThisArtistHasPromotedThisSong))


# feature generation ####
table(dt$ReleaseConsistency)
dt[ReleaseConsistency == "1-2 weeks", ReleaseConsistencyNum := 1.5]
dt[ReleaseConsistency == "3-4 weeks", ReleaseConsistencyNum := 3.5]
dt[ReleaseConsistency == "5-6 weeks", ReleaseConsistencyNum := 5.5]
dt[ReleaseConsistency == "7-8 weeks", ReleaseConsistencyNum := 7.5]
dt[ReleaseConsistency == "9-10 weeks", ReleaseConsistencyNum := 9.5]
dt[ReleaseConsistency == "11-12 weeks", ReleaseConsistencyNum := 11.5]
dt[ReleaseConsistency == "Over 12 weeks", ReleaseConsistencyNum := 14]
table(dt$ReleaseConsistencyNum)


dt[, DaysSinceRelease := floor(Timestamp - ReleaseDate)]
dt[, WeeksSinceRelease := as.integer(floor(DaysSinceRelease/7 + 1))]
dt[, NewRelease := ifelse(DaysSinceRelease < 30, "yes", "no")]
# todo: remove all releasedates where DaysSinceRelease is negative
# dt[DaysSinceRelease <0]

## relative radio streams over time ####
dt[, RelativeRadioStreams7Days := RadioStreamsLast7Days/
     mean(RadioStreamsLast7Days, na.rm = TRUE), by = .(PopularityIndex)]
dt[, NObsPerWeek := .N, by = .(WeeksSinceRelease)]
dt[, MeanRelativeRadioStreams7Days := 
     mean(RelativeRadioStreams7Days, na.rm = TRUE), by = .(WeeksSinceRelease)]

# mean radio by PI:
# Define the breaks and labels
breaks <- c(0, 5, 10, 13, 16, 19, 21, 25, 29, 32, 35, 40, 45, 50, 100)
labels <- c("0-5", "6-10", "11-13", "14-16", "17-19", "20-21", 
            "22-25", "26-29", "30-32", "33-35", "36-40", "41-45", "46-50", "51-100")

# Categorize the variable
dt[, PI_Category := cut(PopularityIndex, breaks = breaks, labels = labels, right = TRUE, include.lowest = TRUE)]

table(dt$PI_Category)

breaks_2 <- c(0, 5, 10, 15, 20, 25, 30, 40, 100)
labels_2 <- c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30", 
            "31-40", "41-100")

# Categorize the variable
dt[, PI_Category_2 := cut(PopularityIndex, breaks = breaks_2, labels = labels_2, 
                          right = TRUE, include.lowest = TRUE)]
table(dt$PI_Category_2)


# mean radio by PI:
# Define the breaks and labels
breaks_3 <- c(0, 5, 10, 25, 50, 100, 1000)
labels_3 <- c("0-5", "5-10", "10-25", "25-50", "50-100",
              "100-1000")
dt[, WeeksSinceReleaseCategory := cut(WeeksSinceRelease, 
                                      breaks = breaks_3, labels = labels_3, 
                          right = TRUE, include.lowest = TRUE)]
table(dt$WeeksSinceReleaseCategory)


dt[, ListenersStreamRatio28Days := StreamsLast28Days/ListenersLast28Days]
hist(dt$ListenersStreamRatio28Days, xlim = c(1, 7), breaks = 500)


dt[, ListenersStreamRatio7Days := StreamsLast7Days/ListenersLast7Days]
# calculating the algorithmic streams in the last 28 and 7 days
dt[, AlgoStreams28Days := RadioStreamsLast28Days + 
     ReleaseRadarStreamsLast28Days +
     DiscoverWeeklyStreamsLast28Days]
dt[, AlgoStreams7Days := RadioStreamsLast7Days + 
     ReleaseRadarStreamsLast7Days +
     DiscoverWeeklyStreamsLast7Days]
# calculate the NonAlgorStreams for the last 28 or 7 days.
dt[, NonAlgoStreams28Days := StreamsLast28Days - AlgoStreams28Days]
dt[, NonAlgoStreams7Days := StreamsLast7Days - AlgoStreams7Days]

# calculate adjuste Streams/Listener ratio
algo_stream_listener_ratio <- 1.3
# todo: here are many negative entries, find out what happened here
dt[, NonAlgoEngagement28Days := pmax(1, NonAlgoStreams28Days /
                                          (ListenersLast28Days -
                                             AlgoStreams28Days/algo_stream_listener_ratio))]
hist(dt$NonAlgoEngagement28Days, xlim = c(1, 7), breaks = 500)
                                        
dt[, NonAlgoEngagement7Days := pmax(1, NonAlgoStreams7Days /
     (ListenersLast7Days -
        AlgoStreams7Days/algo_stream_listener_ratio))
]


# calculate all streams from algorithmic, calculate all without, calculate save rat
# total and save rate for non-algorithmic streams, then calculate missing
# todo: kann man nicht wirklich rausrechnen, lieber total save rate
all_algo_streams <- dt[, sum(AlgoStreams28Days, na.rm = TRUE)]
all_nonalgo_streams <- dt[, sum(NonAlgoStreams28Days, na.rm = TRUE)]
# streams summed for every song that has under 100 algorithmic streams
limit <- 1000
streams_only_non_algo <- dt[AlgoStreams28Days < limit , sum(StreamsLast28Days, na.rm = TRUE)]
streams_with_algo <- dt[AlgoStreams28Days >= limit , sum(StreamsLast28Days, na.rm = TRUE)]

save_rate_total <- dt[ , mean(SavesLast28Days, na.rm = TRUE)/ 
                  mean(StreamsLast28Days, na.rm = TRUE)]

save_rate_nonalgo <- dt[AlgoStreams28Days < limit , mean(SavesLast28Days, na.rm = TRUE)/ 
     mean(StreamsLast28Days, na.rm = TRUE)]

# save_rate <- dt[RadioStreamsLast28Days < 100, mean(SavesLast28Days, na.rm = TRUE)/ mean(StreamsLast28Days,
#                                                             na.rm = TRUE)]

playlist_rate_total <- dt[, mean(PlaylistAddsLast28Days, na.rm = TRUE)/ mean(StreamsLast28Days,
                                                      na.rm = TRUE)]
# playlist_rate <- dt[RadioStreamsLast28Days < 100, mean(PlaylistAddsLast28Days, na.rm = TRUE)/ mean(StreamsLast28Days,
#                                                                        na.rm = TRUE)]

dt[, SavesLast28Days_adj := round(SavesLast28Days - 
     AlgoStreams28Days*save_rate_total, 2)]
dt[, PlaylistAddsLast28Days_adj := round(PlaylistAddsLast28Days - 
     AlgoStreams28Days*playlist_rate_total, 2)]

# adding first 5 weeks after release
dt[, ReleasePhaseEarly := ifelse(DaysSinceRelease < 35, TRUE, FALSE)]
table(dt$ReleasePhaseEarly)

# writing the data table ####
fwrite(dt, "01_data/data_prepared_final_1.csv")

# Checking on data quality: for negative values ####
dt[ StreamsLast7Days < 0, .N]
dt[ ListenersLast7Days < 0, .N]
dt[ SavesLast7Days < 0, .N]
dt[ PlaylistAddsLast7Days < 0, .N]
dt[ RadioStreamsLast7Days < 0, .N]
dt[ DiscoverWeeklyStreamsLast7Days < 0, .N]
dt[ ReleaseRadarStreamsLast7Days < 0, .N]
dt[ CurrentSpotifyFollowers < 0, .N]

# dt[ReleaseDate < "2018-02-21 UTC"]
# res <- dt[PopularityIndex < 0]


# check if same song has different release dates
res <- dt[, .(N_unique = length(unique(ReleaseDate))), 
          by = .(ArtistName, SongName)][N_unique >= 2]

# define sub dt's via ReleasePhaseEarly ####
dt_early <- dt[!is.na(PopularityIndex) & ReleasePhaseEarly == TRUE]
dt_late <- dt[!is.na(PopularityIndex) & ReleasePhaseEarly == FALSE]

# Exploration ####
# todo:
# do discover weekly for PI over x
# todo: analyse radio short term vs radio after 28 days of release
# make boxplots for weeks of radio

## RR for Releasephase early ####
res <- dt_early[, .(N = .N,
                    RR28Mean = round(
                      mean(ReleaseRadarStreamsLast28Days, na.rm = TRUE), 1),
                    RR28Min = min(ReleaseRadarStreamsLast28Days, na.rm = TRUE),
                    RR28Max = max(ReleaseRadarStreamsLast28Days, na.rm = TRUE)),
                by = .(PI_Category_2)][order(PI_Category_2)]
res
write.xlsx(res, "03_docs/result_tables/res_RR.xlsx")

## Old data Release Radar ####
dt_old <- fread("01_data/data from old analysis/dt_prepared_unfiltered.csv")
dt_old[, ReleasePhaseEarly := ifelse(DaysSinceRelease < 35, TRUE, FALSE)]

dt_old[, PI_Category_2 := cut(PopularityIndex, breaks = breaks_2, labels = labels_2, 
                          right = TRUE, include.lowest = TRUE)]
dt_old_early <- dt_old[ReleasePhaseEarly == TRUE] 

res <- dt_old_early[, .(N = .N,
                        RR28Mean = round(
                          mean(ReleaseRadarStreamsLast28Days, na.rm = TRUE), 1)),
                    #RR28Min = min(ReleaseRadarStreamsLast28Days, na.rm = TRUE),
                    #RR28Max = max(ReleaseRadarStreamsLast28Days, na.rm = TRUE)),
                    by = .(PI_Category_2)][order(PI_Category_2)]
res
write.xlsx(res, "03_docs/result_tables/res_RR_old.xlsx")



## DW for Releasephase Late ####
res <- dt_late[, .(N = .N, 
                   DW28Mean = round(
                     mean(DiscoverWeeklyStreamsLast28Days, na.rm = TRUE), 1),
                   DW28Min = round(min(DiscoverWeeklyStreamsLast28Days,
                                       na.rm = TRUE)),
                   DW28Max = round(max(DiscoverWeeklyStreamsLast28Days, 
                                       na.rm = TRUE))),
                   by = .(PI_Category_2)][order(PI_Category_2)]
res
write.xlsx(res, "03_docs/result_tables/res_DW.xlsx")

## old data DW ####
# not enough samplesize to actually show it
dt_old_late <- dt_old[ReleasePhaseEarly == FALSE]

res <- dt_old_late[!is.na(DiscoverWeeklyStreamsLast28Days), 
               .(N = .N, 
                 DW28Mean = round(
                   mean(DiscoverWeeklyStreamsLast28Days, na.rm = TRUE), 1),
                 DW28Min = round(min(DiscoverWeeklyStreamsLast28Days,
                                     na.rm = TRUE)),
                 DW28Max = round(max(DiscoverWeeklyStreamsLast28Days, 
                                     na.rm = TRUE))),
               by = .(PI_Category_2)][order(PI_Category_2)]
# res
# write.xlsx(res, "03_docs/result_tables/res_DW_old.xlsx")




## Radio 28 total ####

res <- dt[, .(# Streams28Mean = round(mean(StreamsLast28Days, na.rm = TRUE)),
  # NonAlgoStreams28Mean = round(mean(NonAlgoStreams28Days, na.rm = TRUE)),
  # Listeners28Mean = round(mean(ListenersLast28Days, na.rm = TRUE)),
  N = .N, 
  Radio28Mean = round(mean(RadioStreamsLast28Days, na.rm = TRUE), 1),
  Radio28Min = round(min(RadioStreamsLast28Days, na.rm = TRUE), 1),
  Radio28Max = round(max(RadioStreamsLast28Days, na.rm = TRUE), 1),
  Radio7Mean = round(mean(RadioStreamsLast7Days, na.rm = TRUE), 1),
  Radio7Min = round(min(RadioStreamsLast7Days, na.rm = TRUE), 1),
  Radio7Max = round(max(RadioStreamsLast7Days, na.rm = TRUE), 1)),
  by = .(PI_Category_2)][order(PI_Category_2)]

res
write.xlsx(res, "03_docs/result_tables/res_Radio28and7.xlsx")

## Radio 7 by Releasephase ####
# early phase
# by early and late
res <- dt[, .(N = .N,
              Radio7Mean = round(mean(RadioStreamsLast7Days, na.rm = TRUE), 1),
              Radio7Min = round(min(RadioStreamsLast7Days, na.rm = TRUE), 1),
              Radio7Max = round(max(RadioStreamsLast7Days, na.rm = TRUE), 1)),
          by = .(PI_Category_2, ReleasePhaseEarly)][order(PI_Category_2,
                                                          -ReleasePhaseEarly)]
res
write.xlsx(res, "03_docs/result_tables/res_Radio7_releasephase.xlsx")

## Algorithmic/nonalgorithmic streams by  Popularity Index Category####
res <- dt[, .(N = .N,
              AlgoStreams28DaysMean = 
                round(mean(AlgoStreams28Days, na.rm = TRUE)),
              NonAlgoStreams28DaysMean = 
                round(mean(NonAlgoStreams28Days, na.rm = TRUE)),
              AlgoPc = round(mean(AlgoStreams28Days/StreamsLast28Days,
                                  na.rm = TRUE), 2),
              NonAlgoPc = round(mean(NonAlgoStreams28Days/StreamsLast28Days, 
                                     na.rm = TRUE), 2)),
          by = .(PI_Category_2)][order(PI_Category_2)]
res
write.xlsx(res, "03_docs/result_tables/Algo_NonAlgo.xlsx")


View(dt[is.na(AlgoStreams28Days)])


## Streams/Listener by  Popularity Index Category####
# Algostreams by 
res <- dt[, .(N = .N,
              Streams28Mean = round(mean(StreamsLast28Days, na.rm = TRUE), 1), 
              # NonAlgoStreams28Mean = round(mean(NonAlgoStreams28Days, na.rm = TRUE)), 
              Listeners28Mean = round(mean(ListenersLast28Days, na.rm = TRUE), 1)),
          by = .(PI_Category)][order(PI_Category)]
res
write.xlsx(res, "03_docs/result_tables/PI_streams_Listeners.xlsx")

# res[, RatioNonAlgoStreams7 := round(NonAlgoStreams7Mean/Streams7Mean, 2)]


## radio vs release radar vs discover weekly ####
res <- dt[, .(N_obs = .N,
              Radio28Mean = round(mean(RadioStreamsLast28Days, na.rm = TRUE), 1),
              DW28Mean = round(
                mean(DiscoverWeeklyStreamsLast28Days, na.rm = TRUE), 1),
              RR28Mean = round(
                mean(ReleaseRadarStreamsLast28Days, na.rm = TRUE), 1)),
          by = .(PI_Category_2)][order(PI_Category_2)]
res
write.xlsx(res, "03_docs/result_tables/algostreams_comparison.xlsx")



# Modeling PI ####
# listeners Last 28 Days
gg <- ggplot(dt, aes(ListenersLast28Days,
                     PopularityIndex)) +
  geom_point(colour = "darkblue", shape = 1) +
  scale_x_continuous() +
  labs(x = "Listeners last 28 days",
       y = "Popularity Index") +
  theme()
gg
ggsave(filename = "PIvsListeners28.png", path = "03_docs/result_graphics/",
       plot = gg)
# streams last 28 days
gg <- ggplot(dt, aes(ListenersLast7Days,
                     PopularityIndex)) +
  geom_point(colour = "darkblue", shape = 1) +
  scale_x_continuous() +
  labs(x = "Listeners last 7 days",
       y = "Popularity Index") +
  theme()
gg


dt_mod <- dt[, .(y = PopularityIndex,
                 x_1= ListenersLast28Days,
                 x_2 = StreamsLast28Days,
                 x_3 = SavesLast28Days,
                 x_4 = PlaylistAddsLast28Days,
                 x_5 = ListenersLast7Days)]


model <- lm(log(y+1) ~ log(x_1 + 1) + log(x_2 + 1) + log(x_3 + 1) +
              log(x_4 + 1), data = dt_mod)
summary(model)

model <- lm(log(y+1) ~ log(x_1 + 1) + log(x_2 + 1), data = dt_mod)
summary(model)


# simplest model
model <- lm(log(y+1) ~ log(x_1 + 1) , data = dt_mod)
summary(model)

model <- lm(log(y+1) ~ log(x_1 + 1) + x_1, data = dt_mod)
summary(model)
# 0.8645 with cleaned data


# Best modell (final)
# without log on left side
model <- lm(y ~ log(x_1 + 1), 
            data = dt_mod)
summary(model)

# for log on left side ()
pred_dt <- data.table(y_fitted = exp(model$fitted.values) - 1,
                      y = exp(model$model$`log(y + 1)`) - 1,
                      x_1 = model$model$x_1)

pred_dt <- data.table(y_fitted = model$fitted.values,
                      y = model$model$y,
                      x_1 = exp(model$model$`log(x_1 + 1)`) - 1)
# manually correct negative values
pred_dt[y_fitted < 0, y_fitted := 0]

pred_dt[, obs := 1:.N]
mean(abs(pred_dt$y_fitted - pred_dt$y))
# 3.530185
# without log on left side:
# 2.48
# with manually setting to 0
# 2.28

# only listeners: 4.2
# listeners in exponential and linear: 3.55
# with listeners last 7 days: 3.5
# with interaction between 7 and 28 days: 3.12
# with interaction between listeners and streams


# Extract coefficient and calculate c
# beta <- coef(model)[["log(x_1 + 1)"]]
# c_estimate <- 1 / beta
# 
# cat("Estimated c:", c_estimate, "\n")

## Plot actual vs fitted values ####
pred_dt_long <- melt(pred_dt, id.vars = c("obs", "x_1"), value.name = "value",
                     variable.name = "type")

gg <- ggplot(pred_dt_long, aes(x = reorder(obs, value), y = value, group = obs,
                         shape = type)) +
  geom_line(color = "gray", alpha = 0.5, type = "dotted") +  # line from y to y_fitted
  geom_point(aes(color = type), size = 1.7) +
  #scale_x_continuous(limits = c(0, 50)) +
  # scale_y_continuous(limits = c(0, 5000)) +  # optional: fix x order
  # coord_cartesian(ylim = c(0, 20000)) +  # set ylim (like y-axis zoom)
  labs(title = "Fitted and true values of Popularity Index", 
       x = "data points (ordered by Popularity Index)", 
       y = "Popularity Index") +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
gg

ggsave(filename = "PI fitted vs actual2.png", path = "03_docs/result_graphics/",
       plot = gg, width = 10, 
       height =  6)

# Modelling radio 7 ####
## Looking at own data ####
dt_sub <- dt[SongName %in% c("The Joy of Melancholy",
                             "Dey Say",
                             "Without",
                             "Moving Clouds")]

gg <- ggplot(dt_sub, aes(x = DaysSinceRelease,
                         y = RadioStreamsLast7Days,
                         size = NonAlgoEngagement28Days,
                         colour = factor(SongName),
                         shape = factor(SongName))) +
  scale_x_continuous(limits = c(0, 70)) +
  #scale_colour_gradient(low = "red", high = "green") +  # Customize here
  scale_size(range = c(1, 8), breaks = c(2, 3, 4, 5, 6)) +  # Adjust size range (min, max)
  geom_point(size = 3) +
  theme(legend.position = "top")
gg


gg <- ggplot(dt, aes(x = WeeksSinceRelease,
                         y = MeanRelativeRadioStreams7Days,
                     colour = NObsPerWeek)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(limits = c(0, 100)) +
  #scale_colour_gradient(low = "red", high = "green") +  # Customize here
  # scale_size(range = c(1, 8), breaks = c(2, 3, 4, 5, 6)) +  # Adjust size range (min, max)
  labs(title = "MeanRelativeRadioStreams7Days vs WeeksSinceRelease", 
       x = "WeeksSinceRelease", 
       y = "MeanRelativeRadioStreams7Days") +
  theme(legend.position = "top")
gg


gg <- ggplot(dt, aes(x = WeeksSinceReleaseCategory, 
                     y = RelativeRadioStreams7Days)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  scale_y_continuous(limits = c(0, 5)) +
  labs(title = "Boxplots of RelativeRadioStreams7Days for 
       Categories of WeeksSinceRelease", 
       x = "WeeksSinceReleaseCategory", 
       y = "RelativeRadioStreams7Days") +
  theme(legend.position = "top")
gg

ggsave(filename = "Boxplot RelativeRadioStreams.png", path = "03_docs/result_graphics/",
       plot = gg, width = 10, 
       height =  6)

## some plots ####
unique(dt$PI_Category)
plot(dt[PI_Category == "20-21", ]$WeeksSinceRelease, 
     dt[PI_Category == "20-21", ]$RadioStreamsLast7Days,
     xlim = c(1,50),
     ylim = c(0, 1000))
unique(dt$PI_Category_2)
# todo: colorize dependend on stream/listener ratio adj

# for all
plot(dt$WeeksSinceRelease, 
     dt$RadioStreamsLast7Days,
     xlim = c(1,40),
     ylim = c(0, 2000))




plot(dt_early$PopularityIndex, dt_early$RadioStreamsLast7Days, xlim = c(0,30),
     ylim = c(0, 2000))

plot(dt_early$CurrentSpotifyFollowers, dt_early$RadioStreamsLast7Days,
     xlim = c(0,4000),
     ylim = c(0, 2000))
plot(dt_early$ListenersStreamRatio28Days, dt_early$RadioStreamsLast7Days,
     xlim = c(0,4),
     ylim = c(0, 2000))






## log model ####
# !is.na(IsThisSongOptedIntoSpotifyDiscoveryMode)
dt_mod <- dt[!is.na(RadioStreamsLast7Days),
             .(y = RadioStreamsLast7Days,
                 x_1= PopularityIndex,
                 x_2 = IsThisSongOptedIntoSpotifyDiscoveryMode,
                 x_3 = ListenersStreamRatio7Days,
                 x_4 = SavesLast28Days, # nicht signifikant
                 x_5 = PlaylistAddsLast28Days, # signifikant
                 x_6 = CurrentSpotifyFollowers, # nicht signifikant
                 x_7 = ReleaseConsistencyNum, # nicht signifikant
                 x_8 = HowManySongsDoYouHaveInRadioRightNow, # nicht signifikant
                 x_9 = WeeksSinceRelease, # not significant, with linear, cubic and log trend
                 x_10 = NonAlgoEngagement7Days, # nicht signifikant
                 x_11 = as.integer(DaysSinceRelease),
                 x_12 = PopularityIndex/as.integer(DaysSinceRelease) + 1,
                 x_13 = NewRelease
                 )]
dt[, obs := 1:.N]
dt_no_na <- dt[!is.na(RadioStreamsLast7Days)]

# Model 1 with only popularity score
# Final model
model <- lm(log(y+1) ~ log(x_1 + 1) + x_1, data = dt_mod) # 0.7372
summary(model)

# # Model 2 with ListenersStreamRatio
# model <- lm(log(y+1) ~ log(x_1 + 1) + x_1 + log(x_3), data = dt_mod) # 0.7414
# summary(model)
# 
# # Model 3 (Model 2 with NewRelease)
# model <- lm(log(y+1) ~ log(x_1 + 1) + x_1 + log(x_3) + x_13, data = dt_mod) # 0.7506
# summary(model)
# 
# 
# # most complex model
# model <- lm(log(y+1) ~ x_12* log(x_1 + 1) + x_1 + x_2 + log(x_3) + x_11, 
#             data = dt_mod) # 0.7658
# summary(model)
# 
# model <- lm(log(y+1) ~ log(x_1 + 1) + x_1 + x_2 + log(x_3) 
#             + log(x_4 + 1) + log(x_5 + 1), data = dt_mod) # 0.71
# model <- lm(log(y+1) ~ log(x_1 + 1) + x_1, data = dt_mod) # 0.63
# 
# model <- lm(log(y+1) ~ x_12* log(x_1 + 1) + x_1 + x_3, data = dt_mod) # 0.63
# summary(model)

# idea: model weekday -> but 7 day window includes all weekdays so no
# add adjusted Stream/Listener Ratio
# for log on left side
pred_dt <- data.table(y_fitted = round(exp(model$fitted.values) - 1),
                      y = exp(model$model$`log(y + 1)`) - 1,
                      PI = model$model$x_1)
pred_dt[, obs := dt_no_na$obs]

pred_dt_red <- copy(pred_dt)
# without log in left side
# pred_dt <- data.table(y_fitted = model$fitted.values,
#                       y = model$model$y)

pred_dt[, PI_Category := dt_no_na$PI_Category]
pred_dt[, .N, by = .(PI_Category)]
pred_dt[, residuals := y_fitted - y]
pred_dt[, abs_residuals := abs(y_fitted - y)]

# adding relative residuals
z <- 50
pred_dt[, rel_residuals :=  abs((y_fitted) - y)/(y+z)]


pred_dt[, ListenersStreamRatio28Days := dt_no_na$ListenersStreamRatio28Days]
pred_dt[, ListenersStreamRatio7Days := dt_no_na$ListenersStreamRatio7Days]
pred_dt[, CurrentSpotifyFollowers := dt_no_na$CurrentSpotifyFollowers]
pred_dt[, DaysSinceRelease := dt_no_na$DaysSinceRelease]
pred_dt[, PromotionMethod := dt_no_na$WhatIsTheDominantWayThisArtistHasPromotedThisSong]


## Evaluation ####
# mean absolute error
mean(abs(pred_dt$y_fitted - pred_dt$y))
# Model 1: 988.8738
# Model 2: 988.29
# Model 3: 1009.89

# mit daten nur mit DM
# Model 1: 549.33
# Model 2: 479.5788
# Model 3: 445

# median absolute error
median(abs(pred_dt$y_fitted - pred_dt$y))
# Model 1: 117
# Model 2: 110
# Model 3: 111

# mean relative residuals
mean(pred_dt$rel_residuals)
# Model 1: 0.7282167
# Model 2: 0.71395
# Model 3: 0.71

cor(dt$PopularityIndex, dt$RadioStreamsLast7Days, use = "complete.obs")
cor(dt$ListenersLast7Days, dt$RadioStreamsLast7Days, use = "complete.obs")

## Plot actual vs fitted values ####
pred_dt_long_1 <- melt(pred_dt_red[PI <= 25], id.vars = c("obs", "PI"), value.name = "value",
                     variable.name = "type")
pred_dt_long_2 <- melt(pred_dt_red[between(PI, 25, 40, incbounds = FALSE)], 
                       id.vars = c("obs", "PI"), value.name = "value",
                       variable.name = "type")

pred_dt_long_3 <- melt(pred_dt_red[PI >= 40], 
                       id.vars = c("obs", "PI"), value.name = "value",
                       variable.name = "type")

gg <- ggplot(pred_dt_long_1, aes(x = reorder(obs, PI), y = value, group = obs,
                               shape = type)) +
  geom_line(color = "gray", alpha = 0.5, type = "dotted") +  # line from y to y_fitted
  geom_point(aes(color = type), size = 1.7) +
  labs(title = "Fitted and true values of RadioStreamsLast7Days for data points
       with Popularity Index <= 25", 
       x = "data points (ordered by Popularity Index)", 
       y = "RadioStreamsLast7Days") +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
gg

ggsave(filename = "Radio7 fitted vs actual PI smaller 25.png", path = "03_docs/result_graphics/",
       plot = gg, width = 10, 
       height =  6)

gg <- ggplot(pred_dt_long_2, aes(x = reorder(obs, PI), y = value, group = obs,
                                 shape = type)) +
  geom_line(color = "gray", alpha = 0.5, type = "dotted") +  # line from y to y_fitted
  geom_point(aes(color = type), size = 1.7) +
  labs(title = "Fitted and true values of RadioStreamsLast7Days for data points
       with Popularity Index between 25 and 40", 
       x = "data points (ordered by Popularity Index)", 
       y = "RadioStreamsLast7Days") +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
gg

ggsave(filename = "Radio7 fitted vs actual PI between 25 and 40.png", path = "03_docs/result_graphics/",
       plot = gg, width = 10, 
       height =  6)


gg <- ggplot(pred_dt_long_3, aes(x = reorder(obs, PI), y = value, group = obs,
                                 shape = type)) +
  geom_line(color = "gray", alpha = 0.5, type = "dotted") +  # line from y to y_fitted
  geom_point(aes(color = type), size = 1.7) +
  labs(title = "Fitted and true values of RadioStreamsLast7Days for data points
       with Popularity Index > 40 ", 
       x = "data points (ordered by Popularity Index)", 
       y = "RadioStreamsLast7Days") +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
gg

ggsave(filename = "Radio7 fitted vs actual PI over 40.png", path = "03_docs/result_graphics/",
       plot = gg, width = 10, 
       height =  6)







## XGBoost model ####

## 1. Prepare your data
# Keep target as raw y (no log)
dt_mod <- dt[!is.na(RadioStreamsLast7Days),
             .(y = RadioStreamsLast7Days,
               x_1 = PopularityIndex,
               x_2 = IsThisSongOptedIntoSpotifyDiscoveryMode,
               x_4 = SavesLast28Days, # nicht signifikant
               x_5 = PlaylistAddsLast28Days, # signifikant
               x_3 = ListenersStreamRatio7Days,
               x_9 = WeeksSinceRelease,
               x_11 = as.integer(DaysSinceRelease),
               x_13 = ifelse(NewRelease == "yes", 1, 0)
             )]
# Feature selection
# Modell 1 
feature_vars <- paste0("x_", c(1, 3, 13))

# Modell 2
feature_vars <- paste0("x_", c(1, 3, 4, 5, 11, 13))
X <- as.matrix(dt_mod[, ..feature_vars])
y <- dt_mod$y

## 2. Split data into training and test sets
set.seed(42)
train_idx <- sample(1:nrow(X), size = 0.8 * nrow(X))
X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test <- X[-train_idx, ]
y_test <- y[-train_idx]

## 3. Create DMatrix
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

watchlist <- list(train = dtrain, eval = dtest)

## 4. Fit the model with monitoring and early stopping
xgb_model <- xgb.train(
  data = dtrain,
  nrounds = 11,                     # you can increase this for early stopping
  watchlist = watchlist,
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 4,
  eta = 0.1,
  early_stopping_rounds = 10,       # stops if no improvement in 10 rounds on test set
  verbose = 1
)

## 4. Predict
y_pred <- predict(xgb_model, newdata = X)

## 5. Post-process: residuals and custom metric
pred_dt <- data.table(
  y_fitted = round(y_pred),
  y = y,
  PI = dt_mod$x_1
)

z <- 50
pred_dt[, residuals := y_fitted - y]
pred_dt[, rel_residuals := abs(y_fitted - y) / (y + z)]

## Optional: attach PI_Category from original dataset
pred_dt[, PI_Category := dt_no_na$PI_Category]
pred_dt[, obs := dt_no_na$obs]
pred_dt[, ListenersStreamRatio28Days := dt_no_na$ListenersStreamRatio28Days]
pred_dt[, ListenersStreamRatio7Days := dt_no_na$ListenersStreamRatio7Days]
pred_dt[, CurrentSpotifyFollowers := dt_no_na$CurrentSpotifyFollowers]
pred_dt[, DaysSinceRelease := dt_no_na$DaysSinceRelease]
pred_dt[, PromotionMethod := dt_no_na$WhatIsTheDominantWayThisArtistHasPromotedThisSong]

## Evaluation ####
# mean absolute error
mean(abs(pred_dt$y_fitted - pred_dt$y))
# xgboost 1: 950.76
# xgboost 2: 948.60


# median absolute error
median(abs(pred_dt$y_fitted - pred_dt$y))
# xgboost 1: 112
# xgboost 2: 124


# mean relative residuals
mean(pred_dt$rel_residuals)
# xgboost 1: 1.06
# xgboost 2: 1.11













# Graphics DaysSince Release against Radio7 ####
dt[, Artist_Song := paste(ArtistName, SongName, sep = "_")]
dt_show <- dt[, .(n_subs = .N), by = .(Artist_Song)]

# todo: find a way to adjust this and show popularity index adjusted
# idea: adjust into different engagement categories and plot seperate
## Radio Streams Investigation ####
# plot Radio Streams vs Weeks since Release 

gg <- ggplot(dt[between(PopularityIndex, 0, 40)], aes(x = DaysSinceRelease,
                     y = RadioStreamsLast7Days,
                     #shape = PI_Category_2,
                     colour = PopularityIndex)) +
  scale_x_continuous(limits = c(0, 180)) +
  scale_y_continuous(limits = c(0, 10000)) +
  scale_colour_gradient(low = "lightyellow", high = "red") +  # Customize here
  #scale_size(range = c(1, 8), breaks = c(2, 3, 4, 5, 6)) +  # Adjust size range (min, max)
  geom_point(size = 3) +
  theme(legend.position = "top")
gg


# residuals vs other variables
ggplot(pred_dt[between(ListenersStreamRatio7Days, 1, 3)], 
       aes(x = obs, y = rel_residuals,
                    colour = ListenersStreamRatio7Days)) +
  # scale_x_continuous(limits = c(1, 10)) +
  scale_colour_gradient(low = "lightyellow", high = "red") +
  geom_point() +
  theme(legend.position = "top")

ggplot(pred_dt[between(DaysSinceRelease, 1, 100)], 
       aes(x = DaysSinceRelease, y = rel_residuals)) +
  # scale_x_continuous(limits = c(1, 10)) +
  #scale_colour_gradient(low = "lightyellow", high = "red") +
  # scale_colour_gradient(low = "lightyellow", high = "red") +
  geom_point() +
  theme(legend.position = "top")

ggplot(pred_dt[between(PI, 0, 100)], 
       aes(x = PI, y = rel_residuals)) +
  # scale_x_continuous(limits = c(1, 10)) +
  #scale_colour_gradient(low = "lightyellow", high = "red") +
  #scale_colour_gradient(low = "lightyellow", high = "red") +
  geom_point() +
  theme(legend.position = "top")

ggplot(pred_dt[between(PI, 0, 100)], 
       aes(x = PI, y = rel_residuals)) +
  # scale_x_continuous(limits = c(1, 10)) +
  #scale_colour_gradient(low = "lightyellow", high = "red") +
  #scale_colour_gradient(low = "lightyellow", high = "red") +
  geom_point() +
  theme(legend.position = "top")


ggplot(pred_dt, 
       aes(x = PromotionMethod, y = rel_residuals)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 5)) +
  #scale_colour_gradient(low = "lightyellow", high = "red") +
  #scale_colour_gradient(low = "lightyellow", high = "red") +
  theme(legend.position = "top")



# full model full dataset: 98
pred_dt[, obs := 1:.N]
set.seed(123)
pred_dt_sub <- pred_dt[sample(100)]

pred_dt_long <- melt(pred_dt, id.vars = c("obs", "PI", "abs_residuals"), value.name = "value")

ggplot(pred_dt_long, aes(x = reorder(obs, PI), y = value, group = obs,
                         shape = variable)) +
  geom_line(color = "gray", alpha = 0.5, type = "dotted") +  # line from y to y_fitted
  geom_point(aes(color = variable), size = 2) +
  #scale_x_continuous(limits = c(0, 50)) +
  scale_y_continuous(limits = c(0, 5000)) +  # optional: fix x order
  # coord_cartesian(ylim = c(0, 20000)) +  # set ylim (like y-axis zoom)
  labs(title = "Actual vs Fitted Values", x = "", y = "Value") +
  theme(legend.position = "top")


plot(pred_dt$PI, pred_dt$y_fitted, xlim = c(0, 30), ylim = c(0, 10000))
points(pred_dt$PI, pred_dt$y, xlim = c(0, 30), ylim = c(0, 10000), col = "green")




