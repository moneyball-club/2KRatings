###### packages #####
library(rvest)
library(dplyr)
library(magrittr)
library(nbastatR)
library(caret)
library(corrplot)
library(plotly)
library(wesanderson)

setwd("//Users//quilviohernandez//Desktop")

##### data collection #####
teams = c("atlanta-hawks", 
          "boston-celtics", 
          "brooklyn-nets",
          "charlotte-hornets",
          "chicago-bulls",
          "cleveland-cavaliers",
          "dallas-mavericks",
          "denver-nuggets",
          "detroit-pistons",
          "golden-state-warriors",
          "houston-rockets",
          "indiana-pacers",
          "los-angeles-clippers",
          "los-angeles-lakers",
          "memphis-grizzlies",
          "miami-heat",
          "milwaukee-bucks",
          "minnesota-timberwolves",
          "new-orleans-pelicans",
          "new-york-knicks",
          "oklahoma-city-thunder",
          "orlando-magic",
          "philadelphia-76ers",
          "phoenix-suns",
          "portland-trail-blazers",
          "sacramento-kings",
          "san-antonio-spurs",
          "toronto-raptors",
          "utah-jazz",
          "washington-wizards")

teams_abbr = c("ATL",
               "BRK",
               "BOS",
               "CHO",
               "CHI",
               "CLE",
               "DAL",
               "DEN",
               "DET",
               "GSW",
               "HOU",
               "IND",
               "LAC",
               "LAL",
               "MEM",
               "MIA",
               "MIL",
               "MIN",
               "NOP",
               "NYK",
               "OKC",
               "ORL",
               "PHI",
               "PHO",
               "POR",
               "SAC",
               "SAS",
               "TOR",
               "UTA",
               "WAS")

# page <- read_html('https://nba2k18.2kratings.com/team/philadelphia-76ers')
# data.raw <- html_table(page, fill=TRUE)
# data.raw
# add_team <- cbind(as.data.frame(data.raw), "76ers", "2018")
# masterdf <- rbind(masterdf, as.data.frame(data.raw))

masterdf <- data.frame()

for (team in teams){
  page <- read_html(paste0('https://nba2k18.2kratings.com/team/', team))
  data.raw <- html_table(page, fill=TRUE)
  data.raw <- cbind(as.data.frame(data.raw), team, "2018")
  masterdf <- rbind(masterdf, data.raw)
}
View(masterdf)

# page <- read_html('https://nba2k19.2kratings.com/team/charlotte-hornets')
# data.raw <- html_table(page, fill=TRUE)
# data.raw <- cbind(as.data.frame(data.raw), "charlotte-hornets", "2019")
# data.raw <- data.raw[, -6]
# names(data.raw)[c(6, 7)] <- c("team", "Year")
# names(data.raw)[3] <- "team"
# View(data.raw)
# masterdf <- rbind(masterdf, data.raw)

# names(masterdf)
# names(data.raw)

names(masterdf)[c(3, 7)] <- c("Rating", "Year")
for (team in teams){
  page <- read_html(paste0('https://nba2k19.2kratings.com/team/', team))
  data.raw <- html_table(page, fill=TRUE)
  data.raw <- cbind(as.data.frame(data.raw), team, "2019")
  data.raw <- data.raw[, -6]
  names(data.raw)[7] <- "Year"
  masterdf <- rbind(masterdf, data.raw)
}

for (team in teams){
  page <- read_html(paste0('https://www.2kratings.com/nba2k20-team/', team))
  data.raw <- html_table(page, fill=TRUE)
  data.raw <- cbind(as.data.frame(data.raw), team, "2020")
  data.raw <- data.raw[, -c(4,5,8)]
  names(data.raw)[c(3,7)] <- c("Rating","Year")
  masterdf <- rbind(masterdf, data.raw)
}

i = 1
# teams[i]
# 
masterdf$team <- as.character(masterdf$team)
# masterdf$team[is.na(masterdf$team)] <- "ATL"
# 
# masterdf$team[masterdf$team == "boston-celtics"] <- "BOS"
# teams[4]
# teams_abbr[4]
# masterdf$team[masterdf$team == teams[4]] <- teams_abbr[4]
for (val in teams_abbr){
  masterdf$team[masterdf$team == teams[i]] <- val
  i = i + 1
}
masterdf$team <- as.factor(masterdf$team)
View(masterdf)

# page <- read_html("https://www.2kratings.com/nba2k20-team/atlanta-hawks")
# data.raw <- html_table(page, fill=TRUE)
# data.raw <- cbind(as.data.frame(data.raw), "hawks", "2019")
# View(data.raw)
# data.raw <- data.raw[, -c(4,5,8)]
# View(data.raw)

bref_players_stats(seasons = 2018:2020, tables = "per_minute",
                   include_all_nba = F, only_totals = TRUE, nest_data = FALSE,
                   assign_to_environment = TRUE, widen_data = TRUE, join_data = TRUE,
                   return_message = TRUE)
##### PREP PER MINUTE #####
class(dataBREFPlayerPerMinute)
head(dataBREFPlayerPerMinute)
View(dataBREFPlayerPerMinute)
train_adv <- dataBREFPlayerPerMinute
names(train_adv)
train_adv[,5] <- train_adv[, 5] + 1
str(train_adv)
names(train_adv)
names(train_adv)[c(1, 5, 17)] <- c("Player.Name", "Year", "team")
train_adv <- train_adv[, -c(2,3,4,6,7,8,9,10,11,12,13,14,24,25,44)]
str(train_adv)
names(train_adv)

##### DATA MERGE #####
merged_data <- merge(train_adv, masterdf, by = c("Player.Name", "Year", "team"))
View(merged_data)

##### CORR ANALYSIS #####

corrvariables <- merged_data[,-c(1,2,3,4,30,32,33)]

corrplot(cor(corrvariables), method = "color", type = "upper", col = cm.colors(100))

descrCor <- cor(corrvariables)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .9)
filteredDescr <- corrvariables[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)

corrplot(descrCor2, method = "color", type = "upper", col = cm.colors(100))

##### DATA CLEAN AND SPLIT #####

# dummies <- dummyVars(Rating ~ ., data = merged_data)
# nzv <- nearZeroVar(merged_data, saveMetrics= TRUE)
# nzv[nzv$nzv,]

preProcValues <- preProcess(corrvariables, method = c("center", "scale", "YeoJohnson"))
preProcValues

transformed<-predict(preProcValues, newdata = corrvariables)
ready_data<-data.frame(merged_data[, c(1,2,3,4,31)],transformed[,-26])

ready_data <- na.omit(ready_data)

train_data <- subset(ready_data, Year==2018 | Year==2019)
test_data <- subset(ready_data, Year == 2020)


##### MODEL TESTING #####

fitControl <- trainControl(method = "repeatedcv",number=10,repeats=10)


##### BRNN #####
brnn_model_minute <- caret::train(y=train_data$Rating, 
                           x=train_data[,-c(1,2,3,4,5)],
                           trControl=fitControl,
                           method='brnn') 

brnn_model_minute

# neurons  RMSE      Rsquared   MAE     
# 1        2.371944  0.8708874  1.851166
# 2        2.266825  0.8812597  1.687928
# 3        2.135605  0.8935195  1.614598
# 
# RMSE was used to select the optimal model using the smallest value.
# The final value used for the model was neurons = 3.

brnn_predictions <- predict(brnn_model_minute, newdata = test_data[,-c(1,2,3,4,5)])
results_data_brnn_minute <- data.frame(brnn_predictions, test_data)
results_data_brnn_minute$errors <- abs(results_data_brnn_minute$Rating - results_data_brnn_minute$brnn_predictions)
sum(results_data_brnn_minute$errors)
#1019



p <- ggplot(results_data_brnn, aes(x = merged_data.Rating, y = brnn_predictions, text = merged_data.Player.Name)) +
  geom_point() 

brnn_p <- ggplotly(p)
brnn_p


##### LM #####
lm_model <- caret::train(y=train_data$merged_data.Rating, 
                           x=train_data[,-c(1,2,3)],
                           trControl=fitControl,
                           method='lm') 

lm_model

# RMSE      Rsquared   MAE    
# 3.115262  0.7816021  2.34318

lm_predictions <- predict(lm_model, newdata = test_data[,-c(1,2,3)])
results_data_lm <- data.frame(lm_predictions, test_data)
results_data_lm$errors <- abs(results_data_lm$merged_data.Rating - results_data_lm$lm_predictions)
sum(results_data_lm$errors)
#1142

lm_imp <- varImp(lm_model, scale = FALSE)
plot(lm_imp)

p <- ggplot(results_data_lm, aes(x = merged_data.Rating, y = lm_predictions, text = merged_data.Player.Name)) +
  geom_point() 

lm_p <- ggplotly(p)
lm_p

##### RANDOM FOREST #####
rf_model <- caret::train(y=train_data$merged_data.Rating, 
                         x=train_data[,-c(1,2,3)],
                         trControl=fitControl,
                         method='rf') 

rf_model

# mtry  RMSE      Rsquared   MAE     
# 2    2.587239  0.8613736  1.943097
# 13    2.436336  0.8648648  1.840658
# 25    2.469455  0.8586485  1.848335

rf_predictions <- predict(rf_model, newdata = test_data[,-c(1,2,3)])
results_data_rf <- data.frame(rf_predictions, test_data)
results_data_rf$errors <- abs(results_data_rf$merged_data.Rating - results_data_rf$rf_predictions)
sum(results_data_rf$errors)
#1392.726

p <- ggplot(results_data_rf, aes(x = merged_data.Rating, y = rf_predictions, text = merged_data.Player.Name)) +
  geom_point() 

rf_p <- ggplotly(p)
rf_p

##### XGB #####
xgb_model <- caret::train(y=train_data$merged_data.Rating, 
                          x=train_data[,-c(1,2,3)],
                          trControl=fitControl,
                          method='xgbTree') 

xgb_model

# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were nrounds = 100, max_depth = 3, eta = 0.3, gamma =
#   0, colsample_bytree = 0.8, min_child_weight = 1 and subsample = 1.

xgb_predictions <- predict(xgb_model, newdata = test_data[,-c(1,2,3)])
results_data_xbg <- data.frame(xgb_predictions, test_data)
results_data_xbg$errors <- abs(results_data_xbg$merged_data.Rating - results_data_xbg$xgb_predictions)
sum(results_data_xbg$errors)
#1390

p <- ggplot(results_data_xbg, aes(x = merged_data.Rating, y = xgb_predictions, text = merged_data.Player.Name)) +
  geom_point() 

xgb_p <- ggplotly(p)
xgb_p

##### PCR #####
pcr_model <- caret::train(y=train_data$merged_data.Rating, 
                          x=train_data[,-c(1,2,3)],
                          trControl=fitControl,
                          method='pcr') 

pcr_model

# ncomp  RMSE      Rsquared   MAE     
# 1      4.180405  0.6070785  3.163446
# 2      3.589444  0.7142101  2.676946
# 3      3.580926  0.7168432  2.663089

pcr_predictions <- predict(pcr_model, newdata = test_data[,-c(1,2,3)])
results_data_pcr <- data.frame(pcr_predictions, test_data)
results_data_pcr$errors <- abs(results_data_pcr$merged_data.Rating - results_data_pcr$pcr_predictions)
sum(results_data_pcr$errors)
#1191

p <- ggplot(results_data_pcr, aes(x = merged_data.Rating, y = pcr_predictions, text = merged_data.Player.Name)) +
  geom_point() 

pcr_p <- ggplotly(p)
pcr_p

##### GLM #####
glm_model <- caret::train(y=train_data$merged_data.Rating, 
                          x=train_data[,-c(1,2,3)],
                          trControl=fitControl,
                          method='glmboost') 

glm_model

# mstop  RMSE      Rsquared   MAE     
# 50    3.299443  0.7627963  2.486566
# 100    3.169138  0.7768609  2.397148
# 150    3.126530  0.7817303  2.375513

glm_predictions <- predict(glm_model, newdata = test_data[,-c(1,2,3)])
results_data_glm <- data.frame(glm_predictions, test_data)
results_data_glm$errors <- abs(results_data_glm$merged_data.Rating - results_data_glm$glm_predictions)
sum(results_data_glm$errors)
#1227

p <- ggplot(results_data_glm, aes(x = merged_data.Rating, y = glm_predictions, text = merged_data.Player.Name)) +
  geom_point() 

glm_p <- ggplotly(p)
glm_p

##### SAVING MODEL #####
saveRDS(brnn_model, "./brnn_model.rds")

brnn_predictions <- predict(brnn_model, newdata = test_data[,-c(1,2,3)])
results_data_brnn <- data.frame(brnn_predictions, test_data)
results_data_brnn$errors <- abs(results_data_brnn$merged_data.Rating - results_data_brnn$brnn_predictions)
sum(results_data_brnn$errors)
#1019

brnn_imp <- varImp(brnn_model_minute, scale = FALSE)
plot(brnn_imp)

p <-  ggplot(results_data_brnn_minute, aes(x = Rating, y = brnn_predictions, 
                                    text = Player.Name, color = errors)) +
  geom_point() +
  scale_color_gradientn(colors = wes_palette("FantasticFox1", 3, type = "continuous") ) + 
  ggtitle("Predictions vs. Ratings")+
  geom_abline(color = "red", linetype = "dotdash", size = 1)

brnn_p <- ggplotly(p)
brnn_p

p <-  ggplot(results_data_brnn_minute, aes(x = Rating, y = brnn_predictions, 
                                    text = Player.Name, color = errors)) +
  geom_point() +
  scale_color_gradientn(colors = wes_palette("FantasticFox1", 3, type = "continuous") ) + 
  geom_abline(color = "red", linetype = "dotdash", size = 1) + 
  ggtitle("Predictions vs. Ratings by Positions") +
  facet_wrap(slugPosition ~ .)

brnn_pos <- ggplotly(p)
brnn_pos

p <-  ggplot(results_data_brnn_minute, aes(x = Rating, y = brnn_predictions, 
                                    text = Player.Name, color = errors)) +
  geom_point() +
  scale_color_gradientn(colors = wes_palette("FantasticFox1", 3, type = "continuous") ) + 
  geom_abline(color = "red", linetype = "dotdash", size = 1) +
  ggtitle("Predictions vs. Ratings by Team")+
  facet_wrap(team ~ .)

brnn_team <- ggplotly(p)
brnn_team

p <- results_data_brnn_minute %>%
        filter(countGamesStarted > 0) %>%
  ggplot(aes(x = Rating, y = brnn_predictions, 
                                text = Player.Name, color = errors)) +
  geom_point() +
  scale_color_gradientn(colors = wes_palette("FantasticFox1", 3, type = "continuous") ) + 
  ggtitle("Predictions vs. Ratings")+
  geom_abline(color = "red", linetype = "dotdash", size = 1)

brnn_starters <- ggplotly(p)
brnn_starters

results_data_brnn_minute$group_chunk <- cut(results_data_brnn_minute$Rating, breaks = c(60,69,79,89,99), c('60-69', '70-79','80-89','90-99'))

p <-  ggplot(results_data_brnn_minute, aes(x = Rating, y = brnn_predictions, 
                                    text = Player.Name, color = errors)) +
  geom_point() +
  scale_color_gradientn(colors = wes_palette("FantasticFox1", 3, type = "continuous") ) + 
  geom_abline(color = "red", linetype = "dotdash", size = 1) +
  ggtitle("Predictions vs. Ratings")+
  facet_wrap(group_chunk ~ ., scales = 'free')

brnn_ratings <- ggplotly(p)
brnn_ratings

