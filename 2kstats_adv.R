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

bref_players_stats(seasons = 2018:2020, tables = "advanced",
                   include_all_nba = F, only_totals = TRUE, nest_data = FALSE,
                   assign_to_environment = TRUE, widen_data = TRUE, join_data = TRUE,
                   return_message = TRUE)
##### PREP PER GAME #####
class(dataBREFPlayerAdvanced)
head(dataBREFPlayerAdvanced)
View(dataBREFPlayerAdvanced)
train_adv <- dataBREFPlayerAdvanced
names(train_adv)
train_adv[,5] <- train_adv[, 5] + 1
str(train_adv)
names(train_adv)
names(train_adv)[c(1, 5, 17)] <- c("Player.Name", "Year", "team")
train_adv <- train_adv[, -c(2,3,4,6,7,8,9,10,11,12,13,14,19,20,42)]
str(train_adv)
names(train_adv)

##### DATA MERGE #####
merged_data <- merge(train_adv, masterdf, by = c("Player.Name", "Year", "team"))
str(merged_data)
names(merged_data)

##### CORR ANALYSIS #####

corrvariables <- merged_data[,-c(1,2,3,4,30,31)]
str(corrvariables)

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
ready_data<-data.frame(merged_data[, c(1,2,3,4,29,30,31)],transformed[,-25])

ready_data <- na.omit(ready_data)

train_data <- subset(ready_data, Year==2018 | Year==2019)
test_data <- subset(ready_data, Year == 2020)


##### MODEL TESTING #####

fitControl <- trainControl(method = "repeatedcv",number=10,repeats=10)


##### BRNN #####
brnn_model_adv <- caret::train(y=train_data$Rating, 
                                x=train_data[,-c(1:7)],
                                trControl=fitControl,
                                method='brnn') 

brnn_model_adv

# neurons  RMSE      Rsquared   MAE     
# 1        2.289350  0.8825193  1.812066
# 2        2.152106  0.8968697  1.667268
# 3        2.146030  0.8980055  1.638637
# 
# RMSE was used to select the optimal model using the smallest value.
# The final value used for the model was neurons = 3.

brnn_predictions_adv <- predict(brnn_model_adv, newdata = test_data[,-c(1:7)])
results_data_brnn_adv <- data.frame(brnn_predictions_adv, test_data)
results_data_brnn_adv$errors <- abs(results_data_brnn_adv$Rating - results_data_brnn_adv$brnn_predictions_adv)
sum(results_data_brnn_adv$errors)
#1197

p <- ggplot(results_data_brnn_adv, aes(x = Rating, y = brnn_predictions_adv, text = Player.Name)) +
  geom_point() 

brnn_p <- ggplotly(p)
brnn_p


##### LM #####
lm_model_adv <- caret::train(y=train_data$Rating, 
                              x=train_data[,-c(1:7)],
                              trControl=fitControl,
                              method='lm') 

lm_model_adv

# RMSE      Rsquared  MAE     
# 2.495988  0.862241  1.929155

lm_predictions_adv <- predict(lm_model_adv, newdata = test_data[,-c(1:7)])
results_data_lm_adv <- data.frame(lm_predictions_adv, test_data)
results_data_lm_adv$errors <- abs(results_data_lm_adv$Rating - results_data_lm_adv$lm_predictions_adv)
sum(results_data_lm_adv$errors)
#1176

lm_imp <- varImp(lm_model, scale = FALSE)
plot(lm_imp)

p <- ggplot(results_data_lm_game, aes(x = Rating, y = lm_predictions_game, text = Player.Name)) +
  geom_point() 

lm_p <- ggplotly(p)
lm_p

##### RANDOM FOREST #####
rf_model_adv <- caret::train(y=train_data$Rating, 
                              x=train_data[,-c(1:7)],
                              trControl=fitControl,
                              method='rf') 

rf_model_adv

# mtry  RMSE      Rsquared   MAE     
# 2    2.577083  0.8622635  1.999173
# 13    2.300875  0.8824055  1.780526
# 24    2.303650  0.8812860  1.782645

rf_predictions_adv <- predict(rf_model_adv, newdata = test_data[,-c(1:7)])
results_data_rf_adv <- data.frame(rf_predictions_adv, test_data)
results_data_rf_adv$errors <- abs(results_data_rf_adv$Rating - results_data_rf_adv$rf_predictions_adv)
sum(results_data_rf_adv$errors)
#1524

##### XGB #####
xgb_model_adv <- caret::train(y=train_data$Rating, 
                               x=train_data[,-c(1:7)],
                               trControl=fitControl,
                               method='xgbTree') 

xgb_model_adv

# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were nrounds = 50, max_depth = 3, eta = 0.3, gamma =
#   0, colsample_bytree = 0.8, min_child_weight = 1 and subsample = 1.

xgb_predictions_adv <- predict(xgb_model_adv, newdata = test_data[,-c(1:7)])
results_data_xbg_adv <- data.frame(xgb_predictions_adv, test_data)
results_data_xbg_adv$errors <- abs(results_data_xbg_adv$Rating - results_data_xbg_adv$xgb_predictions_adv)
sum(results_data_xbg_adv$errors)
#1483

##### PCR #####
pcr_model_adv <- caret::train(y=train_data$Rating, 
                               x=train_data[,-c(1:7)],
                               trControl=fitControl,
                               method='pcr') 

pcr_model_adv

# ncomp  RMSE      Rsquared   MAE     
# 1      3.872787  0.6728705  3.032842
# 2      3.856825  0.6782514  3.028393
# 3      3.426775  0.7451062  2.660680

pcr_predictions_adv <- predict(pcr_model_adv, newdata = test_data[,-c(1:7)])
results_data_pcr_adv<- data.frame(pcr_predictions_adv, test_data)
results_data_pcr_adv$errors <- abs(results_data_pcr_adv$Rating - results_data_pcr_adv$pcr_predictions_adv)
sum(results_data_pcr_adv$errors)
#1512

##### GLM #####
glm_model_adv <- caret::train(y=train_data$Rating, 
                               x=train_data[,-c(1:7)],
                               trControl=fitControl,
                               method='glmboost') 

glm_model_adv

# mstop  RMSE      Rsquared   MAE     
# 50    2.873183  0.8195779  2.150390
# 100    2.812449  0.8250935  2.110809
# 150    2.790486  0.8273819  2.097812

glm_predictions_adv <- predict(glm_model_adv, newdata = test_data[,-c(1:7)])
results_data_glm_adv <- data.frame(glm_predictions_adv, test_data)
results_data_glm_adv$errors <- abs(results_data_glm_adv$Rating - results_data_glm_adv$glm_predictions_adv)
sum(results_data_glm_adv$errors)
#1362

p <- ggplot(results_data_glm, aes(x = merged_data.Rating, y = glm_predictions, text = merged_data.Player.Name)) +
  geom_point() 

glm_p <- ggplotly(p)
glm_p

##### SAVING MODEL #####
saveRDS(lm_model_adv, "./lm_model_adv.rds")

lm_predictions_adv <- predict(lm_model_adv, newdata = test_data[,-c(1:7)])
results_data_lm_adv <- data.frame(lm_predictions_adv, test_data)
results_data_lm_adv$errors <- abs(results_data_lm_adv$Rating - results_data_lm_adv$lm_predictions_adv)
sum(results_data_lm_adv$errors)
#1176

lm_imp <- varImp(lm_model_adv, scale = FALSE)
plot(lm_imp)

p <-  ggplot(results_data_lm_adv, aes(x = Rating, y = lm_predictions_adv, 
                                        text = Player.Name, color = errors)) +
  geom_point() +
  scale_color_gradientn(colors = wes_palette("FantasticFox1", 3, type = "continuous") ) + 
  ggtitle("Predictions vs. Ratings")+
  geom_abline(color = "red", linetype = "dotdash", size = 1)

lm_p <- ggplotly(p)
lm_p

p <-  ggplot(results_data_lm_adv, aes(x = Rating, y = lm_predictions_adv, 
                                        text = Player.Name, color = errors)) +
  geom_point() +
  scale_color_gradientn(colors = wes_palette("FantasticFox1", 3, type = "continuous") ) + 
  geom_abline(color = "red", linetype = "dotdash", size = 1) + 
  ggtitle("Predictions vs. Ratings by Positions") +
  facet_wrap(slugPosition ~ .)

lm_pos <- ggplotly(p)
lm_pos

p <-  ggplot(results_data_lm_adv, aes(x = Rating, y = lm_predictions_adv, 
                                        text = Player.Name, color = errors)) +
  geom_point() +
  scale_color_gradientn(colors = wes_palette("FantasticFox1", 3, type = "continuous") ) + 
  geom_abline(color = "red", linetype = "dotdash", size = 1) +
  ggtitle("Predictions vs. Ratings by Team")+
  facet_wrap(team ~ .)

lm_team <- ggplotly(p)
lm_team

p <- results_data_lm_adv %>%
  filter(countGamesStarted > 0) %>%
  ggplot(aes(x = Rating, y = lm_predictions_adv, 
             text = Player.Name, color = errors)) +
  geom_point() +
  scale_color_gradientn(colors = wes_palette("FantasticFox1", 3, type = "continuous") ) + 
  ggtitle("Predictions vs. Ratings")+
  geom_abline(color = "red", linetype = "dotdash", size = 1)

lm_starters <- ggplotly(p)
lm_starters

results_data_lm_adv$group_chunk <- cut(results_data_lm_adv$Rating, breaks = c(60,69,79,89,99), c('60-69', '70-79','80-89','90-99'))

p <-  ggplot(results_data_lm_adv, aes(x = Rating, y = lm_predictions_adv, 
                                        text = Player.Name, color = errors)) +
  geom_point() +
  scale_color_gradientn(colors = wes_palette("FantasticFox1", 3, type = "continuous") ) + 
  geom_abline(color = "red", linetype = "dotdash", size = 1) +
  ggtitle("Predictions vs. Ratings")+
  facet_wrap(group_chunk ~ ., scales = 'free')

lm_ratings <- ggplotly(p)
lm_ratings
