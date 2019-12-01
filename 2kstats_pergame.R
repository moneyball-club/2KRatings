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

bref_players_stats(seasons = 2018:2020, tables = "per_game",
                   include_all_nba = F, only_totals = TRUE, nest_data = FALSE,
                   assign_to_environment = TRUE, widen_data = TRUE, join_data = TRUE,
                   return_message = TRUE)
##### PREP PER GAME #####
class(dataBREFPlayerPerGame)
head(dataBREFPlayerPerGame)
View(dataBREFPlayerPerGame)
train_adv <- dataBREFPlayerPerGame
names(train_adv)
train_adv[,5] <- train_adv[, 5] + 1
str(train_adv)
names(train_adv)
names(train_adv)[c(1, 5, 17)] <- c("Player.Name", "Year", "team")
train_adv <- train_adv[, -c(2,3,4,6,7,8,9,10,11,12,13,14,25,26,45)]
str(train_adv)
names(train_adv)

##### DATA MERGE #####
merged_data <- merge(train_adv, masterdf, by = c("Player.Name", "Year", "team"))
str(merged_data)
names(merged_data)

##### CORR ANALYSIS #####

corrvariables <- merged_data[,-c(1,2,3,4,33,34)]
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
ready_data<-data.frame(merged_data[, c(1,2,3,4,32,33,34)],transformed[,-28])

ready_data <- na.omit(ready_data)

train_data <- subset(ready_data, Year==2018 | Year==2019)
test_data <- subset(ready_data, Year == 2020)


##### MODEL TESTING #####

fitControl <- trainControl(method = "repeatedcv",number=10,repeats=10)


##### BRNN #####
brnn_model_game <- caret::train(y=train_data$Rating, 
                           x=train_data[,-c(1:7)],
                           trControl=fitControl,
                           method='brnn') 

brnn_model_game

# neurons  RMSE      Rsquared   MAE     
# 1        2.388178  0.8688481  1.839639
# 2        2.231547  0.8827079  1.627655
# 3        2.246643  0.8811261  1.627605
# 
# RMSE was used to select the optimal model using the smallest value.
# The final value used for the model was neurons = 2.

brnn_predictions_game <- predict(brnn_model_game, newdata = test_data[,-c(1:7)])
results_data_brnn_game <- data.frame(brnn_predictions_game, test_data)
results_data_brnn_game$errors <- abs(results_data_brnn_game$Rating - results_data_brnn_game$brnn_predictions_game)
sum(results_data_brnn_game$errors)
#1054



p <- ggplot(results_data_brnn, aes(x = merged_data.Rating, y = brnn_predictions, text = merged_data.Player.Name)) +
  geom_point() 

brnn_p <- ggplotly(p)
brnn_p


##### LM #####
lm_model_game <- caret::train(y=train_data$Rating, 
                         x=train_data[,-c(1:7)],
                         trControl=fitControl,
                         method='lm') 

lm_model_game

# RMSE      Rsquared   MAE     
# 2.805333  0.8195128  2.122601

lm_predictions_game <- predict(lm_model_game, newdata = test_data[,-c(1:7)])
results_data_lm_game <- data.frame(lm_predictions_game, test_data)
results_data_lm_game$errors <- abs(results_data_lm_game$Rating - results_data_lm_game$lm_predictions_game)
sum(results_data_lm_game$errors)
#873

lm_imp <- varImp(lm_model, scale = FALSE)
plot(lm_imp)

p <- ggplot(results_data_lm_game, aes(x = Rating, y = lm_predictions_game, text = Player.Name)) +
  geom_point() 

lm_p <- ggplotly(p)
lm_p

##### RANDOM FOREST #####
rf_model_game <- caret::train(y=train_data$Rating, 
                         x=train_data[,-c(1:7)],
                         trControl=fitControl,
                         method='rf') 

rf_model_game

# mtry  RMSE      Rsquared   MAE     
# 2    2.388944  0.8703246  1.791080
# 14    2.321995  0.8744292  1.728008
# 27    2.317298  0.8746338  1.718244

rf_predictions_game <- predict(rf_model_game, newdata = test_data[,-c(1:7)])
results_data_rf_game <- data.frame(rf_predictions_game, test_data)
results_data_rf_game$errors <- abs(results_data_rf_game$Rating - results_data_rf_game$rf_predictions_game)
sum(results_data_rf_game$errors)
#1085

##### XGB #####
xgb_model_game <- caret::train(y=train_data$Rating, 
                          x=train_data[,-c(1:7)],
                          trControl=fitControl,
                          method='xgbTree') 

xgb_model_game

# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were nrounds = 50, max_depth = 3, eta = 0.3, gamma =
#   0, colsample_bytree = 0.8, min_child_weight = 1 and subsample = 1.

xgb_predictions_game <- predict(xgb_model_game, newdata = test_data[,-c(1:7)])
results_data_xbg_game <- data.frame(xgb_predictions_game, test_data)
results_data_xbg_game$errors <- abs(results_data_xbg_game$Rating - results_data_xbg_game$xgb_predictions_game)
sum(results_data_xbg_game$errors)
#998

##### PCR #####
pcr_model_game <- caret::train(y=train_data$Rating, 
                          x=train_data[,-c(1:7)],
                          trControl=fitControl,
                          method='pcr') 

pcr_model_game

# ncomp  RMSE      Rsquared   MAE     
# 1      3.402395  0.7368979  2.582171
# 2      3.407633  0.7361487  2.584587
# 3      3.391996  0.7373670  2.575950

pcr_predictions_game <- predict(pcr_model_game, newdata = test_data[,-c(1:7)])
results_data_pcr_game <- data.frame(pcr_predictions_game, test_data)
results_data_pcr_game$errors <- abs(results_data_pcr_game$Rating - results_data_pcr_game$pcr_predictions_game)
sum(results_data_pcr_game$errors)
#835

##### GLM #####
glm_model_game <- caret::train(y=train_data$Rating, 
                          x=train_data[,-c(1:7)],
                          trControl=fitControl,
                          method='glmboost') 

glm_model_game

# mstop  RMSE      Rsquared   MAE     
# 50    3.124812  0.7789205  2.318624
# 100    3.056731  0.7878632  2.290388
# 150    3.020407  0.7924958  2.277679

glm_predictions_game <- predict(glm_model_game, newdata = test_data[,-c(1:7)])
results_data_glm_game <- data.frame(glm_predictions_game, test_data)
results_data_glm_game$errors <- abs(results_data_glm_game$Rating - results_data_glm_game$glm_predictions_game)
sum(results_data_glm_game$errors)
#783

p <- ggplot(results_data_glm, aes(x = merged_data.Rating, y = glm_predictions, text = merged_data.Player.Name)) +
  geom_point() 

glm_p <- ggplotly(p)
glm_p

##### SAVING MODEL #####
saveRDS(glm_model_game, "./glm_model_game.rds")

glm_predictions_game <- predict(glm_model_game, newdata = test_data[,-c(1:7)])
results_data_glm_game <- data.frame(glm_predictions_game, test_data)
results_data_glm_game$errors <- abs(results_data_glm_game$Rating - results_data_glm_game$glm_predictions_game)
sum(results_data_glm_game$errors)
#783

glm_imp <- varImp(glm_model_game, scale = FALSE)
plot(brnn_imp)

p <-  ggplot(results_data_glm_game, aes(x = Rating, y = glm_predictions_game, 
                                    text = Player.Name, color = errors)) +
  geom_point() +
  scale_color_gradientn(colors = wes_palette("FantasticFox1", 3, type = "continuous") ) + 
  ggtitle("Predictions vs. Ratings")+
  geom_abline(color = "red", linetype = "dotdash", size = 1)

glm_p <- ggplotly(p)
glm_p

p <-  ggplot(results_data_glm_game, aes(x = Rating, y = glm_predictions_game, 
                                    text = Player.Name, color = errors)) +
  geom_point() +
  scale_color_gradientn(colors = wes_palette("FantasticFox1", 3, type = "continuous") ) + 
  geom_abline(color = "red", linetype = "dotdash", size = 1) + 
  ggtitle("Predictions vs. Ratings by Positions") +
  facet_wrap(slugPosition ~ .)

glm_pos <- ggplotly(p)
glm_pos

p <-  ggplot(results_data_glm_game, aes(x = Rating, y = glm_predictions_game, 
                                    text = Player.Name, color = errors)) +
  geom_point() +
  scale_color_gradientn(colors = wes_palette("FantasticFox1", 3, type = "continuous") ) + 
  geom_abline(color = "red", linetype = "dotdash", size = 1) +
  ggtitle("Predictions vs. Ratings by Team")+
  facet_wrap(team ~ .)

glm_team <- ggplotly(p)
glm_team

p <- results_data_glm_game %>%
  filter(countGamesStarted > 0) %>%
  ggplot(aes(x = Rating, y = glm_predictions_game, 
             text = Player.Name, color = errors)) +
  geom_point() +
  scale_color_gradientn(colors = wes_palette("FantasticFox1", 3, type = "continuous") ) + 
  ggtitle("Predictions vs. Ratings")+
  geom_abline(color = "red", linetype = "dotdash", size = 1)

glm_starters <- ggplotly(p)
glm_starters

results_data_glm_game$group_chunk <- cut(results_data_glm_game$Rating, breaks = c(60,69,79,89,99), c('60-69', '70-79','80-89','90-99'))

p <-  ggplot(results_data_glm_game, aes(x = Rating, y = glm_predictions_game, 
                                    text = Player.Name, color = errors)) +
  geom_point() +
  scale_color_gradientn(colors = wes_palette("FantasticFox1", 3, type = "continuous") ) + 
  geom_abline(color = "red", linetype = "dotdash", size = 1) +
  ggtitle("Predictions vs. Ratings")+
  facet_wrap(group_chunk ~ ., scales = 'free')

glm_ratings <- ggplotly(p)
glm_ratings
