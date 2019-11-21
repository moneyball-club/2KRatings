#packages
library(rvest)
library(dplyr)
library(magrittr)
library(nbastatR)
library(caret)
library(corrplot)

setwd("//Users//quilviohernandez//Desktop")

#data collection
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

bref_players_stats(seasons = 2018:2020, tables = c("advanced", "totals", "per_minute", "per_game"), 
                                 include_all_nba = F, only_totals = TRUE, nest_data = FALSE,
                                 assign_to_environment = TRUE, widen_data = TRUE, join_data = TRUE,
                                 return_message = TRUE)

#### PREP ADVANCED
class(dataBREFPlayerAdvanced)
head(dataBREFPlayerAdvanced)
View(dataBREFPlayerAdvanced)
train_adv <- dataBREFPlayerAdvanced
train_adv[,5] <- train_adv[, 5] + 1
str(train_adv)
names(train_adv)
names(train_adv)[c(1, 5, 17)] <- c("Player.Name", "Year", "team")
train_adv_test <- train_adv[, -c(2,4,6,7,8,9,10,11,12,13,14,42)]
View(train_adv_test)

#### PREP


#data merge
bref201819 <- subset(train_adv_test, Year==2018 | Year==2019)
breftest <- subset(train_adv_test, Year == 2020)

merged_data <- merge(train_adv_test, masterdf, by = c("Player.Name", "Year", "team"))
View(merged_data)

# test <- merge(breftest, masterdf, by = c("Player.Name", "Year", "team"))
# View(masterdf)
# View(train_adv_merge)
# unique(train_adv_merge$Year)
# table(train_adv_merge$Year)
# 
# test_adv_merge <- merge(breftest, test, by = c("Player.Name", "Year", "team") )
# View(test_adv_merge)

###data cleaning
# head(model.matrix(Rating ~ ., data = train_adv_merge))

dummies <- dummyVars(Rating ~ ., data = merged_data)
nzv <- nearZeroVar(merged_data, saveMetrics= TRUE)
nzv[nzv$nzv,]

merged_data <- merged_data[,-8]

#### CORRELATION ANALYSIS

corrvariables <- merged_data[,9:31]

corrplot(cor(merged_data[,9:31]), method = "color", type = "upper", col = cm.colors(100))

descrCor <- cor(corrvariables)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
filteredDescr <- corrvariables[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
# descrCor3 <- cbind(filteredDescr, train_adv_merge[,31])
# names(descrCor3)[16] <- "Ratings"

corrplot(descrCor2, method = "color", type = "upper", col = cm.colors(100))

#### CONTINUE

preProcValues <- preProcess(filteredDescr[,-c(2,31)], method = c("center", "scale", "YeoJohnson"))
preProcValues

transformed<-predict(preProcValues, newdata = filteredDescr[,-c(2,31)])
ready_data<-data.frame(merged_data$Rating,merged_data$Year,transformed[,-15])

train_data <- subset(ready_data, merged_data.Year==2018 | merged_data.Year==2019)
test_data <- subset(ready_data, merged_data.Year == 2020)


fitControl <- trainControl(method = "repeatedcv",number=10,repeats=10)

brnn_model <- caret::train(y=train_data$merged_data.Rating, 
                             x=train_data[,-1],
                             trControl=fitControl,
                             method='brnn') #tried method='nnet' but gave bad results

brnn_model

# Bayesian Regularized Neural Networks 
# neurons  RMSE      Rsquared   MAE     
# 1        2.537176  0.8504387  1.901824
# 2        2.489781  0.8555577  1.859597
# 3        2.475014  0.8576149  1.854231
# 
# RMSE was used to select the optimal model using the smallest value.
# The final value used for the model was neurons = 3.

brnn_predictions <- predict(brnn_model, newdata = test_data[,-1])
results_data_brnn <- data.frame(brnn_predictions, test_data)
results_data_brnn$errors <- abs(results_data_brnn$merged_data.Rating - results_data_brnn$brnn_predictions)
sum(results_data_brnn$errors)
#1530

# write.csv(train_data, file = "train_data.csv")
# write.csv(results_data, file = "results.csv")

xgb_model <- caret::train(y=train_data$merged_data.Rating, 
                           x=train_data[,-1],
                           trControl=fitControl,
                           method='xgbTree') 
xgb_model

# 0.4  3          0.8               1.00       150      2.610294  0.8456342  1.991517
# 
# Tuning parameter 'gamma' was held constant at a value of 0
# Tuning parameter 'min_child_weight' was
# held constant at a value of 1
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were nrounds = 100, max_depth = 2, eta = 0.3, gamma =
#   0, colsample_bytree = 0.8, min_child_weight = 1 and subsample = 1.

xgb_predictions <- predict(xgb_model, newdata = test_data[,-1])
results_data_xgb <- data.frame(xgb_predictions, test_data)
results_data_xgb$errors <- abs(results_data_xgb$merged_data.Rating - results_data_xgb$xgb_predictions)
sum(results_data_xgb$errors)
#1452

lm_model <- caret::train(y=train_data$merged_data.Rating, 
                          x=train_data[,-1],
                          trControl=fitControl,
                          method='lm') 
lm_model

# Resampling results:
#   
#   RMSE      Rsquared   MAE     
# 2.894195  0.8079294  2.145151
# 
# Tuning parameter 'intercept' was held constant at a value of TRUE

lm_predictions <- predict(lm_model, newdata = test_data[,-1])
results_data_lm <- data.frame(lm_predictions, test_data)
results_data_lm$errors <- abs(results_data_lm$merged_data.Rating - results_data_lm$lm_predictions)
sum(results_data_lm$errors)
#1408

rf_model <- caret::train(y=train_data$merged_data.Rating, 
                         x=train_data[,-1],
                         trControl=fitControl,
                         method='rf') 
rf_model
# mtry  RMSE      Rsquared   MAE     
# 2    2.619232  0.8633921  1.995339
# 8    2.466305  0.8590897  1.872052
# 15    2.530999  0.8485010  1.894308
# 
# RMSE was used to select the optimal model using the smallest value.
# The final value used for the model was mtry = 8.

rf_predictions <- predict(rf_model, newdata = test_data[,-1])
results_data_rf <- data.frame(rf_predictions, test_data)
results_data_rf$errors <- abs(results_data_rf$merged_data.Rating - results_data_rf$rf_predictions)
sum(results_data_rf$errors)
#1504

gam_model <- caret::train(y=train_data$merged_data.Rating, 
                         x=train_data[,-1],
                         trControl=fitControl,
                         method='gam') 
gam_model

# select  RMSE      Rsquared   MAE     
# FALSE   2.915075  0.8128504  2.003567
# TRUE   2.837884  0.8186052  1.989842

gam_predictions <- predict(gam_model, newdata = test_data[,-1])
results_data_gam <- data.frame(gam_predictions, test_data)
results_data_gam$errors <- abs(results_data_gam$merged_data.Rating - results_data_gam$gam_predictions)
sum(results_data_gam$errors)
#1360

gbm_model <- caret::train(y=train_data$merged_data.Rating, 
                          x=train_data[,-1],
                          trControl=fitControl,
                          method='gbm') 
gbm_model
# interaction.depth  n.trees  RMSE      Rsquared   MAE     
# 1                   50      2.853920  0.8220613  2.160842
# 1                  100      2.620740  0.8401669  1.976787
# 1                  150      2.599260  0.8426921  1.952410
# 2                   50      2.604428  0.8428876  1.953885
# 2                  100      2.542038  0.8492274  1.905408
# 2                  150      2.538590  0.8497147  1.903887
# 3                   50      2.521607  0.8523706  1.902816
# 3                  100      2.495766  0.8551177  1.876184
# 3                  150      2.486554  0.8563681  1.871741

gbm_predictions <- predict(gbm_model, newdata = test_data[,-1])
results_data_gbm <- data.frame(gbm_predictions, test_data)
results_data_gbm$errors <- abs(results_data_gbm$merged_data.Rating - results_data_gbm$gbm_predictions)
sum(results_data_gbm$errors)
#1480

###### BEST MODEL
gam_model <- caret::train(y=train_data$merged_data.Rating, 
                          x=train_data[,-1],
                          trControl=fitControl,
                          method='gam') 
gam_model

# select  RMSE      Rsquared   MAE     
# FALSE   2.915075  0.8128504  2.003567
# TRUE   2.837884  0.8186052  1.989842

gam_predictions <- predict(gam_model, newdata = test_data[,-1])
results_data_gam <- data.frame(gam_predictions, test_data)
results_data_gam$errors <- abs(results_data_gam$merged_data.Rating - results_data_gam$gam_predictions)
results_data_gam <- data.frame(subset(merged_data[,c(1,2)], Year == 2020), results_data_gam)
sum(results_data_gam$errors)

gam_imp <- varImp(gam_model, scale = FALSE)
plot(gam_imp)

p <- ggplot(results_data_gam, aes(x = merged_data.Rating, y = gam_predictions, text = Player.Name)) +
  geom_point() 
  # geom_text(aes(label=ifelse(gam_predictions>80,as.character(Player.Name), "")), hjust=1, vjust=-.4, 
  #           size = 3)
p <- ggplotly(p)


