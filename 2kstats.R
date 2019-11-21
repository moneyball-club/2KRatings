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
ready_data<-data.frame(merged_data$Rating,merged_data$Year,transformed)

train_data <- subset(ready_data, merged_data.Year==2018 | merged_data.Year==2019)
test_data <- subset(ready_data, merged_data.Year == 2020)


fitControl <- trainControl(method = "repeatedcv",number=10,repeats=10)

brnn_model <- caret::train(y=train_data$merged_data.Rating, 
                             x=train_data[,-1],
                             trControl=fitControl,
                             method='brnn') #tried method='nnet' but gave bad results

brnn_model

# Bayesian Regularized Neural Networks 
# 
# 620 samples
# 16 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 10 times) 
# Summary of sample sizes: 557, 559, 559, 557, 557, 556, ... 
# Resampling results across tuning parameters:
#   
#   neurons  RMSE        Rsquared   MAE       
# 1        0.75400427  0.9875983  0.51789637
# 2        0.07473078  0.9998764  0.05487381
# 3        0.06659471  0.9999064  0.04542111
# 
# RMSE was used to select the optimal model using the smallest value.
# The final value used for the model was neurons = 3.

brnn_predictions <- predict(brnn_model, newdata = test_data[,-1])
results_data <- data.frame(brnn_predictions, test_data)

write.csv(train_data, file = "train_data.csv")
write.csv(results_data, file = "results.csv")
