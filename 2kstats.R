library(rvest)
library(dplyr)
library(magrittr)

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

page <- read_html('https://nba2k18.2kratings.com/team/philadelphia-76ers')
data.raw <- html_table(page, fill=TRUE)
data.raw
add_team <- cbind(as.data.frame(data.raw), "76ers", "2018")
masterdf <- rbind(masterdf, as.data.frame(data.raw))

masterdf <- data.frame()

for (team in teams){
  page <- read_html(paste0('https://nba2k18.2kratings.com/team/', team))
  data.raw <- html_table(page, fill=TRUE)
  data.raw <- cbind(as.data.frame(data.raw), team, "2018")
  masterdf <- rbind(masterdf, data.raw)
}
View(masterdf)

page <- read_html('https://nba2k19.2kratings.com/team/charlotte-hornets')
data.raw <- html_table(page, fill=TRUE)
data.raw <- cbind(as.data.frame(data.raw), "charlotte-hornets", "2019")
data.raw <- data.raw[, -6]
names(data.raw)[c(6, 7)] <- c("team", "Year")
names(data.raw)[3] <- "team"
View(data.raw)
masterdf <- rbind(masterdf, data.raw)

names(masterdf)
names(data.raw)

names(masterdf)[c(3, 7)] <- c("Rating", "Year")
for (team in teams){
  page <- read_html(paste0('https://nba2k19.2kratings.com/team/', team))
  data.raw <- html_table(page, fill=TRUE)
  data.raw <- cbind(as.data.frame(data.raw), team, "2019")
  data.raw <- data.raw[, -6]
  names(data.raw)[7] <- "Year"
  masterdf <- rbind(masterdf, data.raw)
}
View(masterdf)

page <- read_html("https://www.2kratings.com/nba2k20-team/atlanta-hawks")
data.raw <- html_table(page, fill=TRUE)
data.raw <- cbind(as.data.frame(data.raw), "hawks", "2019")
View(data.raw)
data.raw <- data.raw[, -c(4,5,8)]
View(data.raw)

test <- data.frame()

for (team in teams){
  page <- read_html(paste0('https://www.2kratings.com/nba2k20-team/', team))
  data.raw <- html_table(page, fill=TRUE)
  data.raw <- cbind(as.data.frame(data.raw), team, "2020")
  data.raw <- data.raw[, -c(4,5,8)]
  test <- rbind(test, data.raw)
}

names(test)[c(3, 7)] <- c("Rating", "Year")
View(test)
