x <- read.csv("Results/NCAA_MLAX_Results_2017.csv", as.is = T)
y <- read.csv("Results/NCAA_MLAX_Results_2016.csv", as.is = T)
z <- read.csv("Results/NCAA_MLAX_Results_2015.csv", as.is = T)
w <- read.csv("Results/NCAA_MLAX_Results_2014.csv", as.is = T)

x <- rbind(x,y,z,w)

# Clean data
these <- grep("@", x$opponent)
x <- x[-these, ]
x <- x[x$opponent != "Cornell College" & x$opponent != "St. Thomas Aquinas" & x$opponent!= "Walsh",]
x$team <- gsub("&#x27;", "'", x$team)
x$opponent <- gsub("&#x27;", "'", x$opponent)

x$weights <- abs(2013 - x$year) * 2

# Clean
x$OT[!is.na(x$teamscore) & is.na(x$OT)] <- 0
x$scorediff <- x$teamscore - x$oppscore

# Get Ivy Games
ivy <- c("Princeton", "Yale", "Harvard", "Penn", "Brown",  "Cornell",   
         "Dartmouth")
x$ivy <- 0
for(i in 1:nrow(x)){
  x$ivy[i] <- is.element(x$team[i], ivy) +  is.element(x$opponent[i], ivy)
}


# Create Model
lm.hoops <- lm(scorediff ~ team + opponent + location, weights = weights, data = x)
summary(lm.hoops)

# Point Spread to Win Percentage Model
x$winprob <- NA
x$winprob[x$scorediff > 0] <- 1
x$winprob[x$scorediff < 0] <- 0

x$predscorediff <- round(predict(lm.hoops, newdata = x), 1)


glm.pointspread <- glm(winprob ~ predscorediff, data = x)
summary(glm.pointspread)

x$winprob[is.na(x$winprob)] <- 
  round(predict.glm(glm.pointspread, newdata = x[is.na(x$winprob),]), 3)

x$winprob[x$winprob >= 1 & is.na(x$scorediff)] <- 0.999
x$winprob[x$winprob <= 0 & is.na(x$scorediff)] <- 0.001


# All Ivy Games
all_ivy_games <- x[x$ivy == 2 & x$year == 2017, ]


# power rankings
teams <- unique(x$team)
powerrankings <- data.frame(Team = rep(NA, length(teams)),
                            YUSAG_Coefficient = rep(NA, length(teams)))
powerrankings[1, ] <- c(teams[1], 0)

for(i in 2:(length(teams))) {
  teamcoef <- 
    lm.hoops$coefficients[paste("team", teams[i], sep = "")]
  opponentcoef <- 
    lm.hoops$coefficients[paste("opponent", teams[i], sep = "")]
  tmp <- c(teams[i], round((teamcoef - opponentcoef)/2, 1))
  powerrankings[i, ] <- tmp
}
powerrankings$YUSAG_Coefficient <- as.numeric(powerrankings$YUSAG_Coefficient)
powerrankings <- powerrankings[order(powerrankings$YUSAG_Coefficient, decreasing = T), ]
powerrankings$rank <- seq(1, 71, 1)

IVY <- data.frame(Team = ivy,
                  YUSAG_Coefficient = rep(NA, 7),
                  Expected_Wins = rep(NA, 7),
                  Expected_Losses = rep(NA, 7))
for(i in 1:7){
  IVY$YUSAG_Coefficient[IVY$Team == ivy[i]] <- 
    as.numeric(powerrankings$YUSAG_Coefficient[powerrankings$Team == ivy[i]])
  IVY$Expected_Wins[i] <- round(sum(all_ivy_games$winprob[all_ivy_games$team == ivy[i]]), 1)
  IVY$Expected_Losses[i] <- 6 - round(sum(all_ivy_games$winprob[all_ivy_games$team == ivy[i]]), 1)
}

# write results
write.table(powerrankings, "MLAX_Powerrankings.csv", row.names = F, col.names = T, sep = ",")
write.table(IVY, "MLAX_Ivy_League_Predicted_Results.csv", row.names = F, col.name = T, sep = ",")
write.table(all_ivy_games, "MLAX_Ivy_Games.csv", row.names = F, col.names = T, sep = ",")


# Playoffs
games <- all_ivy_games[all_ivy_games$location == "H", ]
games$simwins <- NA
games$oppsimwins <- NA

nums <- c(1:7)
B <- 2500

# Data Frame to Hold Team Wins by Sim
simresults <- data.frame(Princeton = rep(NA, B),
                         Yale = rep(NA, B),
                         Harvard = rep(NA, B),
                         Penn = rep(NA, B),
                         Brown = rep(NA, B),
                         Cornell = rep(NA, B),
                         Dartmouth = rep(NA, B))



# Simulate All Games
for (j in 1:nrow(simresults)){
  print(paste("Sim: ", j, sep = ""))
  for(i in 1:nrow(games)) {
    if(games$winprob[i] == 1){
      games$simwins[i] <- 1
      games$oppsimwins[i] <- 0
    }
    else if(games$winprob[i] == 0){
      games$simwins[i] <- 0
      games$oppsimwins[i] <- 1
    }
    else if(games$winprob[i] < 1 & games$winprob[i] > 0){
      rand <- runif(1)
      if(games$winprob[i] >= rand) {
        games$simwins[i] <- 1
        games$oppsimwins[i] <- 0
      }
      else{
        games$simwins[i] <- 0
        games$oppsimwins[i] <- 1
      }
    }
  }
  for(i in 1:8) {
    simresults[j, i] <- (sum(games$simwins[games$team == ivy[i]]) + 
                           sum(games$oppsimwins[games$opponent == ivy[i]]))
  }
}
 


# Sort Sims
PRIN <- rep(NA, nrow(simresults))
YALE <- rep(NA, nrow(simresults))
HARV <- rep(NA, nrow(simresults))
CORN <- rep(NA, nrow(simresults))
DART <- rep(NA, nrow(simresults))
PENN <- rep(NA, nrow(simresults))
BRWN <- rep(NA, nrow(simresults))

for(i in 1:nrow(simresults)) {
  print(paste("Sorting Season: ", i, sep = ""))
  
  PRIN[i] <- nums[names(sort(simresults[i,], decreasing = T)) == "Princeton"]
  YALE[i] <- nums[names(sort(simresults[i,], decreasing = T)) == "Yale"]
  HARV[i] <- nums[names(sort(simresults[i,], decreasing = T)) == "Harvard"]
  BRWN[i] <- nums[names(sort(simresults[i,], decreasing = T)) == "Brown"]
  PENN[i] <- nums[names(sort(simresults[i,], decreasing = T)) == "Penn"]
  CORN[i] <- nums[names(sort(simresults[i,], decreasing = T)) == "Cornell"]
  DART[i] <- nums[names(sort(simresults[i,], decreasing = T)) == "Dartmouth"]
}


playoffs <- data.frame(Team = ivy,
                       playoff_prob = rep(NA, 7),
                       seed1_prob = rep(NA, 7),
                       seed2_prob = rep(NA, 7),
                       seed3_prob = rep(NA, 7),
                       seed4_prob = rep(NA, 7))

playoffs[playoffs$Team == "Princeton", "playoff_prob"] <- round(sum(PRIN <= 4)*100/B, 1)
playoffs[playoffs$Team == "Princeton", "seed1_prob"] <- round(sum(PRIN == 1)*100/B, 1)
playoffs[playoffs$Team == "Princeton", "seed2_prob"] <- round(sum(PRIN == 2)*100/B, 1)
playoffs[playoffs$Team == "Princeton", "seed3_prob"] <- round(sum(PRIN == 3)*100/B, 1)
playoffs[playoffs$Team == "Princeton", "seed4_prob"] <- round(sum(PRIN == 4)*100/B, 1)

playoffs[playoffs$Team == "Yale", "playoff_prob"] <- round(sum(YALE <= 4)*100/B, 1)
playoffs[playoffs$Team == "Yale", "seed1_prob"] <- round(sum(YALE == 1)*100/B, 1)
playoffs[playoffs$Team == "Yale", "seed2_prob"] <- round(sum(YALE == 2)*100/B, 1)
playoffs[playoffs$Team == "Yale", "seed3_prob"] <- round(sum(YALE == 3)*100/B, 1)
playoffs[playoffs$Team == "Yale", "seed4_prob"] <- round(sum(YALE == 4)*100/B, 1)

playoffs[playoffs$Team == "Harvard", "playoff_prob"] <- round(sum(HARV <= 4)*100/B, 1)
playoffs[playoffs$Team == "Harvard", "seed1_prob"] <- round(sum(HARV == 1)*100/B, 1)
playoffs[playoffs$Team == "Harvard", "seed2_prob"] <- round(sum(HARV == 2)*100/B, 1)
playoffs[playoffs$Team == "Harvard", "seed3_prob"] <- round(sum(HARV == 3)*100/B, 1)
playoffs[playoffs$Team == "Harvard", "seed4_prob"] <- round(sum(HARV == 4)*100/B, 1)

playoffs[playoffs$Team == "Penn", "playoff_prob"] <- round(sum(PENN <= 4)*100/B, 1)
playoffs[playoffs$Team == "Penn", "seed1_prob"] <- round(sum(PENN == 1)*100/B, 1)
playoffs[playoffs$Team == "Penn", "seed2_prob"] <- round(sum(PENN == 2)*100/B, 1)
playoffs[playoffs$Team == "Penn", "seed3_prob"] <- round(sum(PENN == 3)*100/B, 1)
playoffs[playoffs$Team == "Penn", "seed4_prob"] <- round(sum(PENN == 4)*100/B, 1)

playoffs[playoffs$Team == "Brown", "playoff_prob"] <- round(sum(BRWN <= 4)*100/B, 1)
playoffs[playoffs$Team == "Brown", "seed1_prob"] <- round(sum(BRWN == 1)*100/B, 1)
playoffs[playoffs$Team == "Brown", "seed2_prob"] <- round(sum(BRWN == 2)*100/B, 1)
playoffs[playoffs$Team == "Brown", "seed3_prob"] <- round(sum(BRWN == 3)*100/B, 1)
playoffs[playoffs$Team == "Brown", "seed4_prob"] <- round(sum(BRWN == 4)*100/B, 1)

playoffs[playoffs$Team == "Cornell", "playoff_prob"] <- round(sum(CORN <= 4)*100/B, 1)
playoffs[playoffs$Team == "Cornell", "seed1_prob"] <- round(sum(CORN == 1)*100/B, 1)
playoffs[playoffs$Team == "Cornell", "seed2_prob"] <- round(sum(CORN == 2)*100/B, 1)
playoffs[playoffs$Team == "Cornell", "seed3_prob"] <- round(sum(CORN == 3)*100/B, 1)
playoffs[playoffs$Team == "Cornell", "seed4_prob"] <- round(sum(CORN == 4)*100/B, 1)

playoffs[playoffs$Team == "Dartmouth", "playoff_prob"] <- round((sum(DART <= 4))*100/B, 1)
playoffs[playoffs$Team == "Dartmouth", "seed1_prob"] <- round(sum(DART == 1)*100/B, 1)
playoffs[playoffs$Team == "Dartmouth", "seed2_prob"] <- round(sum(DART == 2)*100/B, 1)
playoffs[playoffs$Team == "Dartmouth", "seed3_prob"] <- round(sum(DART == 3)*100/B, 1)
playoffs[playoffs$Team == "Dartmouth", "seed4_prob"] <- round(sum(DART == 4)*100/B, 1)
playoffs[playoffs$Team == "Dartmouth", "seed4_prob"] <- round(sum(DART == 4)*100/B, 1)


write.table(playoffs, "MLAX_playoffs.csv", row.names = F, col.names = T, sep = ",")













table(simresults$Dartmouth)
