v <- read.csv("Results/NCAA_WLAX_Results_2018.csv", as.is = T)
x <- read.csv("Results/NCAA_WLAX_Results_2017.csv", as.is = T)
y <- read.csv("Results/NCAA_WLAX_Results_2016.csv", as.is = T)
z <- read.csv("Results/NCAA_WLAX_Results_2015.csv", as.is = T)
w <- read.csv("Results/NCAA_WLAX_Results_2014.csv", as.is = T)

x <- rbind(v,x,y,z,w)

# Clean data
these <- grep("@", x$opponent)
x <- x[-these, ]
x <- x[x$opponent != "Cornell College" & x$opponent != "St. Thomas Aquinas" & x$opponent!= "Walsh" & x$opponent != "Alderson Broaddus" & x$opponent != "Chowan" & x$opponent != "Loras",]
x$team <- gsub("&#x27;", "'", x$team)
x$opponent <- gsub("&#x27;", "'", x$opponent)

x$weights <- factorial(abs(2013 - x$year))

# Clean
x$OT[!is.na(x$teamscore) & is.na(x$OT)] <- 0
x$scorediff <- x$teamscore - x$oppscore

# Get Ivy Games
ivy <- c("Princeton", "Yale", "Harvard", "Penn", "Brown",  "Cornell",   
         "Dartmouth","Columbia")
x$ivy <- 0
for(i in 1:nrow(x)){
  x$ivy[i] <- is.element(x$team[i], ivy) +  is.element(x$opponent[i], ivy)
}


# Create Model
lm.lax<- lm(scorediff ~ team + opponent + location, weights = weights, data = x)
summary(lm.lax)

# Point Spread to Win Percentage Model
x$winprob <- NA
x$winprob[x$scorediff > 0] <- 1
x$winprob[x$scorediff < 0] <- 0

x$predscorediff <- round(predict(lm.lax, newdata = x), 1)


glm.pointspread <- glm(winprob ~ predscorediff, family = "binomial", data = x)
summary(glm.pointspread)

x$winprob[is.na(x$winprob)] <- 
  round(predict.glm(glm.pointspread, newdata = x[is.na(x$winprob),], type = "response"), 3)


# All Ivy Games
all_ivy_games <- x[x$ivy == 2 & x$year == 2018, ]


# power rankings
teams <- unique(x$team)
powerrankings <- data.frame(Team = rep(NA, length(teams)),
                            YUSAG_Coefficient = rep(NA, length(teams)))
powerrankings[1, ] <- c(teams[1], 0)

for(i in 2:(length(teams))) {
  teamcoef <- 
    lm.lax$coefficients[paste("team", teams[i], sep = "")]
  opponentcoef <- 
    lm.lax$coefficients[paste("opponent", teams[i], sep = "")]
  tmp <- c(teams[i], round((teamcoef - opponentcoef)/2, 1))
  powerrankings[i, ] <- tmp
}
powerrankings$YUSAG_Coefficient <- as.numeric(powerrankings$YUSAG_Coefficient)
powerrankings <- powerrankings[order(powerrankings$YUSAG_Coefficient, decreasing = T), ]
powerrankings$rank <- seq(1, 116, 1)
###Scale
powerrankings$YUSAG_Coefficient  <- powerrankings$YUSAG_Coefficient - mean(powerrankings$YUSAG_Coefficient)

IVY <- data.frame(Team = ivy,
                  YUSAG_Coefficient = rep(NA, 8),
                  Expected_Wins = rep(NA, 8),
                  Expected_Losses = rep(NA, 8))
for(i in 1:8){
  IVY$YUSAG_Coefficient[IVY$Team == ivy[i]] <- 
    as.numeric(powerrankings$YUSAG_Coefficient[powerrankings$Team == ivy[i]])
  IVY$Expected_Wins[i] <- round(sum(all_ivy_games$winprob[all_ivy_games$team == ivy[i]]), 1)
  IVY$Expected_Losses[i] <- 7 - round(sum(all_ivy_games$winprob[all_ivy_games$team == ivy[i]]), 1)
}

# write results
write.table(powerrankings, "WLAX_Powerrankings.csv", row.names = F, col.names = T, sep = ",")
write.table(IVY, "WLAX_Ivy_League_Predicted_Results.csv", row.names = F, col.name = T, sep = ",")
write.table(all_ivy_games, "WLAX_Ivy_Games.csv", row.names = F, col.names = T, sep = ",")


# Playoffs
games <- all_ivy_games[all_ivy_games$location == "H", ]
games$wins <- games$winprob
powranks <- powerrankings


############################# Sims (Ivy Tourney) ########################################
### Simulates Ivy League Tournament
champ.sim <- function(teams) {
  tmp <- data.frame("team" = c(teams[1:2], NA),
                    "opponent" = c(teams[4:3], NA), 
                    stringsAsFactors = F)
  tmp$location <- "N"
  
  
  tmp$predscorediff <- predict(lm.lax, newdata = tmp)
  tmp$winprob <- predict(glm.pointspread, newdata = tmp, type = "response")
  
  ### Sim Semifinals
  rands <- runif(2)
  tmp[3, c("team", "opponent")] <- ifelse(rands <= tmp$winprob[1:2], tmp$team, tmp$opponent)
  tmp$location[3] <- "N"
  
  tmp$predscorediff <- predict(lm.lax, newdata = tmp)
  tmp$winprob <- predict(glm.pointspread, newdata = tmp, type = "response")
  
  # Final
  rand <- runif(1)
  champ <- ifelse(rand <= tmp$winprob[3], tmp$team[3], tmp$opponent[3])
  
  return(champ)
}

################################### IVY SIMS ##################################
### Simulates Ivy League Regular Season
ivy.sim <- function(nsims, games) {
  champ <- rep(NA, nsims)
  
  # Data Frame to Hold Team Wins by Sim
  simresults <- as.data.frame(matrix(nrow = nsims, ncol = length(ivy), byrow = T))
  names(simresults) <- ivy
  
  # Stores pre (and later post) tie-break position in standings
  prebreak.pos <- as.data.frame(matrix(nrow = nsims, ncol = length(ivy), byrow = T))
  names(prebreak.pos) <- ivy
  
  # Simulate All Games
  for (j in 1:nrow(simresults)){
    print(paste("Sim: ", j, sep = ""))
    games$simwins <- NA
    games$oppsimwins <- NA
    rand <- runif(nrow(games))
    games$simwins[games$wins == 1] <- 1
    games$oppsimwins[games$wins == 1] <- 0
    games$simwins[games$wins == 0] <- 0
    games$oppsimwins[games$wins == 0] <- 1
    sims <- games$wins > 0 & games$wins < 1
    games$simwins[sims] <- ifelse(rand[sims] <= games$wins[sims], 1, 0)
    games$oppsimwins[sims] <- ifelse(rand[sims] > games$wins[sims], 1, 0)
    
    # get team win totals for current sim
    for(i in 1:length(ivy)) {
      simresults[j, i] <- (sum(games$simwins[games$team == ivy[i]]) + 
                             sum(games$oppsimwins[games$opponent == ivy[i]]))
    }
    
    # H2H records (Row Team Wins Over Column Team)
    head2head <- matrix(nrow = length(ivy), ncol = length(ivy))
    colnames(head2head) <- ivy
    rownames(head2head) <- ivy
    for(i in 1:length(ivy)) {
      for(k in 1:length(ivy)) {
        head2head[i,k] <- sum(games$simwins[games$team == ivy[i] & games$opponent == ivy[k]]) +
          sum(games$oppsimwins[games$team == ivy[k] & games$opponent == ivy[i]])
      }
    }
    
    # Get order of finish Pre-Tiebreak
    preBreak <- sort(as.vector(simresults[j,], mode = "numeric"), decreasing = T)
    
    for(z in 1:length(ivy)) {
      prebreak.pos[j,z] <- c(1:length(ivy))[preBreak == simresults[j, z]][1]
    }
    
    # Break any ties 
    for(i in 1:(length(ivy) - 1)) {
      if(sum(prebreak.pos[j,] == i) > 1){
        # Get teams to between which to break tie
        teams <- ivy[prebreak.pos[j,] == i]
        tie <- length(teams)
        teamIDs <- c(1:length(ivy))[is.element(ivy, teams)]
        
        # Tiebreak 1 (H2H)
        h2h <- rep(0, length(teams))
        for(k in 1:length(teams)) {
          h2h[k] <- sum(head2head[teams[k], teams[-k]])
        }
        if(sum(h2h == max(h2h)) == 1) {
          winner <- teams[grep(max(h2h), h2h)]
          winnerID <- teamIDs[grep(max(h2h), h2h)]
          # Winner wins tie-break
          simresults[j, winnerID] <- simresults[j, winnerID] + 0.1 * tie
          # Change current standing of losers
          change <- teams[teams != winner]
          prebreak.pos[j, change] <- i + 1
          next
        }
        else if(sum(h2h == max(h2h)) > 1 & sum(h2h == max(h2h)) < length(teams)){
          change <- setdiff(teams, teams[grep(max(h2h), h2h)])
          teams <- teams[grep(max(h2h), h2h)]
          prebreak.pos[j, change] <- i + 1
        }
        
        # Tiebreak 2 (Record vs. 1-8, descending order)
        for(z in 1:length(ivy)) {
          if(z == i) {
            next
          }
          comp_teams <- ivy[prebreak.pos[j,] == z]
          if(length(comp_teams) == 0) {
            next
          }
          comp_teamsIDs <- c(1:length(ivy))[is.element(ivy, comp_teams)]
          
          h2h <- rep(0, length(teams))
          for(k in 1:length(teams)) {
            h2h[k] <- sum(head2head[teams[k], comp_teams])
          }
          
          if(sum(h2h == max(h2h)) == 1) {
            winner <- teams[grep(max(h2h), h2h)]
            winnerID <- teamIDs[grep(max(h2h), h2h)]
            # Winner wins tie-break
            simresults[j, winnerID] <- simresults[j, winnerID] + 0.1 * tie
            # Change current standing of losers
            change <- teams[teams != winner]
            prebreak.pos[j, change] <- i + 1
            break
          }
          else if(sum(h2h == max(h2h)) > 1 & sum(h2h == max(h2h)) < length(teams)){
            change <- setdiff(teams, teams[grep(max(h2h), h2h)])
            teams <- teams[grep(max(h2h), h2h)]
            prebreak.pos[j, change] <- i + 1
          }
        }
        if(z < length(ivy)){
          next
        }
        
        # Tiebreak 3 (Analytics)
        tmp <- powranks[is.element(powranks$Team, teams),]
        tmp <- tmp[order(tmp$Team),]
        winner <- tmp$Team[grep(max(tmp$YUSAG_Coefficient), tmp$YUSAG_Coefficient)]
        winnerID <- c(1:length(ivy))[ivy == winner]
        # Winner wins tie-break
        simresults[j, winnerID] <- simresults[j, winnerID] + 0.1 * tie
        # Change current standing of losers
        change <- teams[teams != winner]
        prebreak.pos[j, change] <- i + 1
      }
    }
    
    # Sim Ivy tournament
    tourney <- c(ivy[as.vector(prebreak.pos[j,] == 1)], ivy[as.vector(prebreak.pos[j,] == 2)],
                 ivy[as.vector(prebreak.pos[j,] == 3)], ivy[as.vector(prebreak.pos[j,] == 4)])
    champ[j] <- champ.sim(tourney)
  }
  
  ### store playoff odds
  playoffs <- data.frame(Team = ivy,
                         champ = rep(NA, length(ivy)),
                         playoff_prob = rep(NA, length(ivy)),
                         seed1_prob = rep(NA, length(ivy)),
                         seed2_prob = rep(NA, length(ivy)),
                         seed3_prob = rep(NA, length(ivy)),
                         seed4_prob = rep(NA, length(ivy)))
  
  
  for(i in 1:length(ivy)) {
    playoffs$champ[i] <- round(sum(is.element(champ, ivy[i]))/nsims * 100, 1)
    playoffs$seed1_prob[i] <- round(sum(prebreak.pos[,i] == 1)/nsims * 100, 1)
    playoffs$seed2_prob[i] <- round(sum(prebreak.pos[,i] == 2)/nsims * 100, 1)
    playoffs$seed3_prob[i] <- round(sum(prebreak.pos[,i] == 3)/nsims * 100, 1)
    playoffs$seed4_prob[i] <- round(sum(prebreak.pos[,i] == 4)/nsims * 100, 1)
    playoffs$playoff_prob[i] <- sum(playoffs[i,4:7], na.rm = T)
  }
  write.table(playoffs, "WLAX_playoffs.csv", row.names = F, col.names = T, sep = ",")
  return(playoffs)
}

suppressWarnings(ivy.sim(10000, games))

