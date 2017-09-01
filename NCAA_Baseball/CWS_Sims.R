x <- read.csv("scores.csv", as.is = T)
y <- read.csv("results.csv", as.is = T)
x$rundiff <- x$teamscore - x$oppscore

# Create Model
lm.bb <- lm(rundiff ~ team + opponent + location, data = x)

# Point Spread to Win Percentage Model
x$winprob <- NA
x$winprob[x$rundiff > 0] <- 1
x$winprob[x$rundiff < 0] <- 0

x$predrundiff <- as.numeric(round(predict(lm.bb, newdata = x), 1))

glm.bb<- glm(winprob ~ predrundiff, data = x, family=binomial(link=logit))

x$winprob[is.na(x$winprob)] <- predict(glm.bb, newdata = x[is.na(x$winprob),], type = "response")

teams <- unique(x$team)
pr <- data.frame("Team" = teams,
                 "Power_Rating" = rep(NA, length(teams)))

for(i in 1:length(teams)){
  pr$Power_Rating[i] <- lm.bb$coefficients[paste("team", teams[i], sep = "")]
}
write.table(pr, "pr.csv", row.names = F, col.names = T, sep = ",")



bb.sim <- function(bracket, G1W, G2W, G3W, G4W, G5W, G6W){
  Games<- x[1:7, c("team", "opponent", "location")]
  Games[1:7,] <- NA
  Games$winprob <- 0
  Games$predrundiff <- 0
  Games$location <- "N"
  data <- y[y$bracket == bracket, ]
  
  
  ### Game 1
  Games[1, c("team", "opponent")] <- c(data$team[1], data$team[4])
  Games$predrundiff[1] <- as.numeric(predict(lm.bb, newdata = Games[1,]))
  Games$winprob[1] <- predict(glm.bb, newdata = Games[1,], type = "response")
  if(G1W == F) {
    rand <- runif(1)
    if(rand < Games$winprob[1]){
      Games$team[3] <- Games$opponent[1]
      Games$team[4] <- Games$team[1]
    }
    else{
      Games$team[4] <- Games$opponent[1]
      Games$team[3] <- Games$team[1]
    }
  }
  else{
    win <- G1W
    lose <- c(Games$team[1], Games$opponent[1])
    lose <- lose[lose != G1W]
    Games$team[4] <- win
    Games$team[3] <- lose
  }
  
  ### Game 2
  Games[2, c("team", "opponent")] <- c(data$team[2], data$team[3])
  Games$predrundiff[2] <- as.numeric(predict(lm.bb, newdata = Games[2,]))
  Games$winprob[2] <- predict(glm.bb, newdata = Games[2,], type = "response")
  rand <- runif(1)
  if(G2W == F){
    if(rand < Games$winprob[2]){
      Games$opponent[3] <- Games$opponent[2]
      Games$opponent[4] <- Games$team[2]
    }
    else{
      Games$opponent[4] <- Games$opponent[2]
      Games$opponent[3] <- Games$team[2]
    }
  }
  else{
    win <- G2W
    lose <- c(Games$team[2], Games$opponent[2])
    lose <- lose[lose != G2W]
    Games$opponent[4] <- win
    Games$opponent[3] <- lose
  }
  
  ### Game 3 
  Games$predrundiff[3] <- as.numeric(predict(lm.bb, newdata = Games[3,]))
  Games$winprob[3] <- predict(glm.bb, newdata = Games[3,], type = "response")
  rand <- runif(1)
  if(G3W == F){
    if(rand < Games$winprob[3]){
      Games$opponent[5] <- Games$team[3]
    }
    else{
      Games$opponent[5] <- Games$opponent[3]
    }
  }
  else{
    Games$opponent[5] <- G3W
  }
  
  
  ### Game 4
  Games$predrundiff[4] <- as.numeric(predict(lm.bb, newdata = Games[4,]))
  Games$winprob[4] <- predict(glm.bb, newdata = Games[4,], type = "response")
  rand <- runif(1)
  if(G4W == F) {
    if(rand < Games$winprob[4]){
      Games$team[5] <- Games$opponent[4]
      Games$team[6] <- Games$team[4]
      
    }
    else{
      Games$team[5] <- Games$team[4]
      Games$team[6] <- Games$opponent[4]
    }
  }
  else{
    win <- G4W
    lose <- c(Games$team[4], Games$opponent[4])
    lose <- lose[lose != G4W]
    Games$team[6] <- win
    Games$team[5] <- lose
  }
  
  
  ### Game 5
  Games$predrundiff[5] <- as.numeric(predict(lm.bb, newdata = Games[5,]))
  Games$winprob[5] <- predict(glm.bb, newdata = Games[5,], type = "response")
  rand <- runif(1)
  if(G5W == F){
    if(rand < Games$winprob[5]){
      Games$opponent[6] <- Games$team[5]
    }
    else{
      Games$opponent[6] <- Games$opponent[5]
    }
  }
  else{
    Games$opponent[6] <- G5W
  }
  ### Game 6
  Games$predrundiff[6] <- as.numeric(predict(lm.bb, newdata = Games[6,]))
  Games$winprob[6] <- predict(glm.bb, newdata = Games[6,], type = "response")
  rand <- runif(1)
  if(G6W == F) {
    if(rand < Games$winprob[6]) {
      winner <- Games$team[6]
    }
    else{
      Games[7,] <- Games[6,]
      rand <- runif(1)
      if(rand < Games$winprob[7]) {
        winner <- Games$team[7]
      }
      else{
        winner <- Games$opponent[7]
      }
    }
  }
  else{
    Games[7,] <- Games[6,]
    rand <- runif(1)
    if(rand < Games$winprob[7]) {
      winner <- Games$team[7]
    }
    else{
      winner <- Games$opponent[7]
    }
  }
  return(winner)
}


finals.sim <- function(team1, team2, G1W, G2W){
  Games<- x[1:3, c("team", "opponent")]
  Games[1:3,] <- NA
  Games$winprob <- 0
  Games$predrundiff <- 0
  Games$location <- "N"
  
  ### game 1
  Games[1, c("team", "opponent")] <- c(winner1, winner2)
  Games$predrundiff[1] <- as.numeric(predict(lm.bb, newdata = Games[1,]))
  Games$winprob[1] <- predict(glm.bb, newdata = Games[1,], type = "response")
  rand <- runif(1)
  if(G1W == F) {
    if(rand < Games$winprob[1]){
      Games$team[2] <- winner1
      Games$opponent[2] <- winner2
    }else{
      Games$team[2] <- winner2
      Games$opponent[2] <- winner1
    }
  }else{
    Games$team[2] <- G1W
    loser <- c(Games$team[1], Games$opponent[1])
    Games$opponent[2] <- loser[loser != G1W]
  }
  ### game 2
  Games$predrundiff[2] <- as.numeric(predict(lm.bb, newdata = Games[2,]))
  Games$winprob[2] <- predict(glm.bb, newdata = Games[2,], type = "response")
  
  if(G2W == F) {
    rand <- runif(1)
    if(rand < Games$winprob[2]){
      winner <- Games$team[2]
    }else{
      rand <- runif(1)
      if(rand < Games$winprob[2]){
        winner <- Games$team[2]
      }else{
        winner <- Games$opponent[2]
      }
    }
  }
  else{
    rand <- runif(1)
    if(rand < Games$winprob[2]){
      winner <- Games$team[2]
    }else{
      winner <- Games$opponent[2]
    }
  }
  return(winner)
}

CWS_odds <- data.frame(team = y$team,
                       finals = rep(0,8),
                       champ = rep(0,8))


sims <- 2500
for(i in 1:sims){
  print(i)
  
  winner1 <- "LSU"
  winner2 <- "Florida"
  champ <- finals.sim(winner1, winner2, "Florida", F)
  
  CWS_odds$finals[CWS_odds$team == winner1] <- CWS_odds$finals[CWS_odds$team == winner1]  + 1
  CWS_odds$finals[CWS_odds$team == winner2] <- CWS_odds$finals[CWS_odds$team == winner2]  + 1
  CWS_odds$champ[CWS_odds$team == champ] <- CWS_odds$champ[CWS_odds$team == champ]  + 1
  
}


CWS_odds$finals <- round(100 * CWS_odds$finals/sims, 1)
CWS_odds$champ <- round(100 * CWS_odds$champ/sims, 1)
write.table(CWS_odds, "CWS_odds.csv", row.names = F, col.names = T, sep = ",")

CWS_odds <- read.csv("CWS_odds.csv", as.is = T)

colors <- c("grey", "blue", "firebrick", "red", "goldenrod1", "darkorange1", "purple", "brown4")

library(ggplot2)



CWS_odds$fraction <- CWS_odds$champ/sum(CWS_odds$champ)
CWS_odds <- CWS_odds[order(CWS_odds$champ), ]
CWS_odds$ymax <- cumsum(CWS_odds$fraction)
CWS_odds$ymin <-  c(0, head(CWS_odds$ymax, n=-1))


p2 <- ggplot(CWS_odds, aes(fill=team, ymax=ymax, ymin=ymin, xmax=4, xmin=3), color = colors) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme(panel.grid=element_blank()) +
  theme(axis.text.y=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(axis.title=element_blank()) +
  annotate("text", x = 1.5, y = 0, label = "Odds to Win \nCWS", fontface = 4, size = 10) +
  annotate("text", x = 0.5, y = 0, 
           label = paste("LSU: ", CWS_odds$champ[CWS_odds$team == "LSU"], "%", sep = "") , fontface = 2, size = 6, color = "Purple")+
  annotate("text", x = 0, y = 0, 
           label = paste("Florida: ", CWS_odds$champ[CWS_odds$team == "Florida"], "%", sep = "") , fontface = 2, size = 6, color = "darkorange2")+
  labs(title="") + scale_fill_manual(values=colors)

p2

CWS_odds$type <- "Champion"

p4 <- ggplot(color = colors) + geom_bar(aes(y = champ, x = type, fill = team), data = CWS_odds,
                          stat="identity", )
p4















