x <- read.csv("scores.csv", as.is = T)
y <- read.csv("tournament.csv", as.is = T)

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

bb.sim <- function(a, G3W, G4L, G4W, G5W){
  sims <- 1000
  Games<- x[1:7, c("team", "opponent", "location")]
  Games[1:7,] <- NA
  Games$winprob <- 0
  Games$predrundiff <- 0
  
  totals <- rep(0, 4)
  data <- y[y$Regional == a, ]
  home <- data$Team[1]
  
  for(i in 1:sims) {
    print(i)
    ### Game 1
#     Games[1, c("team", "opponent", "location")] <- c(data$Team[1], data$Team[4], "H")
#     Games$predrundiff[1] <- as.numeric(predict(lm.bb, newdata = Games[1,]))
#     Games$winprob[1] <- predict(glm.bb, newdata = Games[1,], type = "response")
#     rand <- runif(1)
#     
#     if(rand < Games$winprob[1]){
#       Games$team[3] <- G1L
#       Games$location[3] <- L3
#       Games$team[4] <- G1W
#       Games$location[4] <- L4
#     }else{
#       Games$team[4] <- G1W
#       Games$location[4] <- L4
#       Games$team[3] <- G1L
#       Games$location[3] <- L3
#     }
#     
#     ### Game 2
#     Games[2, c("team", "opponent", "location")] <- c(data$Team[2], data$Team[3], "N")
#     Games$predrundiff[2] <- as.numeric(predict(lm.bb, newdata = Games[2,]))
#     Games$winprob[2] <- predict(glm.bb, newdata = Games[2,], type = "response")
#     rand <- runif(1)
#     
#     if(rand < Games$winprob[2]){
#       Games$opponent[3] <- G2L
#       Games$opponent[4] <- G2W
#     }else{
#       Games$opponent[4] <- G2W
#       Games$opponent[3] <- G2L
#     }
#     
#     ### Game 3 and 4
#     Games$predrundiff[3] <- as.numeric(predict(lm.bb, newdata = Games[3,]))
#     Games$winprob[3] <- predict(glm.bb, newdata = Games[3,], type = "response")
#     Games$predrundiff[4] <- as.numeric(predict(lm.bb, newdata = Games[4,]))
#     Games$winprob[4] <- predict(glm.bb, newdata = Games[4,], type = "response")
#     
#     rand <- runif(1)
#     if(rand < Games$winprob[3]){
#       Games$opponent[5] <- Games$team[3]
#       
#     }else{
#       Games$opponent[5] <- Games$opponent[4]
#     }
#     
#     
#     rand <- runif(1)
#     if(rand < Games$winprob[4]){
#       Games$team[5] <- Games$opponent[4]
#       Games$team[6] <- Games$team[4]
#       
#     }else{
#       Games$team[5] <- Games$team[4]
#       Games$team[6] <- Games$opponent[4]
#     }
    
    Games$team[5] <- G4L
    Games$team[6] <- G4W
    Games$opponent[5] <- G3W
    
    ### Game 5
    if(Games$opponent[5] == home){
      Games$location[5] <- "V"
    }else if (Games$team[5] == home){
      Games$location[5] <- "H"
    }else{
      Games$location[5] <- "N"
    }
    
    Games$predrundiff[5] <- as.numeric(predict(lm.bb, newdata = Games[5,]))
    Games$winprob[5] <- predict(glm.bb, newdata = Games[5,], type = "response")
    
    rand <- runif(1)
    if(rand < Games$winprob[5]){
      Games$opponent[6] <- G5W
    }else{
      Games$opponent[6] <- G5W
    }
    
    ### Game 6
    
    if(Games$opponent[6] == home){
      Games$location[6] <- "V"
    }else if (Games$team[6] == home){
      Games$location[6] <- "H"
    }else{
      Games$location[6] <- "N"
    }
    
    Games$predrundiff[6] <- as.numeric(predict(lm.bb, newdata = Games[6,]))
    Games$winprob[6] <- predict(glm.bb, newdata = Games[6,], type = "response")
    
    rand <- runif(1)
    if(rand < Games$winprob[6]) {
      seed <- data$Seed[data$Team == Games$team[6]]
      totals[seed] <- totals[seed] + 1
      
    }else{
      Games[7,] <- Games[6,]
      rand <- runif(1)
      if(rand < Games$winprob[7]) {
        seed <- data$Seed[data$Team == Games$team[7]]
        totals[seed] <- totals[seed] + 1
      }else{
        seed <- data$Seed[data$Team == Games$opponent[7]]
        totals[seed] <- totals[seed] + 1
      }
    }
  }
  return(round(100 * totals/sims, 1))
}


bb.sim("Fort Worth", "TCU", "Central Conn. St.", "N", "Virginia", "DBU", "H")

bb.sim("Corvallis", "Holy Cross", "Yale", "Oregon St.", "Yale")
bb.sim("Clemson", "UNCG", "Clemson", "Vanderbilt")
bb.sim("Chapel Hill", "North Carolina", "FGCU", "Davidson")
bb.sim("Baton Rouge", "Rice", "Southeastern La.", "LSU")
bb.sim("Fayetteville\xe6", "Oral Roberts", "Arkansas", "Missouri St.")
bb.sim("Hattiesburg", "Southern Miss.", "Ill.-Chicago", "N", "South Ala.", "Mississippi St.", "H")
bb.sim("Gainesville", "South Fla.", "Bethune-Cookman", "Florida")
bb.sim("Houston", "Houston", "Iowa", "Texas A&M")
bb.sim("Lexington", "Indiana", "Kentucky", "North Carolina St.")
bb.sim("Long Beach", "San Diego St.", "Long Beach St.", "Texas")
bb.sim("Lubbock", "Arizona",  "Sam Houston St.", "Texas Tech")
bb.sim("Tallahassee", "Florida St.", "Tennessee Tech", "Auburn")
bb.sim("Winston-Salem","Maryland", "West Virginia",  "Wake Forest")
bb.sim("Louisville", "Xavier", "Oklahoma", "Louisville")














z <- read.csv("corvallis.csv", as.is = T)
plot(OSU ~ Game, data = z, col = "Orange", type = "l",  lwd = 3,
     xaxt='n', ylim = c(0,100), main = "Corvallis, OR NCAA Baseball Regional", 
     ylab = "Advance to Super Regional (%)")
par(new = T)
plot(Nebraska ~ Game, data = z, col = "Red", type = "l",  lwd = 3,
     xaxt='n', ylim = c(0,100), main = "Corvallis, OR NCAA Baseball Regional", 
     ylab = "Advance to Super Regional (%)")
par(new = T)
plot(Yale ~ Game, data = z, col = "Blue", type = "l",  lwd = 3,
     xaxt='n', ylim = c(0,100), main = "Corvallis, OR NCAA Baseball Regional", 
     ylab = "Advance to Super Regional (%)")
par(new = T)
plot(HC ~ Game, data = z, col = "Purple", type = "l",  lwd = 3,
     xaxt='n', ylim = c(0,100), main = "Corvallis, OR NCAA Baseball Regional", 
     ylab = "Advance to Super Regional (%)")

LAB = c("1", "2", "3", "4", "5","6")
pos = 1:6
axis(1, labels = LAB, at = pos)

labels<-c("Oregon St.", "Nebraska", "Yale", "Holy Cross")
colors <-c("Orange", "red", "blue", "purple")


legend("topleft", xjust = 0, cex = .4, title="Legend",
       labels, lwd=2, lty=c(1, 1, 1, 1, 1), col=colors)
