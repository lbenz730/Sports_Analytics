#Get Data
library("XML")
ivy.teams <- c("Brown", "Cornell", "Columbia", "Dartmouth", "Harvard", "Penn",
               "Princeton", "Yale")
BSB <- F
if(BSB){
  word <- "Baseball"
  code <- "bsb"
  innings <- 8
}else{
  word <- "Softball"
  code <- "sball"
  innings <- 7
}

for(j in 1:length(ivy.teams)){
  print(paste("Getting: ", ivy.teams[j], sep = ""))
  url <- paste("http://www.ivyleague.com/sports/", code, "/2016-17/stats/enhanced/teams/", 
               ivy.teams[j], "?view=lineup&r=0&pos=", sep = "")
  x <- readHTMLTable(url)[c(5,7,9,11)]
  
  team.hitting <- as.data.frame(cbind(x[[1]], x[[2]]))
  team.hitting <- team.hitting[-c((nrow(team.hitting) -1), nrow(team.hitting)), -c(20:24)]
  for(i in 1:ncol(team.hitting)) {
    if(i > 4) {
      team.hitting[,i] <- as.numeric(gsub("-", 0, team.hitting[,i]))
    }
    else{
      team.hitting[,i] <- gsub("-", 0, team.hitting[,i])
    }
  }
  team.hitting$team <- ivy.teams[j]
  
  team.pitching <- as.data.frame(x[[3]])
  team.pitching <- team.pitching[-c((nrow(team.pitching) -1), nrow(team.pitching)),]
  for(i in 1:ncol(team.pitching)) {
    if(i > 4) {
      team.pitching[,i] <- as.numeric(gsub("-", 0, team.pitching[,i]))
    }
    else{
      team.pitching[,i] <- gsub("-", 0, team.pitching[,i])
    }
  }
  team.pitching$team <- ivy.teams[j]
  
  team.fielding <- as.data.frame(x[[4]])
  team.fielding <- team.fielding[-c((nrow(team.fielding) -1), nrow(team.fielding)),]
  for(i in 1:ncol(team.fielding)) {
    if(i > 4) {
      team.fielding[,i] <- as.numeric(gsub("-", 0, team.fielding[,i]))
    }
    else{
      team.fielding[,i] <- gsub("-", 0, team.fielding[,i])
    }
  }
  team.fielding$team <- ivy.teams[j]
  
  if(j == 1) {
    ivy.hitting <- team.hitting
    ivy.pitching <- team.pitching
    ivy.fielding <- team.fielding
  }
  else{
    ivy.hitting <- rbind(ivy.hitting, team.hitting)
    ivy.pitching <- rbind(ivy.pitching, team.pitching)
    ivy.fielding <- rbind(ivy.fielding, team.fielding)
  }
}



names(ivy.hitting)[names(ivy.hitting) == "2b"] <- "h.2b"
names(ivy.hitting)[names(ivy.hitting) == "3b"] <- "h.3b"
ivy.hitting$h.1b <- ivy.hitting$h - (ivy.hitting$h.2b + ivy.hitting$h.3b + ivy.hitting$hr)

# Batting WAR
# Hitting
ivy.hitting$wOBA <-
  (0.69*ivy.hitting$bb + 0.72*ivy.hitting$hbp + 0.89*ivy.hitting$h.1b + 1.27*ivy.hitting$h.3b 
   + 2.10*ivy.hitting$hr)/
  (ivy.hitting$ab + ivy.hitting$bb + ivy.hitting$hbp + ivy.hitting$sf)
lwOBA <- mean(ivy.hitting$wOBA, na.rm = T)

ivy.hitting$wRAA <- (ivy.hitting$wOBA-lwOBA)/1.277*ivy.hitting$pa

# Baserunning
games <- 0
for(j in 1:8){
  games <- games + max(ivy.hitting$r[ivy.hitting$team == ivy.teams[1]], na.rm = T)
}

runCS <- -(2*sum(ivy.hitting$r, na.rm = T)/(innings * 6 * games) + 0.075)
lwSB <- (sum(ivy.hitting$sb, na.rm = T)*0.2 + sum(ivy.hitting$cs, na.rm = T)*runCS)/
  (sum(ivy.hitting$bb, na.rm = T) + sum(ivy.hitting$hpb, na.rm = T) + sum(ivy.hitting$h.1b, na.rm = T))

ivy.hitting$wSB <- ivy.hitting$sb*0.2 + ivy.hitting$cs*runCS - 
  lwSB*(ivy.hitting$bb + ivy.hitting$hbp + ivy.hitting$h.1b)

runs.per.out <- sum(ivy.hitting$r, na.rm = T)/(innings * 6 * games)
ivy.hitting$wGDP <- -(ivy.hitting$hdp/ivy.hitting$ab * runs.per.out)
ivy.hitting$br.runs <- ivy.hitting$wGDP + ivy.hitting$wSB
RPW <- sum(ivy.hitting$r, na.rm = T)/(games)*1.5 + 3
ivy.hitting$rep.runs <- 57 * RPW*ivy.hitting$pa/sum(ivy.hitting$pa, na.rm = T)
ivy.hitting$WAR <- (ivy.hitting$wRAA + ivy.hitting$br.runs + ivy.hitting$rep.runs)/RPW



#####  Pitching
for(i in 1:nrow(ivy.pitching)) {
  if(substring(ivy.pitching$ip[i], nchar(ivy.pitching$ip[i]) - 1, nchar(ivy.pitching$ip[i])) == ".1") {
    ivy.pitching$ip[i] <- as.numeric(gsub("1$", "3333", ivy.pitching$ip[i]))
  }
  
  else if(substring(ivy.pitching$ip[i], nchar(ivy.pitching$ip[i]) - 1, nchar(ivy.pitching$ip[i])) == ".2") {
    ivy.pitching$ip[i] <- as.numeric(gsub("2$", "6666", ivy.pitching$ip[i]))
  }
}

lgERA <- sum(ivy.pitching$er, na.rm = T)/sum(ivy.pitching$ip, na.rm = T)*8
FIP_Constant <- lgERA - (13*sum(ivy.pitching$hr, na.rm = T) + 3*sum(ivy.pitching$bb, na.rm = T) - 
                           2*sum(ivy.pitching$k, na.rm = T))/sum(ivy.pitching$ip, na.rm = T)
ivy.pitching$FIP <- (13*ivy.pitching$hr + 3*ivy.pitching$bb - 2*ivy.pitching$k)/ivy.pitching$ip +
  FIP_Constant

lgRA <- sum(ivy.pitching$r, na.rm = T)/sum(ivy.pitching$ip, na.rm = T)*innings
ivy.pitching$FIPR <- ivy.pitching$FIP + lgRA-lgERA

if(BSB){
  park.factors <- read.csv("Park_Factors.csv", as.is = T)
  park.factors$PF <- park.factors$PF/100
  for(i in 1:nrow(ivy.pitching)){
    ivy.pitching$FIPR[i] <- ivy.pitching$FIPR[i]/park.factors$PF[park.factors$Team == ivy.pitching$team[i]]
    }
}

ivy.pitching$ipg <- ivy.pitching$ip/ivy.pitching$app
ivy.pitching$dPRW <- (((2*innings - ivy.pitching$ipg)*mean(ivy.pitching$FIPR, na.rm = T) + 
                        ivy.pitching$ipg*ivy.pitching$FIPR)/18 + 2)*1.5
ivy.pitching$RAAP <- mean(ivy.pitching$FIPR, na.rm = T) - ivy.pitching$FIPR
ivy.pitching$WPGAA <- ivy.pitching$RAAP/ivy.pitching$dPRW
ivy.pitching$replacement.level <- 0.03*(1 - ivy.pitching$gs/ivy.pitching$app) + 
  0.12*(ivy.pitching$gs/ivy.pitching$app)
ivy.pitching$WPGAR <- ivy.pitching$WPGAA + ivy.pitching$replacement.level
ivy.pitching$WAR <- ivy.pitching$WPGAR * ivy.pitching$ip/innings

x <- ivy.hitting[,c(2:3, 30, 4, 38, 32:37, 5:8, 31, 9:29)]
y <- ivy.pitching[,c(2, 20, 3:4, 29, 21:22, 24:28, 5:19, 23)]


write.table(x, paste("Ivy_Hitting",word, "War.csv", sep = "_"), 
            row.names = F, col.names = T, sep = ",")
write.table(y, paste("Ivy_Pitching",word, "War.csv", sep = "_"), 
            row.names = F, col.names = T, sep = ",")
                        




