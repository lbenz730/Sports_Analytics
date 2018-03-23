##################################### Softball ################################
ivy_hitting <- read.csv("ivy_softball_hitting_2018.csv", as.is = T)
ivy_pitching <- read.csv("ivy_softball_pitching_2018.csv", as.is = T)
ivy_games <- c(14, 17, 13, 16, 18, 15, 16, 18)
names(ivy_games) <- c("Brown", "Columbia", "Cornell", "Dartmouth", "Harvard",
                      "Penn", "Princeton", "Yale")
############ Batting WAR ############
# Hitting
ivy_hitting$H1B <- ivy_hitting$H - (ivy_hitting$H2B + ivy_hitting$H3B + ivy_hitting$HR)
ivy_hitting$PA <- (ivy_hitting$AB + ivy_hitting$BB + ivy_hitting$HBP + ivy_hitting$SF)
ivy_hitting$wOBA <-
  (0.69*ivy_hitting$BB + 0.72*ivy_hitting$HBP + 0.89*ivy_hitting$H1B + 1.27*ivy_hitting$H3B 
   + 2.10*ivy_hitting$HR)/ivy_hitting$PA

lwOBA <- mean(ivy_hitting$wOBA, na.rm = T)
ivy_hitting$wRAA <- (ivy_hitting$wOBA-lwOBA)/1.277*ivy_hitting$PA

# Baserunning
games <- sum(ivy_games)
ivy_hitting$CS <- ivy_hitting$SbATT - ivy_hitting$SB
runCS <- -(2*sum(ivy_hitting$R, na.rm = T)/(7 * 6 * games) + 0.075)
lwSB <- (sum(ivy_hitting$SB, na.rm = T)*0.2 + sum(ivy_hitting$CS, na.rm = T)*runCS)/
  (sum(ivy_hitting$BB, na.rm = T) + sum(ivy_hitting$HPB, na.rm = T) + sum(ivy_hitting$H1B, na.rm = T))

ivy_hitting$wSB <- ivy_hitting$SB*0.2 + ivy_hitting$CS*runCS - 
  lwSB*(ivy_hitting$BB + ivy_hitting$HBP + ivy_hitting$H1B)

runs.per.out <- sum(ivy_hitting$R, na.rm = T)/(7 * 6 * games)
ivy_hitting$wGDP <- -(ivy_hitting$GDP/ivy_hitting$AB * runs.per.out)
ivy_hitting$br.runs <- ivy_hitting$wGDP + ivy_hitting$wSB
RPW <- sum(ivy_hitting$R, na.rm = T)/(games)*1.5 + 3
ivy_hitting$rep.runs <- 57 * RPW*ivy_hitting$PA/sum(ivy_hitting$PA, na.rm = T)
ivy_hitting$WAR <- (ivy_hitting$wRAA + ivy_hitting$br.runs + ivy_hitting$rep.runs)/RPW
ivy_hitting <- ivy_hitting[, c(1, 31, 24:25, 27:30, 2:23, 26)]
ivy_hitting <- ivy_hitting[order(ivy_hitting$WAR, decreasing = T),]

#####  Pitching WAR ######
for(i in 1:nrow(ivy_pitching)) {
  if(substring(ivy_pitching$IP[i], nchar(ivy_pitching$IP[i]) - 1, nchar(ivy_pitching$IP[i])) == ".1") {
    ivy_pitching$IP[i] <- as.numeric(gsub("1$", "3333", ivy_pitching$IP[i]))
  }
  
  else if(substring(ivy_pitching$IP[i], nchar(ivy_pitching$IP[i]) - 1, nchar(ivy_pitching$IP[i])) == ".2") {
    ivy_pitching$IP[i] <- as.numeric(gsub("2$", "6666", ivy_pitching$IP[i]))
  }
}

lgERA <- sum(ivy_pitching$ER, na.rm = T)/sum(ivy_pitching$IP, na.rm = T)*7
FIP_Constant <- lgERA - (13*sum(ivy_pitching$HR, na.rm = T) + 
                           3*sum(ivy_pitching$BB, na.rm = T) - 
                           2*sum(ivy_pitching$SO, na.rm = T))/sum(ivy_pitching$IP, na.rm = T)


ivy_pitching$FIP <- (13*ivy_pitching$HR + 3*ivy_pitching$BB - 2*ivy_pitching$SO)/ivy_pitching$IP +
  FIP_Constant

lgRA <- sum(ivy_pitching$R, na.rm = T)/sum(ivy_pitching$IP, na.rm = T)*7
ivy_pitching$FIPR <- ivy_pitching$FIP + lgRA-lgERA


ivy_pitching$ipg <- ivy_pitching$IP/ivy_pitching$GP
ivy_pitching$dPRW <- (((2*7 - ivy_pitching$ipg)*mean(ivy_pitching$FIPR, na.rm = T) + 
                         ivy_pitching$ipg*ivy_pitching$FIPR)/18 + 2)*1.5
ivy_pitching$RAAP <- mean(ivy_pitching$FIPR, na.rm = T) - ivy_pitching$FIPR
ivy_pitching$WPGAA <- ivy_pitching$RAAP/ivy_pitching$dPRW
ivy_pitching$replacement.level <- 0.03*(1 - ivy_pitching$GS/ivy_pitching$GP) + 
  0.12*(ivy_pitching$GS/ivy_pitching$GP)
ivy_pitching$WPGAR <- ivy_pitching$WPGAA + ivy_pitching$replacement.level
ivy_pitching$WAR <- ivy_pitching$WPGAR * ivy_pitching$IP/7
ivy_pitching <- ivy_pitching[order(ivy_pitching$WAR, decreasing = T), c(1, 33, 25:32, 2:25)]

write.table(ivy_hitting, "2018_Softball_Ivy_Hitting_War.csv", 
            row.names = F, col.names = T, sep = ",")
write.table(ivy_pitching, "2018_Softball_Ivy_Pitching_War.csv", 
            row.names = F, col.names = T, sep = ",")


############################### Baseball ######################################
ivy_hitting <- read.csv("ivy_baseball_hitting_2018.csv", as.is = T)
ivy_pitching <- read.csv("ivy_baseball_pitching_2018.csv", as.is = T)
ivy_games <- c(11, 17, 9, 12, 18, 15, 12, 14)
names(ivy_games) <- c("Brown", "Columbia", "Cornell", "Dartmouth", "Harvard",
                      "Penn", "Princeton", "Yale")
############ Batting WAR ############
# Hitting
ivy_hitting$H1B <- ivy_hitting$H - (ivy_hitting$H2B + ivy_hitting$H3B + ivy_hitting$HR)
ivy_hitting$PA <- (ivy_hitting$AB + ivy_hitting$BB + ivy_hitting$HBP + ivy_hitting$SF)
ivy_hitting$wOBA <-
  (0.69*ivy_hitting$BB + 0.72*ivy_hitting$HBP + 0.89*ivy_hitting$H1B + 1.27*ivy_hitting$H3B 
   + 2.10*ivy_hitting$HR)/ivy_hitting$PA

lwOBA <- mean(ivy_hitting$wOBA, na.rm = T)
ivy_hitting$wRAA <- (ivy_hitting$wOBA-lwOBA)/1.277*ivy_hitting$PA

# Baserunning
games <- sum(ivy_games)
ivy_hitting$SbATT[is.na(ivy_hitting$SbATT)] <- 0
ivy_hitting$CS <- ivy_hitting$SbATT - ivy_hitting$SB
runCS <- -(2*sum(ivy_hitting$R, na.rm = T)/(8 * 6 * games) + 0.075)
lwSB <- (sum(ivy_hitting$SB, na.rm = T)*0.2 + sum(ivy_hitting$CS, na.rm = T)*runCS)/
  (sum(ivy_hitting$BB, na.rm = T) + sum(ivy_hitting$HPB, na.rm = T) + sum(ivy_hitting$H1B, na.rm = T))

ivy_hitting$wSB <- ivy_hitting$SB*0.2 + ivy_hitting$CS*runCS - 
  lwSB*(ivy_hitting$BB + ivy_hitting$HBP + ivy_hitting$H1B)

runs.per.out <- sum(ivy_hitting$R, na.rm = T)/(8 * 6 * games)
ivy_hitting$wGDP <- -(ivy_hitting$GDP/ivy_hitting$AB * runs.per.out)
ivy_hitting$br.runs <- ivy_hitting$wGDP + ivy_hitting$wSB
RPW <- sum(ivy_hitting$R, na.rm = T)/(games)*1.5 + 3
ivy_hitting$rep.runs <- 57 * RPW*ivy_hitting$PA/sum(ivy_hitting$PA, na.rm = T)
ivy_hitting$WAR <- (ivy_hitting$wRAA + ivy_hitting$br.runs + ivy_hitting$rep.runs)/RPW
ivy_hitting <- ivy_hitting[, c(1, 31, 24:25, 27:30, 2:23, 26)]
ivy_hitting <- ivy_hitting[order(ivy_hitting$WAR, decreasing = T),]

#####  Pitching WAR ######
for(i in 1:nrow(ivy_pitching)) {
  if(substring(ivy_pitching$IP[i], nchar(ivy_pitching$IP[i]) - 1, nchar(ivy_pitching$IP[i])) == ".1") {
    ivy_pitching$IP[i] <- as.numeric(gsub("1$", "3333", ivy_pitching$IP[i]))
  }
  
  else if(substring(ivy_pitching$IP[i], nchar(ivy_pitching$IP[i]) - 1, nchar(ivy_pitching$IP[i])) == ".2") {
    ivy_pitching$IP[i] <- as.numeric(gsub("2$", "6666", ivy_pitching$IP[i]))
  }
}

lgERA <- sum(ivy_pitching$ER, na.rm = T)/sum(ivy_pitching$IP, na.rm = T)*8
FIP_Constant <- lgERA - (13*sum(ivy_pitching$HR, na.rm = T) + 
                           3*sum(ivy_pitching$BB, na.rm = T) - 
                           2*sum(ivy_pitching$SO, na.rm = T))/sum(ivy_pitching$IP, na.rm = T)


ivy_pitching$FIP <- (13*ivy_pitching$HR + 3*ivy_pitching$BB - 2*ivy_pitching$SO)/ivy_pitching$IP +
  FIP_Constant

lgRA <- sum(ivy_pitching$R, na.rm = T)/sum(ivy_pitching$IP, na.rm = T)*8
ivy_pitching$FIPR <- ivy_pitching$FIP + lgRA-lgERA

ivy_pitching$GP <- as.numeric(gsub("-.*", "", ivy_pitching$G))
ivy_pitching$GS <- as.numeric(gsub("[0-9]*-", "", ivy_pitching$G))


ivy_pitching$ipg <- ivy_pitching$IP/ivy_pitching$GP
ivy_pitching$dPRW <- (((2*8 - ivy_pitching$ipg)*mean(ivy_pitching$FIPR, na.rm = T) + 
                         ivy_pitching$ipg*ivy_pitching$FIPR)/18 + 2)*1.5
ivy_pitching$RAAP <- mean(ivy_pitching$FIPR, na.rm = T) - ivy_pitching$FIPR
ivy_pitching$WPGAA <- ivy_pitching$RAAP/ivy_pitching$dPRW
ivy_pitching$replacement.level <- 0.03*(1 - ivy_pitching$GS/ivy_pitching$GP) + 
  0.12*(ivy_pitching$GS/ivy_pitching$GP)
ivy_pitching$WPGAR <- ivy_pitching$WPGAA + ivy_pitching$replacement.level
ivy_pitching$WAR <- ivy_pitching$WPGAR * ivy_pitching$IP/8
ivy_pitching <- ivy_pitching[order(ivy_pitching$WAR, decreasing = T), c(1, 34, 28:33, 2:28)]

write.table(ivy_hitting, "2018_Baseball_Ivy_Hitting_War.csv", 
            row.names = F, col.names = T, sep = ",")
write.table(ivy_pitching, "2018_Baseball_Ivy_Pitching_War.csv", 
            row.names = F, col.names = T, sep = ",")



