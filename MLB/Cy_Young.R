library(dplyr)
library(XML)

awards <- read.csv("awards.csv", as.is = T)
awards <- awards[, -c(2:3)]
cy_young <- data.frame(year = rep(NA, 111), 
                       player = rep(NA, 111))
for(i in 0:49){
  cy_young$year[2*i + 1] <- awards$Year[i + 1]
  cy_young$player[2*i + 1] <- strsplit(awards$NL.Cy.Young, "-")[i + 1][[1]]
  
  cy_young$year[2*i + 2] <- awards$Year[i + 1]
  cy_young$player[2*i + 2] <- strsplit(awards$AL.Cy.Young, "-")[i + 1][[1]]
}

for(i in 1:11) {
  cy_young$year[100 + i] <- awards$Year[50 + i]
  cy_young$player[100 + i] <- strsplit(awards$NL.Cy.Young, "-")[50 + i][[1]]
}

ids <- read.csv("http://crunchtimebaseball.com/master.csv", as.is = T)
pitchers <- read.csv("FanGraphs Splits Leaderboard Data.csv", as.is = T)


### Game Score Data
#for(i in 1:nrow(pitchers)){
# name <- pitchers$Name[i]
#id <- pitchers$playerid[i]
#year <- pitchers$Year[i]
#print(paste("Getting row", i, ":", name, year, sep = " "))
#url <- paste("http://www.fangraphs.com/statsd.aspx?playerid=",
#             id,
#            "&position=P&type=0&gds=&gde=&season=",
#             year, sep = "")
#data <- readHTMLTable(url)
#data <- as.data.frame(data[[length(data)]])
#these <- c(grep("Date", data$Date), grep("Total", data$Date))
#data <- data[-these, ]
#data$Date <- year
#data$pitcher <- name
#data <-data[data$GS == 1, ]

#if(i == 1) {
# y <- data
#}
#else{
# y <- rbind(y, data)
#}
#}

#write.table(y, "Game_Score_data.csv", row.names = F, col.names = T, sep = ",")

x <- read.csv("Game_Score_data.csv", as.is = T)
cy_young$player <- substr(cy_young$player, 1, nchar(cy_young$player) -1)


gsmean <- function(a, b) {
  return(round(mean(b$GSv2[b$pitcher == a$Name & b$Date == a$Year], na.rm = T), 2))
}
gssd <- function(a, b) {
  return(round(sd(b$GSv2[b$pitcher == a$Name & b$Date == a$Year], na.rm = T), 2))
}

is.cyyoung <- function(a,b) {
  for(i in 1:nrow(b)){
    if(a$Name == b$player[i] & a$Year == b$year[i]){
      return(T)
    }
  }
  return(F)
}

is.cyyoung2 <- function(a,b) {
  for(i in 1:nrow(b)){
    if(a$pitcher == b$player[i] & a$Date == b$year[i]){
      return(T)
    }
  }
  return(F)
}




y <- pitchers[pitchers$IP >= 162, c(1,2)]

for(j in 1:nrow(y)){
  y$cyyoung[j] <- is.cyyoung(y[j,],cy_young)
}

for(i in 1: nrow(y)){
  y$gs_mean[i] <- gsmean(y[i,], x)
  y$gs_sd[i] <- gssd(y[i,], x)
  
}
y$col <- "black"
y$col[y$cyyoung] <- "red"

plot(gs_sd ~ gs_mean, data = y, col = y$col, pch = 19,
     xlab = "Game Score Mean (Dominance)",
     ylab = "Game Score SD (Consistency)",
     main = "Cy Young Award: Consistency or Dominance?")
legend("topright", col = "red", "Cy Young Season", pch = 19)

glm.0 <- glm(cyyoung ~ gs_sd + gs_mean, data=y,
             family=binomial(link=logit))
summary(glm.0)


hist(y$gs_mean, breaks = 250, xlab = "Game Score Mean",
     main = "Histogram of Game Score Mean")
abline(v = y$gs_mean[y$cyyoung], col = "red", lwd = 4)
legend("topleft", col = "red", "Cy Young Season", title = "Legend", lwd = 2)

hist(y$gs_sd, breaks = 250, main = "Histogram of Game Score SD",
     xlab = "Pitcher SD")
abline(v = y$gs_sd[y$cyyoung], col = "red", lwd = 4)
legend("topleft", col = "red", "Cy Young Season", title = "Legend", lwd = 2)




for(j in 1:nrow(x)){
  x$cyyoung[j] <- is.cyyoung2(x[j,],cy_young)
}

gscy_mean <- mean(x$GSv2[x$cyyoung], na.rm = T)
gscy <- function(a, b){
  return(sum(b$GSv2[b$pitcher == a$Name & b$Date == a$Year] > gscy_mean)/
           sum(b$GS[b$pitcher == a$Name & b$Date == a$Year]))
}


for(j in 1:nrow(y)){
 y$cy_starts[j] <- gscy(y[j,], x)
}


hist(y$cy_starts, breaks = 250, main = "Frequency of Cy Young Quality Starts",
     xlab = "Cy Young Quality Start Frequency")
abline(v = y$cy_starts[y$cyyoung], col = "red", lwd = 2)
legend("topleft", col = "red", "Cy Young Season", title = "Legend", lwd = 2)



