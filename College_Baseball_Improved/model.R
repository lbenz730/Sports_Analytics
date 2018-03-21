### YUSAG College Baseball Rankings
### Luke Benz and Max Yuhas
### March 2018


### Read in Data
x <- read.csv("2017_scores.csv", as.is = T)
y <- read.csv("2018_scores.csv", as.is = T)

### Set 2017/2018 Weights
x$year <- "2017"
x$weights <- 1
y$year <- "2018"
y$weights <- 2
x <- rbind(x,y)
x$rundiff <- x$teamscore - x$oppscore

### Fit Model
lm.bb <- lm(rundiff ~ team + opponent + location, data = x, weights = weights)
power_rankings <- data.frame("team" = sort(unique(x$team)),
                             "yusag_coeff" = rep(NA, 297))
x$predrundiff <- predict(lm.bb, newdata = x)

### Power Rankings
scale_factor <- mean(lm.bb$coefficients[2:297])
power_rankings$yusag_coeff <- as.vector(c(0, lm.bb$coefficients[2:297]) - scale_factor)   
power_rankings <- power_rankings[order(power_rankings$yusag_coeff, decreasing = T),]
power_rankings$rank <- 1:nrow(power_rankings)
write.csv(power_rankings, "yusag_baseball_ranks.csv", row.names = F)

### Win Prob Model (Logistic Regression)
x$win <- ifelse(x$rundiff > 0, 1, 0)
glm.winprob <- glm(win ~ predrundiff, data = x, family = "binomial")
x$win[is.na(x$win)] <- predict(glm.winprob, newdata = x[is.na(x$win),], type = "response")