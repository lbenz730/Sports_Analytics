x <- read.csv("finals.csv", as.is = T)[1:8,]
y <- read.csv("win.csv", as.is = T)

colors <- c("darkorange1","goldenrod1", "firebrick", "grey",  "red", "blue", "purple", "brown4")

for(i in 1:8){
  plot(c(x$Pre_Tournament[i], x$Game.1[i], x$Game.2[i], x$Game.3[i], x$Game.4[i], 
         x$Game.5[i], x$Game.6[i], x$Game.7[i], x$Game.8[i], x$Game.9[i], x$Game.10[i],
         x$Game.11[i], x$Game.12[i], x$Game.13[i], x$Game.14[i]), 
       col = colors[i], type = "l",  lwd = 4,
       xaxt='n', ylim = c(0,100), main = "Odds to Advance to CWS Finals", 
       ylab = "Advance to CWS Percentage (%)", xlab = "Through")
  if(i < 8) {
    par(new=T)
  }
}

LAB = c("Pre", "Game 1", "Game 2", "Game 3", "Game 4", 
        "Game 5", "Game 6", "Game 7", "Game 8", "Game 9", "Game 10", 
        "Game 11", "Game 12", "Game 13", "Game 14", "Finals 1")
pos = 1:16
axis(1, labels = LAB, at = pos)
abline(h = 50, col = "black", lwd = 3, lty = 2)

labels<- x$Team
legend("topleft", xjust = 0, cex = 0.6, title="Legend", ncol = 2,
       labels, lwd=2, lty=c(1, 1, 1, 1, 1), col=colors)


for(i in 1:8){
  plot(c(y$Pre_Tournament[i], y$Game.1[i], y$Game.2[i], y$Game.3[i], y$Game.4[i],
         y$Game.5[i], y$Game.6[i], y$Game.7[i], y$Game.8[i], y$Game.9[i], y$Game.10[i],
         y$Game.11[i], y$Game.12[i], y$Game.13[i], y$Game.14[i], y$Game.15[i]),
       col = colors[i], type = "l",  lwd = 4,
       xaxt='n', ylim = c(0,100), main = "Odds to Win CWS", 
       ylab = "Win CWS Percentage (%)", xlab = "Through")
  if(i < 8) {
    par(new=T)
  }
}

axis(1, labels = LAB, at = pos)
legend("topleft", xjust = 0, cex = 0.75, title="Legend", ncol = 2,
       labels, lwd=2, lty=c(1, 1, 1, 1, 1), col=colors)
abline(h = 50, col = "black", lwd = 3, lty = 2)
