library(XML)
library(ggplot2)

### Scrape Data from Ivy Football Website
for(j in 2012:2016) {
  yr <- paste(j, j - 1999, sep = "-")
  url <- paste("http://ivyleague.prestosports.com/sports/fball/", yr,"/stats/enhanced/teams?sort=ppg&r=0&pos=sco", sep = "")
  tmp <- readHTMLTable(url)
  x <- as.data.frame(tmp[[1]][-1])
  for(i in 2:13) {
    x <- cbind(x, as.data.frame(tmp[[i]][-(1:3)]))
  }
  x$year <- j
  
  y <- as.data.frame(tmp[[14]][-1])
  for(i in 14:26) {
    y <- cbind(y, as.data.frame(tmp[[i]][-(1:3)]))
  }
  y$year <- j
  
  if(j == 2012) {
    ivy_conf <- y
    ivy_total <- x
  }
  else{
    ivy_conf <- rbind(ivy_conf, y)
    ivy_total <- rbind(ivy_total, x)
  }
}

write.csv(ivy_conf, "ivy_conf.csv", row.names = F)
write.csv(ivy_total, "ivy_total.csv", row.names = F)

ivy_total <- read.csv("ivy_fb.csv", as.is = T)
ivy_total$Legend <- "Other"
ivy_total$Legend[ivy_total$team == "Yale"] <- "Yale"
ivy_total$Legend[ivy_total$champ] <- "Ivy League Champion"
ivy_total$Points_Per_Game <- ivy_total$ppg
ivy_total$Points_Allowed_Per_Game <- ivy_total$def_ppg

ggplot(ivy_total, aes(x = pass_ypg, y = rush_ypg)) + 
  geom_point(aes(size = Points_Per_Game, color = Legend)) + 
  labs(x = "Passing Yard Per Game", y = "Rushing Yards Per Game") + 
  ggtitle("Ivy League Offense 2012-2016") + 
  theme(plot.title = element_text(size = 16, hjust = 0.5)) + 
  scale_color_manual(values=c("forestgreen", "black", "darkblue"))

ggplot(ivy_total, aes(x = def_pass_ypg, y = def_rush_ypg)) + 
  geom_point(aes(size = Points_Allowed_Per_Game, color = Legend)) + 
  labs(x = "Passing Yards Allowed Per Game", y = "Rushing Yards Allowed Per Game") + 
  ggtitle("Ivy League Defenses 2012-2016") + 
  theme(plot.title = element_text(size = 16, hjust = 0.5)) + 
  scale_color_manual(values=c("forestgreen", "black", "darkblue"))

grep(ivy_total$rush_ypg[ivy_total$champ][9], ivy_total$rush_ypg[order(ivy_total$rush_ypg, decreasing = T)])


    

