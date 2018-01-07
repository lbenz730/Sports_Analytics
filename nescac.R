library(XML)
library(ggplot2)
url <- "http://nescac.com/sports/mbkb/2017-18/teams?sort=&r=0&pos=def"

data <-readHTMLTable(url)
offense <- as.data.frame(data[[1]])
defense <- as.data.frame(data[[2]])

names(offense)[c(5,7,9)] <- c("fg_pct", "3fg_pct", "ft_pct")
names(defense)[c(5,7)] <- c("fg_pct", "3fg_pct")

names(offense) <- c("off_rk", "team", paste("off", names(offense)[3:18], sep = "_"))
names(defense) <- c("def_rk", "team", paste("def", names(defense)[3:17], sep = "_"))

nescac <- merge(offense, defense, by = intersect(names(offense), names(defense)))
nescac <- nescac[,! names(nescac) %in% c("off_fg", "def_fg", "off_rk", "def_rk", "off_3pt", "off_ft", "def_3pt")]

for(i in 2:ncol(nescac)) {
  nescac[,i] <- as.numeric(as.character(nescac[,i]))
}

ggplot(nescac, aes(x = off_pts, y = def_pts)) + 
  geom_point(aes(size = off_reb, color = off_3fg_pct)) +
  geom_text(label=nescac$team, nudge_x = 0.5, nudge_y = 0.5, check_overlap = T) +
  labs(x = "Offensive PPG", y = "Defensive PPG") +
  ggtitle("How NESCAC Men's Basketball Teams are Winning Games") + 
  theme(plot.title = element_text(size = 16, hjust = 0.5)) +   
  scale_size(name = "RPG") +
  scale_color_continuous(name = "3FG %") +
  geom_abline(slope = 1, intercept = 0) +
  annotate("text", x = 78, y = 65, label = "atop(bold('Elite Defense'))", parse = T) +
  annotate("text", x = 85, y = 75, label = "atop(bold('Good Offense + Rebounding'))", parse = T) +
  annotate("text", x = 95, y = 80, label = "atop(bold('Elite 3-Point Shooting'))", parse = T) +
  annotate("text", x = 76, y = 80, label = "atop(bold('Worse Offense Than Defense'))", parse = T) 
  
  
