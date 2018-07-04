library(XML)
library(RCurl)
library(ggplot2)
library(dplyr)

url <- "https://en.wikipedia.org/wiki/Nathan%27s_Hot_Dog_Eating_Contest"
x <- readHTMLTable(getURL(url))[[2]]

years <- as.numeric(as.character(x$Year))
years <- years[!is.na(years)]

winners <- as.character(x$Winner)
index <- sapply(sapply(x$Winner, substring, 1, 2), function(x){!is.na(as.numeric(x))})
winners <- winners[!index]
winners <- gsub("Labor Day", "", winners)
winners <- gsub("June 30", "", winners)
winners <- gsub("Independence Day", "", winners)
winners <- gsub("Memorial Day", "", winners)
winners <- gsub("[][]", "", winners)
winners <- gsub("[0-9]", "", winners)
winners <- gsub("July", "", winners)
winners <- gsub("citation needed", "", winners)
winners <- gsub("date", "", winners)
winners <- gsub("WOMEN'S", "", winners)
winners <- gsub("MEN'S", "", winners)
winners <- gsub("ONE-ON-ONE CHALLENGE WITH JAPAN", "", winners)
winners <- gsub("December", "", winners)
winners <- sapply(winners, function(x) gsub("\\s*$", "", gsub("^\\s*", "", x)))

x$dogs <- as.character(x$`Hot dogs incl. buns
(HDB)`)
x$dogs[index] <- as.character(x$Winner)[index]
x$dogs <- gsub("½", ".5", x$dogs)
x$dogs <- gsub("¾", ".75", x$dogs)
x$dogs <- gsub("¼", ".25", x$dogs)
x$dogs <- gsub(" 1/2", ".5", x$dogs)
x$dogs <- gsub("(Eat-off:)", "", x$dogs)
x$dogs <- gsub("[(*+)]", "", x$dogs)
x$dogs <- gsub("Unknown", NA, x$dogs)
x$dogs <- gsub("\n 5", "", x$dogs)
x$dogs <- as.numeric(gsub(" 3.5", "", x$dogs))

windex <- grep("WOMEN", as.character(x$Year))
women_winners <- gsub("WOMEN'S ", "" ,x$Year[windex])

duration <- as.character(x$`Contest duration`)
duration
duration <- gsub("½", ".5", duration)
duration <- gsub("¾", ".75", duration)
duration <- gsub("¼", ".25", duration)
duration <- gsub(" min.", "", duration)
duration <- gsub("Unknown", NA, duration)
clean <- function(x) strsplit(x,"\n")[[1]][1]
for(i in 1:length(duration)) {
  duration[i] <- clean(duration[i])
}
duration <- as.numeric(duration)

hotdog <- data.frame("year" = years,
                     "winner" = gsub("Unknown", NA, winners),
                     "dogs_eaten" = x$dogs[sapply(years, grep, x$Year)],
                     "women_winner" = c(women_winners, rep(NA, 46 - length(women_winners))),
                     "women_dogs_eaten" = c(x$dogs[windex], rep(NA, 46 - length(women_winners))),
                     "contest_duration" = duration[sapply(years, grep, x$Year)])
hotdog$dogs_per_min <- hotdog$dogs_eaten/hotdog$contest_duration
hotdog$secs_per_dog <- 60 * hotdog$contest_duration/hotdog$dogs_eaten
hotdog$women_dogs_per_min <- hotdog$women_dogs_eaten/hotdog$contest_duration
hotdog$women_secs_per_dog <- 60 * hotdog$contest_duration/hotdog$women_dogs_eaten
write.csv(hotdog, "nathans_hot_dog.csv", row.names = F)

hotdog$winnersimp <- ifelse(hotdog$winner == "Joey \"Jaws\" Chestnut", "Joey Chestnut",
                            ifelse(hotdog$winner == "Takeru \"The Prince\"/\"The Tsunami\" Kobayashi", "Takeru Kobayashi", "other"))
hotdog$wwinnersimp <- ifelse(hotdog$women_winner == "Miki Sudo", "Miki Sudo", "Sonya Thomas")

ggplot(filter(hotdog, !is.na(dogs_per_min), !is.na(winnersimp)), 
              aes(x = year, y = dogs_per_min, col = winnersimp)) + 
  geom_point() + 
  geom_line() + 
  scale_color_manual(values = c("red", "yellow3", "green")) + 
  labs(x = "Year", y = "Hot Dogs Consumed Per Minute", col = "Winner", title = "Nathan's Hot-Dog Eating Contest Consumption Rates") + 
  theme(axis.title = element_text(size = 14), plot.title = element_text(size = 18, hjust = 0.5))

ggplot(filter(hotdog, !is.na(secs_per_dog), !is.na(winnersimp)), 
       aes(x = year, y = secs_per_dog, col = winnersimp)) + 
  geom_point() + 
  geom_line() + 
  scale_color_manual(values = c("red", "yellow3", "green")) + 
  labs(x = "Year", y = "Seconds Per Hot Dog Eaten", col = "Winner", title = "Nathan's Hot-Dog Eating Contest Consumption Rates") + 
  theme(axis.title = element_text(size = 14), plot.title = element_text(size = 18, hjust = 0.5))


ggplot(filter(hotdog, !is.na(women_dogs_eaten), !is.na(wwinnersimp)), 
       aes(x = year, y = women_dogs_eaten, col = wwinnersimp)) + 
  geom_point() + 
  geom_line() + 
  scale_color_manual(values = c("red", "yellow3")) + 
  labs(x = "Year", y = "Seconds Per Hot Dog Eaten", col = "Winner", title = "Nathan's Hot-Dog Eating Contest Winner Totals (WOMEN)") + 
  theme(axis.title = element_text(size = 14), plot.title = element_text(size = 18, hjust = 0.5))


