library(ncaahoopR)
library(gridExtra)
library(ggplot2)

qf1 <- ggplot(get_pbp_game(401083166, T), 
              aes(x = max(secs_remaining) - secs_remaining, y = winprob)) + 
  geom_ribbon(ymin = 0, aes(ymax = winprob), fill = "#0C2340") + 
  geom_ribbon(ymax = 1, aes(ymin = winprob), fill = "#E87722") + 
  ylim(c(0,1)) + 
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
  

qf2 <- ggplot(get_pbp_game(401082672, T), 
              aes(x = max(secs_remaining) - secs_remaining, y = winprob)) + 
  geom_ribbon(ymin = 0, aes(ymax = winprob), fill = "#A6192E") + 
  geom_ribbon(ymax = 1, aes(ymin = winprob), fill = "#0736A4") + 
  ylim(c(0,1)) + 
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

qf3 <- ggplot(get_pbp_game(401082335, T), 
              aes(x = max(secs_remaining) - secs_remaining, y = winprob)) + 
  geom_ribbon(ymin = 0, aes(ymax = winprob), fill = "#E84A27") + 
  geom_ribbon(ymax = 1, aes(ymin = winprob), fill = "#C1C6C8") + 
  ylim(c(0,1)) + 
  theme_minimal() + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

qf4 <- ggplot(get_pbp_game(401083404, T), 
              aes(x = max(secs_remaining) - secs_remaining, y = winprob)) + 
  geom_ribbon(ymin = 0, aes(ymax = winprob), fill = "#F1BE48") + 
  geom_ribbon(ymax = 1, aes(ymin = winprob), fill = "#CC0033") + 
  ylim(c(0,1)) + 
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

sf1 <- ggplot(get_pbp_game(401096921, T), 
              aes(x = max(secs_remaining) - secs_remaining, y = winprob)) + 
  geom_ribbon(ymin = 0, aes(ymax = winprob), fill = "#0736A4") + 
  geom_ribbon(ymax = 1, aes(ymin = winprob), fill = "#E87722") + 
  ylim(c(0,1)) + 
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

sf2 <- ggplot(get_pbp_game(401096920, T), 
              aes(x = max(secs_remaining) - secs_remaining, y = 1 - winprob)) + 
  geom_ribbon(ymin = 0, aes(ymax = 1 - winprob), fill = "#CC0033") + 
  geom_ribbon(ymax = 1, aes(ymin = 1 - winprob), fill = "#C1C6C8") + 
  ylim(c(0,1)) + 
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

final <- ggplot(get_pbp_game(401096923 , T), 
              aes(x = max(secs_remaining) - secs_remaining, y = winprob)) + 
  geom_ribbon(ymin = 0, aes(ymax = winprob), fill = "#C1C6C8") + 
  geom_ribbon(ymax = 1, aes(ymin = winprob), fill = "#0736A4") + 
  ylim(c(0,1)) + 
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

blank <- ggplot() + theme_minimal()

title <- ggplot() + 
  theme_minimal() + 
  annotate("text", x = 4, y = 45, label = "Maui Invitational \n Win Probability Bracket",
           size = 10) + ylim(c(0,80)) + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

name <- ggplot() + 
  theme_minimal() + 
  annotate("text", x = 4, y = 45, label = "Luke Benz \n (@recspecs730)",
           size = 4) + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

df <- data.frame(Team = c("Auburn", "Duke", "Xavier", "San Diego St.", 
                          "Gonzaga", "Illinois", "Iowa St.", "Arizona"),
                 num = 0)
legend <- ggplot(df, aes(x = Team, y = num, fill = Team)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#CC0033", "#E87722","#0736A4", "#C1C6C8",  
                                "#E84A27","#F1BE48", "#A6192E", "#0C2340"), 
                    labels = c("Arizona", "Auburn", "Duke", "Gonzaga", 
                               "Illinois", "Iowa St.", "San Diego St.", "Xavier")) + 
  theme_minimal() + 
  labs(fill = "") + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = "white",
                                         size=0.5, linetype="solid", 
                                         colour ="black")) + 
  guides(fill=guide_legend(ncol=2))

grid.arrange(qf1, blank, title, 
             blank, sf1, name,
             qf2, blank, blank,
             blank, blank, final,
             qf3, blank, blank,
             blank, sf2, blank,
             qf4, blank, legend, ncol = 3)
