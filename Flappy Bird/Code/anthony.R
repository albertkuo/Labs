# Working with Anthony's data
library(ggplot2)

# Read data
setwd('Flappy Bird')
anthony <- read.table("scores_Anthony.txt", quote="\"")
anthony$index = 1:nrow(anthony)
anthony$highscore = 0
colnames(anthony) = c("Score", "Index", "High_Score")

# Mark high score
anthony$High_Score[which.max(anthony$Score)] = 1

# Scatter plot with high scores labeled
# Colors http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#DC143C")

ggplot(data=anthony, aes(Index, Score)) + 
  geom_point(aes(color = as.factor(High_Score), 
                 shape = as.factor(High_Score))) + 
  scale_colour_manual(values=cbPalette, labels=c("New Score","High Score")) + 
  scale_shape_manual(values=c(1,9), labels=c("New Score","High Score")) + 
  scale_y_continuous(expand = c(0,0), limits=c(0, 230)) +
  labs(title = "Anthony's Scores", x = "Game Number") +
  theme(panel.background = element_blank(),
        axis.line.y = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.title = element_blank()) 

# Histogram of Density
ggplot(data=anthony2, aes(x=Score)) + 
  geom_histogram(aes(y = ..density..), binwidth=5, color = "white", fill="gray") +  
  #geom_density(color="#009E73") + 
  stat_function(fun = dgeom, args = list(x=seq(0,100), prob=1/mean(anthony$Score)), 
                aes(color="#E69F00"), size=1.5) +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(), legend.key = element_blank(),
        legend.text = element_text(colour="brown", size=12,face = "bold"),
        legend.justification = c(1,1), legend.position = c(1,1)) +
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(values = c("#E69F00", "blue", "green", "orange"),
                      labels = c("Theoretical Density")) +
  labs(title="Anthony's Scores")