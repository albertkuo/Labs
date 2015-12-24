# Working with Albert's data
library(ggplot2)

# Read data
setwd('Flappy Bird')
albert <- read.table("scores_Albert.txt", quote="\"")
albert$index = 1:nrow(albert)
albert$highscore = 0
colnames(albert) = c("Score", "Index", "High_Score")

# Get list of new high scores
max = 0
high_scores = c()
high_index = c()
for(i in 1:nrow(albert)){
  if(albert$Score[i]>max){
    high_scores = c(high_scores, albert$Score[i])
    high_index = c(high_index, i)
    max = albert$Score[i]
    albert[i, 3] = 1
  }
}

# Scatter plot with high scores labeled
# Colors http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#DC143C")

ggplot(data=albert, aes(Index, Score)) + 
  geom_point(aes(color = as.factor(High_Score), 
                 shape = as.factor(High_Score))) + 
  #geom_smooth() +
  scale_colour_manual(values=cbPalette, labels=c("New Score","New High Score")) + 
  scale_shape_manual(values=c(1,9), labels=c("New Score","New High Score")) + 
  scale_y_continuous(expand = c(0,0), limits=c(0, 130)) +
  labs(title = "Albert's Scores", x = "Game Number") +
  theme(panel.background = element_blank(),
        axis.line.y = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.title = element_blank()) 

# Histogram of Counts
ggplot(data=albert, aes(x=Score)) + 
  geom_histogram(stat="bin", binwidth=5, color = "white", fill="darkgray") +  
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0))

# Histogram of Density
ggplot(data=albert, aes(x=Score)) + 
  geom_histogram(aes(y = ..density..), binwidth=3, color = "white", fill="gray") +  
  #geom_density(color="#009E73") + 
  stat_function(fun = dgeom, args = list(x=seq(0,100), prob=1/mean(albert$Score)), 
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
  labs(title="Albert's Scores")

# Times between new high scores
times = as.data.frame(diff(high_index))
times$index = high_index[-1]
colnames(times) = c("Time", "Index")
## Plot
ggplot(data = times, aes(x=Index, y=Time)) + 
  geom_bar(stat="identity", aes(fill=Time^(1/4))) +
  theme(panel.background = element_blank()) + 
  guides(fill=FALSE)

# Change between new high scores
delta = as.data.frame(diff(high_scores))
delta$index = 1:nrow(delta)
colnames(delta) = c("Change","Index")
## Plot
ggplot(data = delta, aes(x=Index, y=Change)) + 
  geom_bar(stat="identity", aes(fill=Change)) +
  theme(panel.background = element_blank()) + 
  guides(fill=FALSE)

# Scatter plot of time versus increase
test = cbind.data.frame(times$Index,times$Time,delta$Change)
colnames(test) = c("Index","Time","Change")
ggplot(data = test, aes(x=Time, y=Change)) + 
  geom_point(aes(color=Index, size=2)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.justification = c(1,1), legend.position = c(1,1)) + 
  guides(size=F) + 
  scale_color_gradient(name="Game #", 
                       breaks=rev(c(1,200,400,600,800)), 
                       labels=rev(c(1,200,400,600,800)),
                       limits=c(1,800),
                       high = "#132B43", low = "#56B1F7") +
  labs(title="Albert's New High Scores", y="Increase in High Score",
       x="Time Passed (since last high score)")
  
