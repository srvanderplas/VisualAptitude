# Given a 5x5 grid with squares 1-25 
# i.e. 
# | 1| 2| 3| 4| 5|
# | 6| 7| 8| 9|10|
# |11|12|13|14|15|
# |16|17|18|19|20|
# |21|22|23|24|25|

# How quickly can someone locate the target in a group of 24 distractors?

library(ggplot2)
library(reshape2)
library(plyr)

# define coordinate grid
rect.coords <- data.frame(num=1:25, xstart=(0:24)%%5, xend=(0:24)%%5+1, ystart=5-floor((0:24)/5)-1, yend=5-floor((0:24)/5))

# function to prevent two neighboring squares from being filled in
is.adjacent <- function(samp){
  (abs(diff(samp))==1 & diff(floor((samp-1)/5))==0) || 
   (diff((samp-1)%%5)==0 & abs(diff(floor((samp-1)/5)))==1) || 
   (diff(samp)==0)
}

# List of all possible choices in which the squares are non-adjacent
plot.choices <- expand.grid(1:25, 1:25)
plot.choices <- plot.choices[!apply(plot.choices, 1, is.adjacent),]

# data frame for all pictures together (to make the key)
overall <- data.frame()

# generate 25 data sets
set.seed(9090983726)
for(i in 1:25){
  # sample 25 different stimuli sets
  stimuli <- plot.choices[sample(1:nrow(plot.choices), 25, replace=FALSE),]
  
  # replace one of them with stimuli # 13
  ans <- sample(c(1:12, 14:25), 1)
  stimuli[ans,] <- stimuli[13,]
  
  # Make stimuli into a data frame with each square filled in as a separate row
  stimuli <- melt(stimuli)
  stimuli$variable <- rep(1:25, times=2)
  names(stimuli) <- c("plot", "coords")
  
  # save answer
  stimuli$ans <- ans
  
  # make nice labels
  stimuli$plot <- as.character(stimuli$plot)
  stimuli$plot[which(stimuli$plot=="13")] <- "TARGET"
  stimuli$plot[c(14:25,39:50)] <- rep(as.character(13:24), times=2)
  stimuli$plot <- factor(stimuli$plot,levels=c(as.character(1:12), "TARGET", as.character(13:24)))
  
  # add in coordinate information for plotting
  stimuli <- cbind(stimuli, rect.coords[stimuli$coords,])
  
  # save trial number for answer key later
  stimuli$trial <- i
  overall <- rbind(overall, stimuli)
  
  # save plot
  ggplot() + 
    geom_rect(data=rect.coords, aes(xmin=xstart, ymin=ystart, xmax=xend, ymax=yend), fill="#FFFFFF", colour="#000000") +
    geom_rect(data=stimuli, aes(xmin=xstart, ymin=ystart, xmax=xend, ymax=yend), fill="black", inherit.aes=FALSE) +
    facet_wrap(~plot) +
    coord_fixed() + 
    theme(axis.ticks=element_blank(), 
          panel.grid=element_blank(), 
          plot.background=element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  ggsave(paste("./Images/VisualSearch/VST-", i, ".pdf"), width=8, height=8, units="in", dpi=100)
}

key <- ddply(overall, .(trial), summarize, ans=unique(ans))
write.csv(key, "./Images/VisualSearch/VST-Key.csv", row.names=FALSE)
