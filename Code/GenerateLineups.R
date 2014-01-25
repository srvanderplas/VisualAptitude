library(nullabor)
library(ggplot2)
library(plyr)

source("./Code/theme_lineup.R")
source("./Code/GenerateData.R")

set.seed(31415926)

# Linear trend alone - no outliers
N <- 30
seeds <- c(699181, 190785, 746705, 938857, 256246, 
           130991, 829750, 107019, 310685, 968407, 
           982098, 247471, 658815, 828038, 956567, 
           706934,  74904, 743373, 955470, 339582, 
           230370, 594429, 959280, 677468, 301141, 
           523820, 397938, 952596, 726989, 247199)
# seeds <- sample(0:1e6, N) + sample(0:1e3, N) + sample(0:100, N)
answers <- data.frame()

colors <- c("#882E72", "#D92120", "#4EB265", "#3F4EA1", 
            "#E7742F", "#1965B0", "#B1B343", "#4683C1", "#DFA53A")
permute.var <- c("x", "y")
for(i in 1:N){
  set.seed(seeds[i])
  dframe <- linear.trend()
  
  # Slope alone
  pos.x <- sample(1:20, 1)
  filename <- paste("./Images/Lineups/NoOutliers-", i, "-Slope.png", sep="")
  pv <- sample(permute.var, 1)
  lineupdata <- lineup(null_permute(pv), dframe, pos=pos.x)
  ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y), size=3) + 
    facet_wrap(~.sample) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=pos.x, target2=NA, seed=seeds[i], idx=i, permute.var=pv))
  
  # Color alone
  pos.x <- sample(1:20, 1)
  pv <- "group"
  filename <- paste("./Images/Lineups/NoOutliers-", i, "-Color.png", sep="")
  lineupdata <- lineup(null_permute(pv), dframe, pos=pos.x)
  ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, color=factor(group)), size=3) + 
    facet_wrap(~.sample) + 
    scale_color_manual(values=colors[c(2,8)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=NA, target2=pos.x, seed=seeds[i], idx=i, permute.var=pv))
  
  # Slope vs. Color
  pos.x <- sample(1:20, 1)
  pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
  pv <- sample(permute.var, 1)
  filename <- paste("./Images/Lineups/NoOutliers-", i, "-SlopeColor.png", sep="")
  lineupdata <- permute.groups2(lineup(null_permute(pv), dframe, pos=pos.x), 
                                ngroups=3, pos=pos.y)
  
  ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, color=factor(group.k)), size=3) + 
    facet_wrap(~.sample) + 
    scale_color_manual(values=colors[c(3,5,6)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=pos.x, target2=pos.y, seed=seeds[i], idx=i, permute.var=pv))
}

write.csv(answers, "./Images/Lineups/NoOutliersKey.csv", row.names=FALSE)