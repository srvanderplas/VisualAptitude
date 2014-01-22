require(plyr)
linear.trend <- function(n=30, a=sample(c(-1, 1), 1)*rnorm(1, 3), b=rnorm(1), n.outliers=0){
  sd=sqrt(n/5)
  df <- data.frame(x=seq(-n/10, n/10, length.out=n))
  df$y <- rnorm(n, a*df$x+b+rnorm(n, sd=sd))
  df$outlier <- FALSE
  df$group <- as.numeric((df$x+rnorm(n, sd=1/sqrt(n/10)))>0)+1   
  if(n.outliers>0){
    # select outlier from the first 1/5 of the dataset or the last 1/5 of the dataset
    idx <- c(1:n)
    cutoff <- sample(c("low", "high"), 1)
    outlier.vals <- idx[(cutoff=="low" & idx<=n/5) | (cutoff=="high" & idx>=4*n/5)]
    outlier <- sample(outlier.vals, n.outliers)
    replace.vals <- idx[(cutoff!="low" & idx<=n/5) | (cutoff!="high" & idx>=4*n/5)]
    replace <- sample(replace.vals, n.outliers)
    df$outlier[outlier] <- TRUE
    df$y[outlier] <- rnorm(n.outliers, df$y[replace], sd=sd/3)
    df$group[outlier] <- 3
  }

  return(df)
}

permute.groups2 <- function(lineupdata, ngroups=3, pos=sample(1:20, 1)){
  ddply(lineupdata, .(.sample), function(df){
    dst <- dist(df[,c("x", "y")])
    if(sum(df$.sample==pos)==0){
      df$group.k = cutree(hclust(dst, method="complete"), round(nrow(df)/(ngroups+1)+1))%%ngroups+1
    } else {
      df$group.k = cutree(hclust(dst, method="complete"), ngroups)
    }
    df
  })
}