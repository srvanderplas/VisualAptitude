linear.trend <- function(n=30, a=sample(c(-1, 1), 1)*rnorm(1, 3), b=rnorm(1), n.outliers=0){
  df <- data.frame(x=seq(-n/10, n/10, length.out=n))
  df$y <- a*df$x+b+rnorm(n)
  df$outlier <- FALSE
  # select outlier from the first 1/5 of the dataset or the last 1/5 of the dataset
  outlier.vals <- c(1:n)
  outlier.vals <- outlier.vals[outlier.vals<=n/5 | outlier.vals>=4*n/5]
  outlier <- sample(outlier.vals, 1)
  df$outlier[outlier] <- TRUE
  df$y[outlier] <- df$y[outlier]-sign(df$x[outlier])*3*a
  df$group <- as.numeric((df$x+rnorm(n))>0)+1
  return(df)
}