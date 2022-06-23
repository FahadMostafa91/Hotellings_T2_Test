library(Hotelling)
library(ggplot2)
library(ICSNP)
library(reshape2)

n <- 100  # no. of sample
group <- rep(1:2, each = n)   # grouping the data 
xs <- results_depth_15_power_20  # data from group-1
ys <- results_depth_15_power_40    # data from group-1

plot(xs)
plot(ys)
long <- melt(data.frame(group, xs, ys), id.vars = c("group"))
long$group <- as.factor(long$group)
ggplot(data = long, aes(x = (group), y = value)) + geom_violin(aes(fill = group), 
                                                               colour = "grey20", scale = "width") + facet_wrap(~variable) + theme_minimal() + 
  scale_fill_manual(values = c("gold", "cadetblue3"))

# rename the data
x2 <- xs
y2 <- ys 

depth_15 <-x2$tdepth
depth_15 <-y2$tdepth
df1 <- data.frame(depth_15,depth_15)
boxplot(df1)

power_20_density <-x2$tpdensity
power_40_density <-y2$tpdensity
df1 <- data.frame(power_20_density,power_40_density)

boxplot(df1)

library(glue)
TwoSampleT2Test <- function(X, Y){
  nx <- nrow(X)
  ny <- nrow(Y)
  delta <- colMeans(X) - colMeans(Y)
  p <- ncol(X)
  Sx <- cov(X)
  Sy <- cov(Y)
  S_pooled <- ((nx-1)*Sx + (ny-1)*Sy)/(nx+ny-2)
  t_squared <- (nx*ny)/(nx+ny) * t(delta) %*% solve(S_pooled) %*% (delta)
  statistic <- t_squared * (nx+ny-p-1)/(p*(nx+ny-2))
  p_value <- pf(statistic, p, nx+ny-p-1, lower.tail=FALSE)
  print(glue("Test statistic: {statistic}
 Degrees of freedom: {p} and {nx+ny-p-1}
 p-value: {p_value}"))
  return(list(TestStatistic=statistic, p_value=p_value))
}

TwoSampleT2Test(x2, y2)
