library(ggplot2)
lis <- c(0,123,150,90,64,30,25)
burnin <- 20
delta <- 30
d <- data.frame(1:length(lis),lis)
names(d) <- c("iteration","isl")
head(d)
a <- ggplot(d,aes(iteration,isl))+geom_line(size=1)+geom_point(shape = 19,size = 3)+xlim(1,burnin)+geom_vline(xintercept = burnin,linetype = "dashed")
a <- a + geom_hline(yintercept = delta, linetype = "dashed")
a
