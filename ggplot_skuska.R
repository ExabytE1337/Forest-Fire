library(ggplot2)
lis <- c(0,123,150,90,64,30,25)
d <- data.frame(1:length(lis),lis)
names(d) <- c("iteration","isl")
head(d)
a <- ggplot(d,aes(iteration,isl))+geom_line(size=1)+geom_point(size = 3)
a
