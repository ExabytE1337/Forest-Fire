#plotovanie dlzky IS
library(ggplot2)
library(cowplot)
theme_BW_jk <- {theme_bw() + theme(axis.line=element_line(colour = "white"),
                                   #axis.text.x=element_blank(),
                                   #axis.text.y=element_blank(),
                                   axis.ticks=element_line(colour = "white"),
                                   #axis.title.x=element_blank(),
                                   axis.title=element_text(colour = "white",size = 15),
                                   #axis.title.y=element_text(colour = "white"),
                                   #axis.title.y=element_blank(),
                                   #legend.position="none",
                                   axis.text.x = element_text(colour = "white"),
                                   axis.text.y = element_text(colour = "white"),
                                   panel.background=element_rect(fill = "black", colour = NA),
                                   panel.border = element_rect(colour = "white"), #spravi tie biele ciary hore
                                   #panel.border=element_blank(),panel.grid.major=element_blank(),
                                   #panel.grid.minor=element_blank(),
                                   plot.background=element_rect(fill = "black", colour = NA))}
##################################################
lis <- c(0,123,150,90,64,30,25)
burnin <- 20
delta <- 30
d <- data.frame(1:length(lis),lis)
names(d) <- c("iteration","isl")
head(d)
a <- ggplot(d,aes(iteration,isl),group = 1)+xlim(1,burnin)+geom_vline(xintercept = burnin,linetype = "dashed")
a <- a + geom_hline(yintercept = delta, linetype = "dashed") + xlab("Iteration") + ylab("CI length") #+ggtitle("Progress of CI length")
a <- a+geom_line(size=1, col = "#2A9FD6")+geom_point(shape = 19,size = 5, col = "#2A9FD6")
a <- a + theme_gray()
a

#pridanie noveho bodu
nove_body <- c(8,20)
d
d[nrow(d)+1,] <- nove_body
d
a + geom_point(data = d,aes(iteration,isl),shape = 19,size = 5, col = "#2A9FD6") + geom_line(data = d, aes(iteration,isl),size=1, col = "#2A9FD6")

#plotovanie pasu IS
is_data <- read.table("C:\\Users\\jakub.kovac\\Documents\\SHINY PROJEKTY\\Forest Fire\\ISU.txt",header = FALSE)
is_data <- data.frame(1:nrow(is_data),is_data,(is_data$V2+is_data$V1)/2)
names(is_data) <- c("iteration","isl","isu","mid")
head(is_data)
g <- ggplot(is_data)+ geom_ribbon(aes(ymin = isl,ymax = isu,x=iteration), alpha = 0.3)
g <- g + geom_line(aes(x=iteration,y=mid),col ="#2A9FD6",size = 1)+ylab("Number of trees") +geom_vline(xintercept = burnin,linetype = "dashed")
g + theme(axis.text.x = element_text(colour = "white"),axis.text.y = element_text(colour = "white")) +xlab("Iteration")

#cierno bielo
b <- ggplot(is_data)+geom_vline(xintercept = burnin,linetype = "dashed", colour ="black")+ theme_BW_jk+ geom_ribbon(aes(ymin = isl,ymax = isu,x=iteration), alpha = 0.3, fill = "white")
b <- b + geom_line(aes(x=iteration,y=mid),col ="#2A9FD6",size = 2)+ylab("Number of trees") +xlab("Iteration")
b + theme(axis.text.x = element_text(colour = "white"),axis.text.y = element_text(colour = "white"))
b 
#ggdraw(b) + theme(panel.background = element_rect(fill = "black",colour = "black"))
