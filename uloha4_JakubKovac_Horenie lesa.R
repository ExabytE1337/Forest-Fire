Firestarter <- function(f,p,L,N,doplot,saveoutp,showprogress,color,VonNeumann,alone) {
  # f.............. probability of a tree getting hit by a lightning
  # p.............. probability of a tree growing
  # L.............. size of a square forest
  # N.............. number of cycles
  # doplot......... plot each iteration T/F
  # saveoutp....... save images to working directory T/F
  # showprogress... show progress bar of each simulation run T/F
  # color.......... plot green/red vs. white/grey
  # VonNeumann..... type of neighbour definition used T/F VonNeumann vs. Moore
  # alone.......... display parameters called T/F
  #watch out for colors if one of the states missing ->image() function has its flaws
  
  #http://bib.irb.hr/datoteka/278897.Ljiljana_Bodrozic_ceepus2006_2.pdf 
  #podla tohto pouzivame Von Neumannovo okolie
  #mozno je zaujimavejsie Moorove okolie
  if(missing(color))
    color <- T
  if (color==T) color=c("green","red")
  else color=c("grey32","white")
  if(missing(VonNeumann))
    VonNeumann <- T
  # 0...... empty -> black
  # 1...... tree -> green
  # 2...... burning tree -> red
  if(missing(doplot))
    doplot <- F
  if(missing(saveoutp))
    saveoutp <- F
  if(missing(showprogress))
    showprogress <- F
  if(missing(alone))
    color <- T
  rotate <- function(x) t(apply(x, 2, rev))
  if (alone){
    cat("\n")
    cat("Running Burning Forest simulation for a forest of size",L,"x",L,"and",N,"cycles.","\n")
    cat("\n")
  }
  pocitac <- 0
  progress <- 0
  ptm <- proc.time()
 #budeme generovat v cykle? ak ano program bude pomalsi, ak nie tak mozu nastat velke pamatove naroky
  les <- matrix(rep(0,(L + 2)^2),nrow=L+2,ncol=L+2) #trik pre overovanie hodnot
  les2 <- les #tuto velky pozor aby sa vsetko vykonalo simultanne :)
  for (k in 1:N){
    if (showprogress && (!doplot || saveoutp)){
      if (k>=pocitac){
        pocitac <- pocitac+N/100*5
        if (progress==0 || progress==1) cat(progress*5,"  % \n")
        else cat(progress*5," % \n")
        progress <- progress+1#po 5% vypisujeme progress
      }
    }
    if (saveoutp){
     if (k < 10) {name = paste('000',k,'plot.png',sep='')}
     if (k < 100 && k >= 10) {name = paste('00',k,'plot.png', sep='')}
     if (k >= 100) {name = paste('0', k,'plot.png', sep='')}
     # png(name)
     png(name,width = 1000,height = 1000,bg="black")
    }
   for (i in 2:(L+1)){
     for (j in 2:(L+1)){
       if (les[i,j]==2){
         les2[i,j] <- 0
       }
       else {
         if (les[i,j]==0){
           u <- runif(1)
           if (u<=p){
             les2[i,j] <- 1
           }
         }
         else {
           if (VonNeumann){
             if (les[i-1,j]==2 || les[i+1,j]==2 || les[i,j-1]==2 || les[i,j+1]==2){
                 les2[i,j] <- 2
             }
             else{
              u <- runif(1)
              if(u<=f){
                 les2[i,j] <- 2
               }
             }
           }
           else{# Moore
             if (les[i-1,j]==2 || les[i+1,j]==2 || les[i,j-1]==2 || les[i,j+1]==2 || les[i-1,j-1] || les[i-1,j+1] || les[i+1,j-1] || les[i+1,j+1]){
               les2[i,j] <- 2
             }
             else{
               u <- runif(1)
               if(u<=f){
                 les2[i,j] <- 2
               }
             }
           }
         }
       }
      }
   }
    les <-les2
       if (doplot==T){Sys.sleep(0.05)
         par(mar=c(0,0,0,0)) #zahod margins
         image(les[-c(1,dim(les)[1]),-c(1,dim(les)[2])],breaks =c(-1,0,1,2) ,col=c("black", color),xaxt='n', ann=FALSE,yaxt='n',bty="n",asp=1)
         par(mar=c(5, 4, 4, 2) + 0.1) #default
         if (saveoutp) dev.off()
       }
  }
  
  if (doplot==F || saveoutp==T){
  par(mar=c(0,0,0,0)) #zahod margins
  image(les[-c(1,dim(les)[1]),-c(1,dim(les)[2])],breaks =c(-1,0,1,2), col=c("black", color),xaxt='n', ann=FALSE,yaxt='n',bty="n",asp=1)
  par(mar=c(5, 4, 4, 2) + 0.1) #default
  }
  if(alone){
    cat("100 % Simulation ended.","\n")
    cat("\n")
    cat("User time elapsed:",(proc.time()-ptm)[1],"s.\n")
  }
  return(invisible(rotate(rotate(rotate(les[-c(1,dim(les)[1]),-c(1,dim(les)[2])])))))
  
  if (saveoutp){
    cat("\n")
    cat("Graphical output saved in working directory.\n")
  }
}
######################################################################################################
#          (f,p,L,N,doplot,saveoutp,showprogress,color,VonNeumann,alone)
Firestarter(0.0005,0.05,100,300,T,F,F,T,T,T)
######################################################################################################
Number_of_Trees <- function(f,p,L,N,delta,color,output,burnin){
  options(scipen =8)
  if(missing(color))
    color <- T
  if(missing(burnin))
    burnin <- 10
  if(missing(output))
    output <- F
  trees <- NULL
  lengthIS <- delta+1
  l <- 0; q <- 0
  it <- 1
  kvantil <- qnorm(0.975)
  if (output) {
    cat("Running Burning Forest simulation for a forest of size",L,"x",L,"and",N,"cycles.","\n")
    cat("Probability of a tree getting hit by a lightning:",f,".\n")
    cat("Probability of a tree growing:",p,".\n")
  }
  cat("Confidence interval length required:",delta,".\n")
  cat("\n")
  
  while(lengthIS > delta || it <= burnin){
    pes <- Firestarter(f,p,L,N,F,F,F,color,T,F)
    trees <- c(trees, sum(pes[pes==1]))
     l <- l + trees[it] 
     q <- q + trees[it]^2
     priemer <- l / it
     poldlzka <- kvantil * sqrt((q / it - priemer^2) / it)
     ISd <- priemer - poldlzka
     ISu <- priemer + poldlzka
     #if(it>=burnin) 
       lengthIS <- ISu - ISd
     it <- it + 1
     if (output){
       if(it>2){
         if (it>burnin) b <- NULL
         else b <- "(burn-in phase)"
         cat("Iteration number:",it-1,"      Confidence interval length: ",lengthIS,b,"\n")
       }
     }
  }
  if (output) cat("---------------------------------------------------------------- \n")
  cat("Number of iterations needed:",it-1,".\n")
  cat("Final confidence interval for number of trees after",N,"cycles: (",ISd,";",ISu,").\n")
  cat("Number of trees estimated:",priemer,".\n")
  return(invisible(c(ISd,priemer,ISu)))
}
######################################################################################################
#                          (f,p,L,N,delta,color,output,burnin)
Number_of_Trees(0.0005,0.05,80,100,100,T,T,20)
######################################################################################################
