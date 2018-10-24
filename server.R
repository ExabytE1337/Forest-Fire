#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(cowplot)
# 0...... empty -> black
# 1...... tree -> green
# 2...... burning tree -> red
rotate <- function(x) t(apply(x, 2, rev))
withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
    txt <- capture.output(results <- expr, type = "output")
    if (length(txt) > 0) {
      insertUI(paste0("#", containerId), where = "beforeEnd",
               ui = paste0(txt, "\n", collapse = ""))
    }
    results
}

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

################ fire function
Firestarter <- function(A,f,p,L,N,VonNeumann){
    les2 <- A
    for (i in 2:(L+1)){
      for (j in 2:(L+1)){
        if (A[i,j]==2){
          les2[i,j] <- 0
        }
        else {
          if (A[i,j]==0){
            u <- runif(1)
            if (u<=p){
              les2[i,j] <- 1
            }
          }
          else {
            if (VonNeumann){ #vonneumann
              if (A[i-1,j]==2 || A[i+1,j]==2 || A[i,j-1]==2 || A[i,j+1]==2){
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
              if (A[i-1,j]==2 || A[i+1,j]==2 || A[i,j-1]==2 || A[i,j+1]==2 || A[i-1,j-1] || A[i-1,j+1] || A[i+1,j-1] || A[i+1,j+1]){
                les2[i,j] <- 2
              }
              else{
                u <- runif(1)
                if(u<=p){
                  les2[i,j] <- 2
                }
              }
            }
          }
        }
      }
    }
    return(les2)
}
    
# Define server logic required to draw a histogram
#Firestarter(f,p,L,N,doplot,saveoutp,showprogress,color,VonNeumann,alone)
shinyServer(function(input, output, session) {
  kvantil <- qnorm(0.975)
  vals <- reactiveValues( A = matrix(0,5,5),counter = 0,B = matrix(0,5,5),lis = 0,it=0, trees = NULL,l = 0, q= 0,poldlzka = 0,priemer = 0, ISu=0,ISd =0)
  progress <- shiny::Progress$new(session, min=0, max=1)
  progress$close()
  ######################### start
  observeEvent(input$Start, {progress$initialize(session, min = 0, max = 1)
  #progress <- shiny::Progress$new()
    })
  observeEvent(input$Start, {
    vals$counter <- 0 #vzdy ked stlacim button tak vynulujem iteraciu, takze po stlaceni mozem spustat znova
    # Make sure it closes when we exit this reactive, even if there's an error
    progress$set(message = "Running Forest Fire simulation", value = 0)
    maxIter <- isolate(input$N)
    if (input$color==T) color=c("green","red")
    else color=c("#424242","white") #daj spravnu sedu
    vals$A <- matrix(rep(0,(input$L + 2)^2),nrow=input$L+2,ncol=input$L+2)
    #tu poviem ze na button click rob graf ak sa zmeni vals$
    shinyjs::disable("Start")
    shinyjs::disable("N")
    shinyjs::disable("L")
    shinyjs::disable("prob2")
    shinyjs::disable("prob1")
    shinyjs::disable("color")
    shinyjs::disable("VonNeumann")
    shinyjs::disable("Start2")
    shinyjs::disable("burnin")
    shinyjs::disable("delta")
    output$A <- renderPlot({
      par(bg = 'black')
      par(mar=c(0,0,0,0)) #zahod margins
      image(vals$A[-c(1,dim(vals$A)[1]),-c(1,dim(vals$A)[2])],breaks =c(-1,0,1,2) ,col=c("black", color),xaxt='n', ann=FALSE,yaxt='n',bty="n",asp=1)
      # par(mar=c(5, 4, 4, 2) + 0.1)
      #image(vals$A,xaxt='n', ann=FALSE,yaxt='n',bty="n",asp=1)
      })
    
  })
  observeEvent(input$Start, {#na Start button klik
    observe({         
      maxIter <- isolate(input$N)
       isolate({
         vals$A <- Firestarter(vals$A,input$prob1,input$prob2,input$L,input$N,input$VonNeumann)
         vals$counter <- vals$counter + 1 #for loop
        # output$skuska <- renderPrint(vals$counter)
         progress$inc(1/maxIter, detail = paste("Iteration number", vals$counter))
       })
       
      
      if (isolate(vals$counter) < maxIter){
        #nerob ak si prekrocil iteracie
        #cat(vals$counter,"\") #progres do konzole
        invalidateLater(0, session)
        }
    })
  })
  observe({vals$counter
    if (vals$counter == input$N){
            shinyjs::enable("Start")
            shinyjs::enable("N")
            shinyjs::enable("L")
            shinyjs::enable("prob2")
            shinyjs::enable("prob1")
            shinyjs::enable("color")
            shinyjs::enable("VonNeumann")
            shinyjs::enable("Start2")
            shinyjs::enable("burnin")
            shinyjs::enable("delta")
            on.exit(progress$close())
            }
  })
  ######################### start2
  observeEvent(input$Start2, {#na Start MC button klik
    isolate({
    vals$B <- matrix(rep(0,(input$L + 2)^2),nrow=input$L+2,ncol=input$L+2)
    vals$it <- 0
    vals$trees <- NULL
    vals$l <- 0
    vals$q <- 0
    vals$poldlzka <- 0
    vals$lis <- 0
    })
    #removeUI(paste0("#console"))
    #insertUI(paste0("#console"),where = "beforeEnd",ui = paste0("", "\n", collapse = ""))
    shinyjs::disable("Start")
    shinyjs::disable("N")
    shinyjs::disable("L")
    shinyjs::disable("prob2")
    shinyjs::disable("prob1")
    shinyjs::disable("color")
    shinyjs::disable("VonNeumann")
    shinyjs::disable("Start2")
    shinyjs::disable("burnin")
    shinyjs::disable("delta")
    withConsoleRedirect("console", {
      cat("Running Burning Forest simulation of size",input$L,"x",input$L,"and",input$N,"cycles.","\n")
      cat("Probability of a tree getting hit by a lightning:",input$prob1,".\n")
      cat("Probability of a tree growing:",input$prob2,".\n")
      cat("Confidence interval length required:",input$delta,".\n")
    })
  })
  observeEvent(input$Start2, {#na Start MC button klik 
    observe({   
      maxIter <- isolate(input$N)
      isolate({
        delta <- input$delta
        burnin <- input$burnin
       # while(lengthIS > delta || it <= burnin){
          for (k in 1:maxIter){
            les2 <- Firestarter(vals$B,input$prob1,input$prob2,input$L,input$N,input$VonNeumann)
            vals$B <-les2
          }
          vals$trees <- c(vals$trees,sum(vals$B[vals$B==1]))
          vals$it <- vals$it + 1
          vals$l <- vals$l + vals$trees[vals$it] 
          vals$q <- vals$q + vals$trees[vals$it]^2
          vals$priemer <- vals$l / vals$it
          vals$poldlzka <- kvantil * sqrt((vals$q / vals$it - vals$priemer^2) / vals$it)
          vals$ISd <- vals$priemer - vals$poldlzka
          vals$ISu <- vals$priemer + vals$poldlzka
          lengthIS <- 2*vals$poldlzka
          vals$lis <- lengthIS
          withConsoleRedirect("console", {
            if(vals$it>2){
             cat("Iteration number:",vals$it-1,"      Confidence interval length: ",vals$lis,"\n")
            }
          })
      })
        if (isolate(vals$it) <= burnin || isolate(vals$lis) > delta){
            invalidateLater(0,session)
        }
        else{
          isolate({
            shinyjs::enable("Start")
            shinyjs::enable("N")
            shinyjs::enable("L")
            shinyjs::enable("prob2")
            shinyjs::enable("prob1")
            shinyjs::enable("color")
            shinyjs::enable("VonNeumann")
            shinyjs::enable("Start2")
            shinyjs::enable("burnin")
            shinyjs::enable("delta")
            output$lis <- renderText({
              #paste("Final length of CI: ",round(vals$lis,4))
              paste("Final CI: [",round(vals$ISd,2)," ; ",round(vals$ISu,2),"] with length ", round(vals$lis,2))
            })
          })
           
        }
    })
  })
  ######################### About
  observeEvent(input$about, {
    showModal(modalDialog(
    title = "About",
    img(src="vonneumann.png", height = 100, width = 100),
    HTML("Von Neumann neighborhood"),
    br(),
    br(),
    img(src="moore.png", height = 100, width = 100),
    HTML("Moore neighborhood"),
    br(),
    br(),
    HTML("The shiny app was based on my project Forest Fire Simulation for 2-PMS-123.<br>
                            Author: Jakub Kov\u00E1\u010D"),
      easyClose = TRUE
    ))
  })
  ######################### About2
  observeEvent(input$about2, {
    showModal(modalDialog(
      title = "About MC", HTML("This is a Monte Carlo simulation for the confidence interval for the number of trees in a forest with parameters specified on the Simulation tab.
      <br>Two additional parameters are needed:
      <ul><li>Length of the burn-in phase: is needed to prevent too narrow CI's after a few iterations</li>
      <li>Lenght of the confidence interval: stopping criteria</li></ul>"),
                               
      br(),
      br(),
      HTML("The shiny app was based on my project Forest Fire Simulation for 2-PMS-123.<br>
                            Author: Jakub Kov\u00E1\u010D"),

      easyClose = TRUE
      ))
  })
  ######################### graf LenghtIS
  output$a <- renderPlot({
  lis2 <- c(0,123,150,90,64,30,25)
  burnin2 <- 20
  delta2 <- 30
  d <- data.frame(1:length(lis2),lis2)
  names(d) <- c("iteration","isl")
  head(d)
  a <- ggplot(d,aes(iteration,isl))+xlim(1,burnin2)+geom_vline(xintercept = burnin2,linetype = "dashed")
  a <- a + geom_hline(yintercept = delta2, linetype = "dashed") + xlab("ITERATION") + ylab("CI LENGTH") #+ggtitle("Progress of CI length")
  a <- a+geom_line(size=2, col = "#2A9FD6")+geom_point(shape = 19,size = 7, col = "#2A9FD6")
  a <- a + theme_BW_jk
  a <- ggdraw(a) + theme(panel.background = element_rect(fill = "black",colour = "black"))
  print(a)
  })
  
})
