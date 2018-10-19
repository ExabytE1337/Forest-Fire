#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
  # 0...... empty -> black
  # 1...... tree -> green
  # 2...... burning tree -> red
  rotate <- function(x) t(apply(x, 2, rev))

# Define server logic required to draw a histogram
#Firestarter(f,p,L,N,doplot,saveoutp,showprogress,color,VonNeumann,alone)
shinyServer(function(input, output, session) {
  
  vals <- reactiveValues( A = matrix(0,5,5),counter = 0,B = matrix(0,5,5))
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

         les2 <- vals$A
      #    #tuto velky pozor aby sa vsetko vykonalo simultanne :)
           for (i in 2:(input$L+1)){
             for (j in 2:(input$L+1)){

               if (vals$A[i,j]==2){
                 les2[i,j] <- 0
               }
                else {
                   if (vals$A[i,j]==0){
                     u <- runif(1)
                     if (u<=input$prob2){
                       les2[i,j] <- 1
                     }
                  }
                  else {
                    if (input$VonNeumann){ #vonneumann
                      if (vals$A[i-1,j]==2 || vals$A[i+1,j]==2 || vals$A[i,j-1]==2 || vals$A[i,j+1]==2){
                        les2[i,j] <- 2
                      }
                      else{
                        u <- runif(1)
                        if(u<=input$prob1){
                          les2[i,j] <- 2
                        }
                      }
                    }
                    else{# Moore
                      if (vals$A[i-1,j]==2 || vals$A[i+1,j]==2 || vals$A[i,j-1]==2 || vals$A[i,j+1]==2 || vals$A[i-1,j-1] || vals$A[i-1,j+1] || vals$A[i+1,j-1] || vals$A[i+1,j+1]){
                        les2[i,j] <- 2
                      }
                      else{
                        u <- runif(1)
                        if(u<=input$prob1){
                          les2[i,j] <- 2
                        }
                      }
                    }
                  }
                }
             }
           }
         vals$A <-les2
         vals$counter <- vals$counter + 1 #for loop
         output$skuska <- renderPrint(vals$counter)
         progress$inc(1/maxIter, detail = paste("Iteration number", vals$counter))
       })
       
      
      if (isolate(vals$counter) < maxIter){
        #nerob ak si prekrocil iteracie
        #cat(vals$counter,"\") #progres do konzole
        invalidateLater(0, session)}
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
    vals$B <- matrix(rep(0,(input$L + 2)^2),nrow=input$L+2,ncol=input$L+2)
    observe({         
      maxIter <- isolate(input$N)
      isolate({
        for (k in 1:maxIter){
          les2 <- vals$B
          #    #tuto velky pozor aby sa vsetko vykonalo simultanne :)
          for (i in 2:(input$L+1)){
            for (j in 2:(input$L+1)){
              
              if (vals$B[i,j]==2){
                les2[i,j] <- 0
              }
              else {
                if (vals$B[i,j]==0){
                  u <- runif(1)
                  if (u<=input$prob2){
                    les2[i,j] <- 1
                  }
                }
                else {
                  if (input$VonNeumann){ #vonneumann
                    if (vals$B[i-1,j]==2 || vals$B[i+1,j]==2 || vals$B[i,j-1]==2 || vals$B[i,j+1]==2){
                      les2[i,j] <- 2
                    }
                    else{
                      u <- runif(1)
                      if(u<=input$prob1){
                        les2[i,j] <- 2
                      }
                    }
                  }
                  else{# Moore
                    if (vals$B[i-1,j]==2 || vals$B[i+1,j]==2 || vals$B[i,j-1]==2 || vals$B[i,j+1]==2 || vals$B[i-1,j-1] || vals$B[i-1,j+1] || vals$B[i+1,j-1] || vals$B[i+1,j+1]){
                      les2[i,j] <- 2
                    }
                    else{
                      u <- runif(1)
                      if(u<=input$prob1){
                        les2[i,j] <- 2
                      }
                    }
                  }
                }
              }
            }
          }
          vals$B <-les2
        }
      })
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
  
})
