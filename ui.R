#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
foo <- function() {
  message("one")
  message("two")
}
library(shiny)
library(shinythemes)
library(shinyjs)
# Define UI for application that draws a histogram

shinyUI(  
  navbarPage("FOREST FIRE",
    tabPanel("Simulation",
      fluidPage(
        shinyjs::useShinyjs(),
        tags$style(type="text/css",
                   ".recalculating {opacity: 1.0;}"), #super vec vdaka ktorej nemrzne graf
        tags$style(HTML("h1 {line-height: 1.6;color: #ffffff;font-size: 400%;
                          }
                          body {
                              background-color: #000000;
                          }")),
        # Application title
        headerPanel("SINGLE FIRE SIMULATION"),
        #shinythemes::themeSelector(),
        theme = shinytheme("cyborg"),
        br(),
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
          sidebarPanel(
            sliderInput("prob1",
                        "Probability of a tree getting hit by a lightning",
                        min = 0,
                        max = 1,
                        value = 0.005),
            sliderInput("prob2",
                        "Probability of a tree growing",
                        min = 0,
                        max = 1,
                        value = 0.05),
            sliderInput("N",
                        "Number of cycles",
                        min = 10,
                        max = 500,
                        value = 90),
            
             sliderInput("L",
                         "Size of the forest",
                         min = 10,
                         max = 200,
                         value = 80),
              selectInput("color",  "Forest color",
                        c("Green/Red"= T,
                          "Gray/White"= F
                        )),
            selectInput("VonNeumann",  "Type of neighborhood",
                        c("Von Neumann"= T,
                          "Moore"= F
                        )),
            actionButton("Start","START", width='100%'),
            br(),
            br(),
            actionButton("about","About", width='50%')
      
          ),
          mainPanel(width=8,
                    plotOutput("A",width='100%',height = '600px')
                    )

        )
               )),
    tabPanel("Confidence Interval",
             fluidPage(
               shinyjs::useShinyjs(),
               tags$style(type="text/css",
                          ".recalculating {opacity: 1.0;}"), #super vec vdaka ktorej nemrzne graf
               tags$style(HTML("h1 {line-height: 1.6;color: #ffffff;font-size: 400%;
                          }
                          body {
                              background-color: #000000;
                          }")),
               # Application title
               headerPanel("CI FOR NUMBER OF TREES"),
               #shinythemes::themeSelector(),
               theme = shinytheme("cyborg"),
               br(),
               # Sidebar with a slider input for number of bins 
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("burnin",
                               "Length of the burn-in phase",
                               min = 0,
                               max = 100,
                               value = 20),
                   sliderInput("delta",
                               "Lenght of the confidence interval",
                               min = 1,
                               max = 1000,
                               value = 50),
                   actionButton("Start2","START MC", width='100%'),
                   br(),
                   br(),
                   actionButton("about2","About MC", width='50%')
                   
                 ),
                 mainPanel(width=8)
               )
            )
    )
)
)
