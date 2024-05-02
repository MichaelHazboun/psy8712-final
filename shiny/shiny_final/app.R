setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(shiny)
library(ggplot2)

import_tbl <- readRDS("import.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Final Shiny Project"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
          sliderInput("sizepoint",
                      label = "Change the size of the points",
                      min = 1,
                      max=10,
                      value=5
          ),
          radioButtons("varselect",
                       label = "Which variable would you like to see?",
                       choices= c("Is Astrology Science?"="ASTROSCI",
                                  "Money compared to parents?"="PARSOL",
                                  "Number of pets?"="NUMPETS"),
                       selected = "ASTROSCI"),
          radioButtons("error",
                       label= "Do you want to see error bars?",
                       choices=c("Yes"=TRUE,
                                 "No"=FALSE),
                       selected = TRUE),
          radioButtons("fifteen_k", #could change this to do I want the line to be lm or something else...
                       label = "Do you want to see the people that made less than 15k?",
                       choices=c("Yes","No"),
                       selected = "Yes"),
          radioButtons("colorline",
                       label = "What color do you want the line to be?",
                       choices=c("Red"="red",
                                 "Blue"="blue",
                                 "Green"="green"),
                       selected = "red"),
          radioButtons("colorpoint",
                       label = "What color do you want the points to be?",
                       choices=c("Black"="black",
                                 "Purple"="purple",
                                 "Orange"="orange"),
                       selected = "black")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
    filtered_tbl <- import_tbl
      if(input$fifteen_k == "No")  
      filtered_tbl <- filter(import_tbl, Income_10==TRUE) else filtered_tbl <- import_tbl
    if(input$varselect == "ASTROSCI")
      y_val <- filtered_tbl$ASTROSCI 
    else if(input$varselect == "PARSOL")
      y_val <-filtered_tbl$PARSOL 
    else y_val <-filtered_tbl$NUMPETS
 
    size_thing <- input$sizepoint
      
        #same comments will not be restated
        ggplot(filtered_tbl,aes(x=INCOME,y=y_val))+ 
        geom_jitter(width=0.3,height = 0.3,size=input$sizepoint,color=input$colorpoint)+ 
        geom_smooth(method="lm",
                    se=as.logical(input$error),
                    color=input$colorline)

            })
}

# Run the application 
shinyApp(ui = ui, server = server)
