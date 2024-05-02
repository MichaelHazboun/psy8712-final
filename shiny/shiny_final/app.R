library(shiny) #required package
library(ggplot2) #needed it for visualization, didn't use tidyverse to minimize run time



# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Three potential predictors of income"), #set title

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
          sliderInput("sizepoint", #added a slider than can be used to change the size. This was the only thing on a continuous scale so I choose to use a slider
                      label = "Change the size of the points", #what we see
                      min = 1, #minimum value possible
                      max=10,  #maximum value possible
                      value=5  #default value
          ),
          radioButtons("varselect", #radio buttons were just convenient and functional for my goals, so I used them for everything from this point on.
                       label = "Which variable would you like to see?", #what we see
                       choices= c("Is Astrology Science?"="ASTROSCI", #options, also assigned the options to their code in the data set for easier use later
                                  "Money compared to parents?"="PARSOL",
                                  "Number of pets?"="NUMPETS"),
                       selected = "ASTROSCI"), #default value
          radioButtons("error", 
                       label= "Do you want to see error bars?", #practically same comments, no need to repeat 
                       choices=c("Yes"=TRUE,
                                 "No"=FALSE),
                       selected = TRUE),
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
  
  import_tbl <- readRDS("import.RDS") #import data that will be used

    output$distPlot <- renderPlot({

      if(input$varselect == "ASTROSCI") #did an if statement to define the y values used based on the selected options. If was just simple, logical and easy, the first thing that came to mind on how I could do what I wanted, quickly.
        y_val <- import_tbl$ASTROSCI 
      else if(input$varselect == "PARSOL")
        y_val <-import_tbl$PARSOL 
      else if(input$varselect == "AGE")
        y_val <-import_tbl$AGE
      else y_val <-import_tbl$NUMPETS
      
      #same comments will not be restated
      ggplot(import_tbl,aes(x=RINCOME,y=y_val))+ #y is y_val to have it change with the options chosen
        geom_jitter(width=0.3,height = 0.3,size=input$sizepoint,color=input$colorpoint)+ # allows people to change the shape and size of the points from the given options
        geom_smooth(method="lm", # the smaller sample size when I changed things to only 15K income and more was causing problems with the default method, so I switched to lm
                    se=as.logical(input$error), #had to add as.logical because the value was moving as character, this lets people remove or show the error bands as desired
                    color=input$colorline) # this allows them to change the color of the line to one of the three colors given
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
