library(shiny) #required package
library(ggplot2) #needed it for visualization, didn't use tidyverse to minimize run time
library(dplyr) #needed for filtering

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("The relationship between multiple variables"), #set title

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
          sliderInput("sizepoint", #added a slider than can be used to change the size. This was the only thing on a continuous scale so I choose to use a slider
                      label = "Change the size of the points", #what we see
                      min = 1, #minimum value possible
                      max=10,  #maximum value possible
                      value=5  #default value
          ),
          selectInput("xselect", #using a drop down list felt the most appropriate for the size, length and number of variables here
                       label = "Which variable would you like to see on the X-axis?", #what we see
                       choices= c(
                         "Age"="AGE",#options, also assigned the options to their code in the data set for easier use later
                         "Is Astrology Science?"="ASTROSCI", 
                         "Money compared to parents?"="PARSOL",
                         "Number of pets"="NUMPETS",
                         "Family income"= "INCOME"),
                       selected = "AGE"), #default value
          selectInput("yselect", 
                      label = "Which variable would you like to see on the Y-axis?", #what we see
                      choices= c(
                        "Family income"= "INCOME",
                        "Is Astrology Science?"="ASTROSCI", 
                        "Money compared to parents?"="PARSOL",
                        "Number of pets"="NUMPETS",
                        "Age"="AGE"
                        ),
                      selected = "INCOME"),
          radioButtons("age25", 
                       label= "Do you want to see people younger than 25??", #radio buttons were just convenient and functional for my goals, so I used them for everything from this point on.
                       choices=c("Yes"=TRUE,
                                 "No"=FALSE),
                       selected = TRUE),
          radioButtons("colorpoint",
                       label = "What color do you want the points to be?",
                       choices=c("Black"="black",
                                 "Purple"="purple",
                                 "Orange"="orange"),
                       selected = "black")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot"),
          textOutput("distext")
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  import_tbl <- readRDS("import.RDS") #import data that will be used
  
    output$distPlot <- renderPlot({
      
      if(input$age25==FALSE)
        filter_tbl <- filter(import_tbl, AGE_25==TRUE) 
      else filter_tbl <- import_tbl
      
      if(input$yselect == "ASTROSCI") #did an if statement to define the y values used based on the selected options. If was just simple, logical and easy, the first thing that came to mind on how I could do what I wanted, quickly.
        y_val <- filter_tbl$ASTROSCI 
      else if(input$yselect == "PARSOL")
        y_val <-filter_tbl$PARSOL 
      else if(input$yselect == "AGE")
        y_val <-filter_tbl$AGE
      else if(input$yselect == "INCOME")
        y_val <-filter_tbl$INCOME
      else y_val <-filter_tbl$NUMPETS
      
      if(input$xselect == "ASTROSCI") #did an if statement to define the y values used based on the selected options. If was just simple, logical and easy, the first thing that came to mind on how I could do what I wanted, quickly.
        x_val <- filter_tbl$ASTROSCI 
      else if(input$xselect == "PARSOL")
        x_val <-filter_tbl$PARSOL 
      else if(input$xselect == "AGE")
        x_val <-filter_tbl$AGE
      else if(input$xselect == "INCOME")
        x_val <-filter_tbl$INCOME
      else x_val <-filter_tbl$NUMPETS
      
      #same comments will not be restated
      ggplot(filter_tbl,aes(x=x_val,y=y_val))+ #y is y_val, and x is x_val to have it change with the options chosen
        geom_jitter(width=0.3,height = 0.3,size=input$sizepoint,color=input$colorpoint) # allows people to change the shape and size of the points from the given options
    })
    output$distext <- renderText({
      
      if(input$age25==FALSE)
        filter_tbl <- filter(import_tbl, AGE_25==TRUE) #making a new filtered dataset to be used if someone wants to see the filtered data
      else filter_tbl <- import_tbl
      
      if(input$yselect == "ASTROSCI") #did an if statement to define the y values used based on the selected options. If was just simple, logical and easy, the first thing that came to mind on how I could do what I wanted, quickly.
        y_val <- as.numeric(filter_tbl$ASTROSCI) #because this is a factor, made it numeric so that the correlation works.
      else if(input$yselect == "PARSOL")
        y_val <-filter_tbl$PARSOL 
      else if(input$yselect == "AGE")
        y_val <-filter_tbl$AGE
      else if(input$yselect == "INCOME")
        y_val <-filter_tbl$INCOME
      else y_val <-filter_tbl$NUMPETS
      
      if(input$xselect == "ASTROSCI") #did an if statement to define the y values used based on the selected options. If was just simple, logical and easy, the first thing that came to mind on how I could do what I wanted, quickly.
        x_val <- as.numeric(filter_tbl$ASTROSCI) 
      else if(input$xselect == "PARSOL")
        x_val <-filter_tbl$PARSOL 
      else if(input$xselect == "AGE")
        x_val <-filter_tbl$AGE
      else if(input$xselect == "INCOME")
        x_val <-filter_tbl$INCOME
      else x_val <-filter_tbl$NUMPETS
      
      
      correlation<- cor.test(x=c(x_val),y=c(y_val)) #calculated correlation between selected variables, not going to repeat reasoning stated in previous file
      paste0("The correlation between ",input$xselect," and ",input$yselect," was ",round(correlation$estimate,2)) #made a sentence to be printed out that says codes of the variables being looked at and a rounded to 2 decimal place correlations, I didn't use formatc because if the correlation is less than 2 decimal places it's practically zero and that is the output that I'd like people to see.
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
