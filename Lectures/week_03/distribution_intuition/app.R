# Goal: Build a Shiny App to offer students the 
library(shiny)
require(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simulating Random Distributions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            titlePanel("Normal"),
            sliderInput(inputId = "mu",
                        "mean (centeral tendency)",
                        min = -25,
                        max = 25,
                        value = 0),
            sliderInput(inputId = "sigma",
                        "variance (spread)",
                        min = 0,
                        max = 50,
                        value = 15)
        ),
        
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        ),
        
    ),
    
    sidebarLayout(
        sidebarPanel(
            titlePanel("Binomial"),
            sliderInput(inputId = "size",
                        "size (possible outcomes)",
                        min = 1,
                        max = 50,
                        value = 1),
            sliderInput(inputId = "prob",
                        "probability of an outcome",
                        min = 0.01,
                        max = .99,
                        value = .5)
        ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot2")
        ),
        
    ),
    
    sidebarLayout(
        sidebarPanel(
            titlePanel("Poisson"),
            sliderInput(inputId = "lambda",
                        "Lambda (the rate of occurrence)",
                        min = 0.1,
                        max = 10,
                        value = .5)
        ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot3")
        ),
        
    )
    
    
    
)

# Define server logic required to draw a density plot
server <- function(input, output) {

    output$distPlot <- renderPlot({
        set.seed(123)
            tibble(x = rnorm(5e3,input$mu,input$sigma)) %>% 
                ggplot(aes(x)) +
                geom_histogram(fill='steelblue',color='steelblue',alpha=.5) +
                geom_vline(aes(xintercept = mean(x)),color="grey30",linewidth=4) +
                geom_segment(x=(input$mu-input$sigma*2),xend=(input$sigma*2+input$mu),y=0,yend=0,
                             color="grey30",size = 2) +
                xlim(-100,100) +
                theme_bw()    
    })
        
        output$distPlot2 <- renderPlot({
            set.seed(123)
                tibble(x = rbinom(5e3,input$size,input$prob)) %>% 
                    ggplot(aes(x)) +
                    geom_histogram(fill='darkred',color='darkred',alpha=.5) +
                    theme_bw()    
        
        })
        
        
        output$distPlot3 <- renderPlot({
            set.seed(123)
            tibble(x = rpois(5e3,input$lambda)) %>% 
                ggplot(aes(x)) +
                geom_histogram(fill='darkgreen',color='darkgreen',alpha=.5) +
                theme_bw()    
            
        })
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
