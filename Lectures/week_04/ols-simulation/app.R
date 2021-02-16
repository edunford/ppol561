# Simulate an ols model

require(shiny)
require(tidyverse)

ui <- fluidPage(

    # Application title
    titlePanel("OLS Simulation"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("beta",
                        "Coefficient",
                        min = -5,
                        max = 5,
                        value = 1),
            sliderInput("error_mean",
                        "Mean Error",
                        min = -5,
                        max = 5,
                        value = 0),
            sliderInput("error_var",
                        "Error Variance",
                        min = 0,
                        max = 10,
                        value = 1)
        ),
        

        # Show a plot 
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        set.seed(123)
        
        n = 1000
        
        error <- rnorm(n,mean = input$error_mean,sd = input$error_var)
        
        x <- rnorm(n)
        
        intercept = 1
        slope = input$beta
        
        # Simulate y as a function of x1 + error
        y = intercept + slope*x + error
        
        tibble(y,x) %>% 
            ggplot(aes(x,y))+
            geom_point(alpha=.4,color="grey30",size=2) +
            geom_smooth(method='lm') +
            geom_abline(intercept = 1,slope=input$beta,color='darkred',size=2,alpha=.4) + 
            theme_light() + 
            labs(title="Red line is the 'true' effect")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
