#'
#' Week 8: Regression Discontinuity 
#'

require(tidyverse)


# Simulating RDD  ---------------------------------------------------------

    N = 1000
    something = rnorm(N)
    x <- something + rnorm(N)
    tr <- 1*(x > 1)
    y <- tr + something + rnorm(N)
    D = tibble(y,x)
    
    # Visualize
    D %>% 
      ggplot(aes(x,y)) +
      geom_point()
    
    # Model
    m <- lm(y ~ x,data = D)
    summary(m)
    

    # RDD
    cutoff <- 1
    lm(y ~ I(1*(x >= cutoff)) + I(x - cutoff),data = D) %>% 
      summary

    # Visualize RDD
    D %>% 
      ggplot(aes(x-cutoff,y,color=factor(x >= cutoff))) +
      geom_point(show.legend = F,alpha=.5) +
      geom_vline(xintercept=0,color="grey30",lty=2) +
      geom_smooth(method = "lm",se = F,show.legend = F,size=2) +
      theme(text=element_text(size=20))
    
    
  
# If Something else also jumps at the error term ----------------------------------------

    N = 1000
    something = rnorm(N)
    x <- something + rnorm(N)
    tr <- 1*(x >= 1)
    z <- tr + rnorm(N)
    y <- tr + (z + something + rnorm(N))
    D = tibble(y,x,z)
    
    # RDD
    cutoff <- 1
    lm(y ~ I(1*(x >= cutoff)) + I(x - cutoff),data = D) %>% 
      summary
    
    # Check
    lm(z ~ I(1*(x >= cutoff)) + I(x - cutoff),data = D) %>% 
      summary
    
    

# Placebo Tests -----------------------------------------------------------

        
    N = 1000
    something = rnorm(N)
    x <- something + rnorm(N)
    tr <- 1*(x > 1)
    y <- 3*tr + something + rnorm(N,sd=10)
    D = tibble(y,x)
    
    # Check different cutoffs
    output = c()
    for ( i in -2:2){
      lm(y ~ I(1*(x >= i)) + I(x - i),data = D) %>% 
        broom::tidy(.) %>% slice(2) %>% 
        select(estimate,statistic) %>% 
        mutate(cutoff = i) %>% 
        bind_rows(.,output) -> output  
    }
    output
    
    