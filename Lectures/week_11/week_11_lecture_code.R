#'
#' Week 11 - Simulation Code
#'

require(tidyverse)
require(tidysynth)



# Simulate Data -----------------------------------------------------------
    
    n_periods = 20
    n_units = 10
    dat <- 
      expand_grid(time = 1:n_periods,unit = 1:n_units) %>% 
      # predictors
      mutate(x1 = rnorm(n(),mean=0,sd=.5),
             x2 = rnorm(n(),mean=0,sd=.5),
             x3 = rnorm(n(),mean=0,sd=.5),
             x4 = rnorm(n(),mean=0,sd=.5),
             x5 = rnorm(n(),mean=0,sd=.5),
             error = rnorm(n(),mean=cos(unit/n_periods),sd=.5),
             treatment = 1*(time>10)*(unit==1) ) %>% 
      # Outcome
      mutate(y = .1*time + .5*treatment*time + -2*x1 + 2*x2 + .25*x3 + -1*x4 + .5*x4*x5 + error )
    

# Plot the trends ---------------------------------------------------------

    dat %>% 
      ggplot(aes(time,y,group=factor(unit))) +
      geom_line()
    
    

# Generate the synthetic control ------------------------------------------

    synth_out <-
      
      dat %>%
      
      synthetic_control(outcome = y, 
                        unit = unit, 
                        time = time, 
                        i_unit = 1, 
                        i_time = 10, 
                        generate_placebos=T 
      ) %>%
      
      generate_predictor(time_window = 1:10,
                         # y = mean(y),
                         x1 = mean(x1, na.rm = T),
                         x2 = mean(x2, na.rm = T),
                         x3 = mean(x3, na.rm = T),
                         x4 = mean(x4, na.rm = T),
                         x5 = mean(x5, na.rm = T)) %>%
      
      generate_predictor(time_window = 10,
                         y_10 = mean(y, na.rm = T)) %>%
      # 
      generate_predictor(time_window = 5,
                         y_5 = mean(y, na.rm = T)) %>%

      generate_predictor(time_window = 2,
                         y_2 = mean(y, na.rm = T)) %>%
      
      
     
      generate_weights(optimization_window = 1:10) %>%
      
      # Generate the synthetic control
      generate_control()
    

# Explore the control -----------------------------------------------------

    synth_out %>% plot_trends()    
    
    synth_out %>% plot_differences()
    
    synth_out %>% plot_weights()    
    
    synth_out %>% grab_balance_table()
    
    synth_out %>% plot_placebos()
    
    synth_out %>% plot_mspe_ratio()

    
    