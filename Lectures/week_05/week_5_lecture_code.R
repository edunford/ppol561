#' 
#' Week 5 -- FE and DID 
#' 
require(tidyverse)


# Simulating Clusters -----------------------------------------------------

    # Simulate simple random var
    N = 1000
    tibble(x = rnorm(N)) %>% 
      ggplot(aes(x)) +
      geom_histogram(bins=50,alpha=.5)
    
    
    # Simulate clusters with no differences
    N = 1000
    clust_dat <- 
      tibble(group = c(rep("A",500),rep("B",500)),
             x = rnorm(N)) 
    
    clust_dat %>% 
      ggplot(aes(x,fill=group)) +
      geom_histogram(bins=50,alpha=.5)
    
    
    # Simulate clusters with differences
    N = 1000
    clust_dat <- 
      tibble(group = c(rep("A",500),rep("B",500)),
             group_mean = ifelse(group=="A",-3,3),
             x = rnorm(N,mean=group_mean)) 
    
    clust_dat %>% 
      ggplot(aes(x,fill=group)) +
      geom_histogram(bins=50,alpha=.5)
    
    

# Simulating Simpson's Paradox --------------------------------------------

    set.seed(1234)                            # set seed to replicate
    N_cities = 50                             # No. of Fake cities
    city = paste0("city",1:N_cities)          # City name ID
    city_effect = rnorm(N_cities,             # Baseline mean for 
                        mean = 20,sd = 5)     # each city
    year = 1990:2000                          # Fake year range
    D = expand.grid(city=city,year=year)      # Combine city-years
    D = left_join(D,tibble(city,city_effect)) # merge data
    D = as_tibble(D) %>% arrange(city)
    D
    
    N = nrow(D) # N obs (city-year)
    x = rnorm(N,mean=D$city_effect)  # mean of x conditional on city
    error = rnorm(N,mean=D$city_effect,sd=3) # mean of y conditional on city
    true_effect <- -.5 # population effect
    y = 1 + true_effect*x + error # true model
    D$crime <- y; D$police <- x # create variables 
    
    # Look at the naive relationship
    ggplot(D,aes(police,crime)) +
      geom_point(alpha=.4,size=4) + 
      geom_smooth(method="lm",se=F) +
      theme(legend.position = "none")
    
    # Look at the relationship by city
    ggplot(D,aes(police,crime,color=factor(city))) +
      geom_point(alpha=.4,size=4) + 
      geom_smooth(method="lm",se=F) +
      theme(legend.position = "none")
    
    # Estimate model
    broom::tidy(lm(crime~police,data=D))
    

# Removing the confounder: including city-specific fixed effects! -------------------------------------------------
    
    # Remove city specific effects: control for confounder
    D$police_star <- resid(lm(police ~ factor(city),data=D))
    D$crime_star <- resid(lm(crime ~ factor(city),data=D))
    
    # Look at the adjusted relationship
    ggplot(D,aes(police_star,crime_star)) +
      geom_point(alpha=.4,size=4) + 
      geom_smooth(method="lm",se=F) +
      theme(legend.position = "none")
    
    # Estimate model
    lm(crime_star~police_star,data=D) %>% 
      broom::tidy() %>% 
      mutate_if(is.numeric,function(x) round(x,2))
    
    
    # Fixed effects
    lm(crime~police+ factor(city),data=D) %>% 
      broom::tidy() %>% 
      mutate_if(is.numeric,function(x) round(x,2))
    

# Demeaning Approach ---------------------------------------------------------------

    D <- 
      D %>% 
      group_by(city) %>% 
      mutate(crime_demeaned = crime - mean(crime),
             police_demeaned = police - mean(police)) %>% 
      ungroup
    
    lm(crime_demeaned~police_demeaned,data = D) %>% 
      broom::tidy() %>% 
      mutate_if(is.numeric,function(x) round(x,2))
    
    
    # Using plm (which implements the demeaning approach)
    plm::plm(crime~police,data=D,index="city") %>% summary
    
    
# Monte Carlo Simulation: Comparing Results -------------------------------

    # Function to generate data.
    gen_data <- function(true_effect = -.5,N_cities = 50,N_years=10){
      city = paste0("city",1:N_cities)          # City name ID
      city_effect = rnorm(N_cities,             # Baseline mean for 
                          mean = 20,sd = 5)     # each city
      year = 1990:(1990 + N_years)              # Fake year range
      D = expand.grid(city=city,year=year)      # Combine city-years
      D = left_join(D,tibble(city,city_effect),by="city") # merge data
      N = nrow(D) # N obs (city-year)
      x = rnorm(N,mean=D$city_effect)  # mean of x conditional on city
      error = rnorm(N,mean=D$city_effect,sd=1) # mean of y conditional on city
      y = 1 + true_effect*x + error # true model
      D$crime <- y; D$police <- x # create variables 
      return(as_tibble(D))
    }
    
    # Run simulations
    n_sims = 500
    dat_out <- data.frame(id = 1:n_sims,
                          orig = NA,
                          fe_dummy = NA,
                          fe_demean = NA)
    for( i in 1:n_sims){
      dat <- gen_data() # Generate data
      
      # Original
      m1 <- lm(crime ~ police, data = dat)
      
      # FE w/ Dummy
      m2 <- lm(crime ~ police + factor(city), data = dat)
      
      # FE via demean
      dat %>% 
        group_by(city) %>% 
        mutate(crime_demeaned = crime - mean(crime),
               police_demeaned = police - mean(police)) %>% 
        lm(crime_demeaned ~ police_demeaned, data = .) -> m3
      
      # Store values
      dat_out$orig[i] = coef(m1)[2]
      dat_out$fe_dummy[i] = coef(m2)[2]
      dat_out$fe_demean[i] = coef(m3)[2]
    }
    
    
    # Visualize
    dat_out %>% 
      pivot_longer(cols = c(orig,fe_dummy,fe_demean)) %>% 
      ggplot(aes(x=value,fill=name)) +
      geom_histogram(bins = 100,alpha=.5,color="white") +
      geom_vline(xintercept = -.5,lty=2,color="grey30") +
      theme_minimal() 
    
    

# When errors differ by group ---------------------------------------------

        
    # Function to generate data.
    gen_data2 <- function(true_effect = -.5,N_cities = 50,N_years=10){
      city = paste0("city",1:N_cities)          # City name ID
      city_effect = rnorm(N_cities,mean = 20,sd = 5)    
      city_error = runif(N_cities,min = 1,max=10)   
      year = 1990:(1990 + N_years)              # Fake year range
      D = expand.grid(city=city,year=year)      # Combine city-years
      D = left_join(D,tibble(city,city_effect,city_error),by="city") # merge data
      N = nrow(D) # N obs (city-year)
      x = rnorm(N,mean=D$city_effect,sd =1)  # mean of x conditional on city
      error = rnorm(N,mean=D$city_effect,sd=D$city_error) # mean of y conditional on city
      y = 1 + true_effect*x + error # true model
      D$crime <- y; D$police <- x # create variables 
      return(as_tibble(D))
    }
    
    # Run simulations
    n_sims = 500
    dat_out2 <- data.frame(id = 1:n_sims,
                           orig = NA,
                           fe_dummy = NA,
                           fe_demean = NA)
    for( i in 1:n_sims){
      dat <- gen_data2() # Generate data
      
      # Original
      m1 <- lm(crime ~ police, data = dat)
      
      # FE w/ Dummy
      m2 <- lm(crime ~ police + factor(city), data = dat)
      
      # FE via demean
      dat %>% 
        group_by(city) %>% 
        mutate(crime_demeaned = crime - mean(crime),
               police_demeaned = police - mean(police)) %>% 
        lm(crime_demeaned ~ police_demeaned, data = .) -> m3
      
      # Store values
      dat_out2$orig[i] = coef(m1)[2]
      dat_out2$fe_dummy[i] = coef(m2)[2]
      dat_out2$fe_demean[i] = coef(m3)[2]
    }
    
    
    # Visualize
    bind_rows(
      dat_out %>% 
        pivot_longer(cols = c(orig,fe_dummy,fe_demean))  %>% 
        mutate(type = "Consistent error"),
      dat_out2 %>% 
        pivot_longer(cols = c(orig,fe_dummy,fe_demean)) %>% 
        mutate(type = "Var in error")
    )  %>% 
      ggplot(aes(x=value,fill=type)) +
      geom_histogram(bins = 50,alpha=.50,color="white") +
      geom_vline(xintercept = -.5,lty=2,color="grey30") +
      theme_minimal() +
      facet_wrap(~name,ncol=1,scales="free")
    
    # What's going on here?
    
    

# Difference-in-Difference Simulations ------------------------------------
    
    
    # Basic setup...  
    units = 10
    time = 10
    
    # Control Group Data Generating Process
    d1 <- 
      crossing(tibble(id = 1:units),
               tibble(time = 1:time)) %>% 
      mutate(treat_group = 0,
             policy = ifelse(time > 5,1,0),
             y = sin(time) + rnorm(n()))
    
    # Treatment Group Data Generating Process
    d2 <- 
      crossing(tibble(id = (1:units) + 1000),
               tibble(time = 1:time)) %>% 
      mutate(treat_group = 1,
             policy = ifelse(time > 5,1,0),
             y = sin(time) + .5*policy + rnorm(n()))
    D = bind_rows(d1,d2)
    
    # models 
    lm(y ~ policy, data = D) %>% summary()
    lm(y ~ treat_group*policy, data = D) %>% summary()
  
    
    
    
    
    # Monte Carlo Simulation
    n_sims = 500
    out_did = rep(NA,n_sims)
    out_naive = rep(NA,n_sims)
    for( i in 1:n_sims){
      units = 10
      time = 10
      d1 <- 
        crossing(tibble(id = 1:units),
                 tibble(time = 1:time)) %>% 
        mutate(treat_group = 0,
               policy = ifelse(time > 5,1,0),
               y = sin(time) + policy + rnorm(n()))
      
      d2 <- 
        crossing(tibble(id = (1:units) + 1000),
                 tibble(time = 1:time)) %>% 
        mutate(treat_group = 1,
               policy = ifelse(time > 5,1,0),
               y = sin(time) + policy + .5*policy*treat_group + rnorm(n()))
      D = bind_rows(d1,d2)
      
      m1 <- lm(y ~ policy, data = D)
      m2 <- lm(y ~ treat_group*policy, data = D)
      out_naive[i] <- coef(m1)[2]
      out_did[i] <- coef(m2)[4]
    }
    
    hist(out_naive,xlim=c(-2,2),col=scales::alpha("grey30",.5),border="white",main = "")
    hist(out_did,col=scales::alpha("steelblue",.5),border="white",add=T)
    abline(v=.5,lty=2)
    
    # Plot Trends
    D %>% 
      ggplot(aes(time,y,color=factor(treat_group))) +
      geom_point()+
      geom_smooth()
    
    

# Violating Parallel Trends Assumption -------------------------------------------------

    
    # Monte Carlo Simulation
    n_sims = 500
    out_did = rep(NA,n_sims)
    out_naive = rep(NA,n_sims)
    for( i in 1:n_sims){
      units = 10
      time = 10
      d1 <- 
        crossing(tibble(id = 1:units),
                 tibble(time = 1:time)) %>% 
        mutate(treat_group = 0,
               policy = ifelse(time > 5,1,0),
               y = sin(time) + policy + rnorm(n()))
      
      d2 <- 
        crossing(tibble(id = (1:units) + 1000),
                 tibble(time = 1:time)) %>% 
        mutate(treat_group = 1,
               policy = ifelse(time > 5,1,0),
               y = -3*sin(time) + policy + .5*policy*treat_group + rnorm(n()))
      D = bind_rows(d1,d2)
      
      m1 <- lm(y ~ policy, data = D)
      m2 <- lm(y ~ treat_group*policy, data = D)
      out_naive[i] <- coef(m1)[2]
      out_did[i] <- coef(m2)[4]
    }
    
    # plot estimates
    hist(out_naive,xlim=c(-2,2),col=scales::alpha("grey30",.5),border="white",main = "")
    hist(out_did,col=scales::alpha("steelblue",.5),border="white",add=T)
    abline(v=.5,lty=2)
    
    # Plot Trends
    D %>% 
      ggplot(aes(time,y,color=factor(treat_group))) +
      geom_point()+
      geom_smooth()
    
    
    
    m = lm(y ~ I(time == 3)*policy + I(time == 4)*policy + I(time == 5)*policy, 
             # I(time == 6)*policy + I(time == 7)*policy + I(time == 8)*policy , 
           data = D)
    summary(m)
    