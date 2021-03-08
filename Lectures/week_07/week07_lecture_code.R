#'
#'  Week 7: Experiments
#'  Simulation Code
#'

require(tidyverse)


# Simulate a randomized experiment ----------------------------------------
    
    N = 1000
    A <- rnorm(N)  # Assignment Variable 
    tr <- 1*(A > 0) # Treatment (assignment into the T or C group depends on A)
    b1 = 1 # Treatment effect
    y <- 1 + b1*tr + rnorm(N)
    
    # Run model
    m <- lm(y ~ tr)
    summary(m)
    
    

# Simulate Imbalance ------------------------------------------------------


    N = 1000
    A <- rnorm(N)  # Assignment Variable 
    x <- rbinom(N,size=1,prob = .5)
    tr <- 1*(A > 0) # Treatment (assignment into the T or C group depends on A)
    b1 = 1 # Treatment effect
    y <- 1 + b1*tr + 2*x + rnorm(N)
    D = tibble(y,tr,x)
    
    # Imbalance as a function of random sampling.
    D_imb <- 
      bind_rows(
        D %>% filter(tr == 1, x == 1) %>% sample_frac(1),
        D %>% filter(tr == 1, x == 0) %>% sample_frac(.5),
        D %>% filter(tr == 0, x == 1) %>% sample_frac(1),
        D %>% filter(tr == 0, x == 0) %>% sample_frac(1),
      )
    
    # Visualize the imbalance
    D_imb %>% 
      ggplot(aes(factor(x))) +
      geom_bar()
    
    # Run model
    m <- lm(y ~ tr,data=D_imb)
    summary(m)
    
    
    # Monte Carlo Simulation
    n_sims <- 200
    orig_est <- rep(NA,n_sims)
    for ( i in 1:n_sims){
      N = 1000
      A <- rnorm(N)  # Assignment Variable 
      x <- rbinom(N,size=1,prob = .5)
      tr <- 1*(A > 0) # Treatment (assignment into the T or C group depends on A)
      b1 = 1 # Treatment effect
      y <- 1 + b1*tr + 2*x + rnorm(N)
      D = tibble(y,tr,x)
      
      # Imbalance as a function of random sampling.
      D_imb <- 
        bind_rows(
          D %>% filter(tr == 1, x == 1) %>% sample_frac(1),
          D %>% filter(tr == 1, x == 0) %>% sample_frac(.5),
          D %>% filter(tr == 0, x == 1) %>% sample_frac(1),
          D %>% filter(tr == 0, x == 0) %>% sample_frac(1),
        )
      
      # Run model
      m <- lm(y ~ tr,data=D_imb)
      orig_est[i] = broom::tidy(m) %>% filter(term=="tr") %>% pull("estimate")
    }
    
    # Visualize
    tibble(est=orig_est) %>% 
      ggplot(aes(est)) +
      geom_histogram(bin=30,alpha=.5) +
      geom_vline(xintercept = b1,lty=2) +
      xlim(0,2)
    
    
    # Imbalance checks
    lm(x ~ tr,data=D_imb) %>% summary()
    
    # Correction? Include the variable
    m <- lm(y ~ tr + x,data=D_imb)
    summary(m)
    

# Simulate Non-Compliance -----------------------------------------------------

    N = 1000
    A <- rnorm(N)  # Assignment Variable 
    C <- rbinom(N,size=1,prob=.5) # Compliance
    tr <- 1*(A*C > 0) # Treatment (assignment into the T or C group depends on A)
    b1 = 1 # Treatment effect
    y <- 1 + b1*tr + C + rnorm(N)
    D = tibble(y,tr,x,C)
    
    # Run model 
    m <- lm(y ~ tr,data=D)
    summary(m)
    
    
    # ITT Correction
    m <- lm(y ~ A,data=D)
    summary(m)
    
    
    # 2SLS Correction
    m_2sls <- AER::ivreg(y ~ tr | A, data = D)
    summary(m_2sls)

    
    # Monte Carlo Simulation
    n_sims <- 200
    orig_tr_est <- rep(NA,n_sims)
    itt_tr_est <- rep(NA,n_sims)
    iv_tr_est <- rep(NA,n_sims)
    for ( i in 1:n_sims){
      N = 1000
      A <- rnorm(N)  # Assignment Variable 
      C <- rbinom(N,size=1,prob=.5) 
      tr <- 1*(A*C > 0) # Treatment (assignment into the T or C group depends on A)
      b1 = 1 # Treatment effect
      y <- 1 + b1*tr + C + rnorm(N)
      D = tibble(y,tr,x,C)
      
      # Run model 
      orig_tr_est[i] <- lm(y ~ tr,data=D) %>% broom::tidy() %>% filter(term == 'tr') %>% pull('estimate')
      
      # ITT Correction
      itt_tr_est[i] <- lm(y ~ A,data=D) %>% broom::tidy() %>% filter(term == 'A') %>% pull('estimate')
      
      # 2SLS Correction
      iv_tr_est[i] <- AER::ivreg(y ~ tr | A, data = D) %>% broom::tidy() %>% filter(term == 'tr') %>% pull('estimate') %>%  .[1]
    }
    
    # Visualize
    bind_rows(
      tibble(est=orig_tr_est,type="ols"),
      tibble(est=itt_tr_est,type="itt"),
      tibble(est=iv_tr_est,type="2SLS")
    ) %>% 
      ggplot(aes(est,fill=type)) +
      geom_histogram(position="identity",alpha=.5,color="white",bin=100) +
      geom_vline(xintercept = b1,lty=2,color="darkred") +
      geom_vline(xintercept = 0,lty=3,color="grey30") +
      theme_bw()
    
    

# Simulate Attrition ---------------------------------------------------------------

    N = 1000
    A <- rnorm(N)  # Assignment Variable 
    tr <- 1*(A > 0) # Treatment (assignment into the T or C group depends on A)
    z <- 1*(rnorm(N) > 0)
    stay <- 1*((tr + z) > 0)
    b1 = 1 # Treatment effect
    y <- 1 + b1*tr + z + rnorm(N)
    D = tibble(y,tr,x,z,stay) %>% 
      filter(stay == 1)
    
    
    # Run model 
    m <- lm(y ~ tr,data=D)
    summary(m)
    
    
    # Monte Carlo Simulation
    n_sims <- 200
    orig_est <- rep(NA,n_sims)
    for ( i in 1:n_sims){
      N = 1000
      A <- rnorm(N)  # Assignment Variable 
      tr <- 1*(A > 0) # Treatment (assignment into the T or C group depends on A)
      z <- 1*(rnorm(N) > 0)
      stay <- 1*((tr + 3*z + rnorm(N)) > 0)
      b1 = 1 # Treatment effect
      y <- 1 + b1*tr + z + rnorm(N)
      D = tibble(y,tr,x,z,stay) %>% 
        filter(stay == 1)
      
      # Run model 
      m <- lm(y ~ tr,data=D)
      
      # Store result
      orig_est[i] = broom::tidy(m) %>% filter(term=="tr") %>% pull("estimate")
    }
    
    # Visualize
    tibble(est=orig_est) %>% 
      ggplot(aes(est)) +
      geom_histogram(bins=30,alpha=.5) +
      geom_vline(xintercept = b1,lty=2) 
    
    
    # Check for attrition. 
    lm(stay ~ tr,data = D) %>% summary()
    lm(stay ~ tr*z,data = D) %>% summary()
    
    
    
    # Correction 1: Multivariate regression 
    lm(y ~ tr + z,data=D) %>% summary()
    
    # Correction 2: Trimming
    set.seed(1234)
    bind_rows(
      D %>% filter(tr == 1,z == 1) %>% sample_n(50),
      D %>% filter(tr == 1,z == 0) %>% sample_n(50),
      D %>% filter(tr == 0,z == 1) %>% sample_n(50),
      D %>% filter(tr == 0,z == 0) %>% sample_n(50),
    ) %>% 
      lm(y ~ tr,data=.) %>% 
      summary()
    
    # Correction 3: Selection model (talk about this week 14)
    
    
    