#'
#' Week 6 Code: Instruments 
#'

require(tidyverse)


# Basic Simulations for the IV --------------------------------------------

    set.seed(123) # to reproduce the simulation 
    N = 1000 # Sample size
    
    # Endogeneity 
    something <- rnorm(N)
    
    # Error
    e <- rnorm(N)
    
    # instrument
    z <- rnorm(N) 
    
    # Other random variable (control of some sorts)
    x2 <- rnorm(N) 
    
    # Underlying model for the variable we care about
    x1 <- 1 + 1*z + something + rnorm(N)
    
    # "TRUE" Outcome model
    y <- 1 + -2*x1 + .5*x2 + something + e
    
    # Save as data 
    D = tibble(y,x1,x2,z)
    
    # Run normal model
    m1 <- lm(y ~ x1 + x2,data=D)
    summary(m1)
    
    # Instrumental Variables
    
    # Estimate the first stage Equation
    mod_s1 <- lm(x1 ~ z + x2,data=D)
    summary(mod_s1)
    
    # Get the predictions from the first stage model
    x1_hat <- predict(mod_s1)
    
    
    # Estimate the second stage model
    mod_s2 <- lm(y ~ x1_hat + x2,data=D)
    summary(mod_s2)
    
    # What if we included Z in the second stage model?
    mod_s2 <- lm(y ~ x1_hat + x2 + z,data=D)
    summary(mod_s2)
    
    # Z and X1 hat are perfectly collinear.
    plot(D$z,x1_hat)
    
    
    # Package: AER for ivreg
    mod_2sls <- AER::ivreg(y ~ x1 + x2 | z + x2,data=D)
    summary(mod_2sls)


# Violating the Inclusion Condition (Weak Instrument)
    
    n_sims = 500
    output = c()
    for (i in 1:n_sims){
      N = 1000
      something <- rnorm(N) # Endogeneity 
      z <- rnorm(N) # instrument
      x2 <- rnorm(N) # control
      x1 <- 1 + .05*z + 2*something + rnorm(N) # Key Endogenous Indep. Var
      y <- 1 + -2*x1 + x2 + 2*something + rnorm(N) # "TRUE" Outcome model
      D = tibble(y,x1,x2,z) 
      
      # Run model
      mod_ols <- lm(y ~ x1 + x2,data=D)
      mod_2sls <- AER::ivreg(y ~ x1 + x2 | z + x2,data=D)
      output <- bind_rows(output,
                          broom::tidy(mod_ols) %>% mutate(type="ols",sim_id = i),
                          broom::tidy(mod_2sls) %>% mutate(type="2sls",sim_id = i)
      )
    }
    
    # Visualize
    output %>% 
      filter(term =="x1") %>% 
      ggplot(aes(estimate,fill=type)) +
      geom_histogram(position="identity",bins = 50,alpha=.5,color='white') +
      geom_vline(xintercept = -2,color="darkred",lty=2,size=1) +
      xlim(-5,5) +
      theme_minimal()
    
    
    # What happens when we decrease/increase the sample size?
    


# Violating the Exclusion Condition
    
    n_sims = 500
    output = c()
    for (i in 1:n_sims){
      N = 1000
      something <- rnorm(N) # Endogeneity 
      z <- rnorm(N) # instrument
      x2 <- rnorm(N) # control
      x1 <- 1 + z + 2*something + rnorm(N) # Key Endogenous Indep. Var
      y <- 1 + -2*x1 + x2 + 2*z + 2*something + rnorm(N) # "TRUE" Outcome model
      D = tibble(y,x1,x2,z) 
      
      # Run model
      mod_ols <- lm(y ~ x1 + x2,data=D)
      mod_2sls <- AER::ivreg(y ~ x1 + x2 | z + x2,data=D)
      output <- bind_rows(output,
                          broom::tidy(mod_ols) %>% mutate(type="ols",sim_id = i),
                          broom::tidy(mod_2sls) %>% mutate(type="2sls",sim_id = i)
      )
    }
    
    # Visualize
    output %>% 
      filter(term =="x1") %>% 
      ggplot(aes(estimate,fill=type)) +
      geom_histogram(position="identity",bins = 50,alpha=.5,color='white') +
      geom_vline(xintercept = -2,color="darkred",lty=2,size=1) +
      theme_minimal()


# Quasi-Instruments (Exclusion condition not really satisfied)
    
    n_sims = 500
    output = c()
    for (i in 1:n_sims){
      N = 1000
      something <- rnorm(N) # Endogeneity 
      z <- rnorm(N) # instrument
      x2 <- rnorm(N) # control
      x1 <- 1 + z + 2*something + rnorm(N) # Key Endogenous Indep. Var
      y <- 1 + -2*x1 + x2 + .1*z + 2*something + rnorm(N) # "TRUE" Outcome model
      D = tibble(y,x1,x2,z) 
      
      # Run model
      mod_ols <- lm(y ~ x1 + x2,data=D)
      mod_2sls <- AER::ivreg(y ~ x1 + x2 | z + x2,data=D)
      output <- bind_rows(output,
                          broom::tidy(mod_ols) %>% mutate(type="ols",sim_id = i),
                          broom::tidy(mod_2sls) %>% mutate(type="2sls",sim_id = i)
      )
    }
    
    output %>% 
      filter(term =="x1") %>% 
      ggplot(aes(estimate,fill=type)) +
      geom_histogram(position="identity",bins = 50,alpha=.5,color='white') +
      geom_vline(xintercept = -2,color="darkred",lty=2,size=1) +
      theme_minimal()
    

# Multiple Instruments
    
    
    n_sims = 500
    output = c()
    for (i in 1:n_sims){
      N = 1000
      something <- rnorm(N) # Endogeneity 
      z1 <- rnorm(N) # instrument 1
      z2 <- rnorm(N) # instrument 2
      z3 <- rnorm(N) # instrument 3
      x2 <- rnorm(N) # control
      x1 <- 1 + z1 + z2 + z3 + 2*something + rnorm(N) # Key Endogenous Indep. Var
      y <- 1 + -2*x1 + x2 + 2*something + rnorm(N) # "TRUE" Outcome model
      D = tibble(y,x1,x2,z1,z2,z3) 
      
      # Run model
      mod_ols <- lm(y ~ x1 + x2,data=D)
      mod_2sls <- AER::ivreg(y ~ x1 + x2 | z1 + z2 + z3 + x2,data=D,model = TRUE)
      output <- bind_rows(output,
                          broom::tidy(mod_ols) %>% mutate(type="ols",sim_id = i),
                          broom::tidy(mod_2sls) %>% mutate(type="2sls",sim_id = i)
      )
    }
    
    output %>% 
      filter(term =="x1") %>% 
      ggplot(aes(estimate,fill=type)) +
      geom_histogram(position="identity",bins = 50,alpha=.5,color='white') +
      geom_vline(xintercept = -2,color="darkred",lty=2,size=1) +
      theme_minimal()
    
    
    # Tests
    summary(mod_2sls,diagnostics = TRUE)
    # Weak instrument == F test (stat sig. == instruments are not weak)
    
    # Wu-Hausman is a test for consistency with ols. Test for whether ols and
    # 2sls are similar. when rejected it means that ols differs from 2sls
    # (implying the ols model is biased)
    
    # Sargan is a test for exogeneity using overidentification. When rejected, at least one iv is invalid
    
    
    # Examining the first stage:
    summary(lm(x1 ~ z1 + z2 + z3 + x2,data=D))
    
    
    # Questions:
      # What happens if one of the instruments doesn't satisfy the inclusion condition?
      # What happens if all the instruments are weak instruments?
      # What happens if one of the instruments doesn't satisfy the exclusion condition?
    


