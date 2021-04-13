# Week 14: Synthetic Control (Example)
# Proposition 99 Example (Abadie et al 2010)


# devtools::install_github("edunford/tidysynth")

require(tidyverse)
require(tidysynth)




# Data --------------------------------------------------------------------

smoking <- read_csv("data/smoking.csv")




# Plot California's trend relative to the other states --------------------

smoking %>% 
  mutate(type = ifelse(state=="California","California","Rest of the US")) %>% 
  group_by(type,year) %>% 
  summarize(cigsale = mean(cigsale)) %>% 
  ggplot(aes(year,cigsale,linetype=type)) +
  geom_vline(xintercept = 1988,alpha=.75,color="darkred") +
  geom_line(size=1) +
  theme_minimal() +
  labs(y="per-capita cigarette sales (in packs)",linetype="") +
  ylim(0,150) +
  theme(text = element_text(size=16))
  




# Implement the Synthetic Control method ----------------------------------


smoking_out <-
  smoking %>%
  
  # Initial specification of the method
  synthetic_control(outcome = cigsale,
                    unit = state,
                    time = year,
                    i_unit = "California",
                    i_time = 1988) %>%
  
  # Generate the aggregate predictors used to generate the weights
  generate_predictor(time_window=1980:1988,
                     lnincome = mean(lnincome, na.rm = T),
                     retprice = mean(retprice, na.rm = T),
                     age15to24 = mean(age15to24, na.rm = T)) %>%
  
  generate_predictor(time_window=1984:1988,
                     beer = mean(beer, na.rm = T)) %>%
  
  generate_predictor(time_window=1975,
                     cigsale_1975 = cigsale) %>%
  
  generate_predictor(time_window=1980,
                     cigsale_1980 = cigsale) %>%
  
  generate_predictor(time_window=1988,
                     cigsale_1988 = cigsale) %>%
  
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window =1970:1988,
                   Margin.ipop=.01,Sigf.ipop=7,Bound.ipop=6) %>%
  
  # Generate the synthetic control
  generate_control()



rel_window = 1970:2000
smoking_out %>% plot_trends(time_window = rel_window) + ylim(0,150)
smoking_out %>% plot_differences(time_window = rel_window) #+ ylim(-25,25)
smoking_out %>% plot_weights()
smoking_out %>% plot_placebos(time_window = rel_window,prune=F) # ylim(-50,50)
smoking_out %>% plot_rmspe_ratio(time_window = rel_window)

# grab functions
smoking_out %>% grab_signficance(time_window = rel_window) %>% select(unit_name,fishers_exact_pvalue,z_score)
smoking_out %>% grab_balance_table()
smoking_out %>% grab_outcomes()
smoking_out %>% grab_predictors(type = "controls")



# In-Time Placebo ---------------------------------------------------------

smoking_out_placebo <-
  smoking %>%
  
  # Initial specification of the method
  synthetic_control(outcome = cigsale,
                    unit = state,
                    time = year,
                    i_unit = "California",
                    i_time = 1983) %>%
  
  # Generate the aggregate predictors used to generate the weights
  generate_predictor(time_window=1975:1983,
                     lnincome = mean(lnincome, na.rm = T),
                     retprice = mean(retprice, na.rm = T),
                     age15to24 = mean(age15to24, na.rm = T)) %>%
  generate_predictor(time_window=1970,cigsale_1975 = cigsale) %>%
  generate_predictor(time_window=1975,cigsale_1975 = cigsale) %>%
  generate_predictor(time_window=1980,cigsale_1980 = cigsale) %>%
  generate_predictor(time_window=1983,cigsale_1983 = cigsale) %>%
  
  # Generate weights and controls
  generate_weights(optimization_window =1970:1983) %>%
  generate_control()


smoking_out_placebo %>% plot_trends(time_window = rel_window) + ylim(0,150)
