# Convert Student Vote Data
require(tidyverse)

# For summary and lecture
dat = haven::read_dta('/Users/edunford/Desktop/student vote GVPT 729a.dta')
dat %>% 
  transmute(warsup = names(attr(dat$warsup,"labels"))[warsup],
            dem = ifelse(partyid<=3,1,0),
            female) %>% 
  tidyr::drop_na(warsup) %>% 
  dplyr::sample_n(500) %>% 
  write_csv(here::here('Lectures/week_05/summaries/student_vote.csv'))




