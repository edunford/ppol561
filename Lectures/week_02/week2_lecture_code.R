#' 
#' **PPOL561 - Week 2 - Data Wrangling, Visualization, and Presentation**
#' 
#' Lecture code walkthrough. 
#' 

require(tidyverse)

# Data Wrangling  ---------------------------------------------------------

    # Presidential Data Frame
    dat <- presidential
    dat

# Selecting Variables -----------------------------------------------------

    # Allows us to select variables 
    select(dat,name,start,party)

# Selecting + Renaming + Reordering Variables ----------------------------------------------------

    # Reorder Variables in Data Frame
    select(dat,party,name,start,end)
    
    # Rename variables on the fly
    select(dat,political_party = party, president = name)
    
    # NOTE there is also a rename() function
    rename(dat,political_party = party)
    
    # Reordering via slice operator 
    select(dat,name:end)


# Dropping Variables -----------------------------------------------------

    # Drop 1 variable
    select(dat,-start)
    
    # Dropping multiple variables
    select(dat,-start,-end)
    select(dat,-c(start,end))

    
# Selection Helper Functions ----------------------------------------------

    # contains() - extract columns with a specific naming convention.
    
    select(dat,contains("a"))
    
    # starts_with() - extract columns that start with a specific naming convention.
    
    select(dat,starts_with("n"))
    
    # ends_with() - extract columns that end with a specific naming convention.
    
    select(dat,ends_with("t"))
    
    # everything() - extract every remaining column not yet stated in the selection.
    
    select(dat,party,end,everything())
    
    # matches() - extract columns using a regular expression.
    
    select(dat,matches("^s"))
    
    
# Filtering Rows by Variable Values ----------------------------------------------------------
    
    filter(dat, party == "Republican" )
    
    filter(dat, party == "Democratic" )
    filter(dat, party != "Republican" )
    
    # Compound logical statements
    filter(dat, party == "Democratic" & start <= "2000-01-01")
    
    
# Arranging Rows by Variable Values  --------------------------------------
    
    # Arrange by character
    arrange(dat,name)
    arrange(dat,party)
    
    # Arrange by date
    arrange(dat,start)
    
    # Descending order using desc()
    arrange(dat,desc(start))
    
    # Arranging by multiple variables 
    arrange(dat,party,desc(start))
    
    
# Mutate to generate new variables ----------------------------------------
    
    mutate(dat, 
           cold_war = start <= "1989-01-01",
           cold_war = as.numeric(cold_war),
           republican = as.numeric(party == "Republican"),
           interaction = cold_war * republican)
    
    
# Transmute ---------------------------------------------------------------
    
    transmute(dat, 
              cold_war = start <= "1989-01-01",
              cold_war = as.numeric(cold_war),
              republican = as.numeric(party == "Republican"),
              interaction = cold_war * republican)
    
# Summarizing -------------------------------------------------------------
    
    # Aggregate variables using summarize
    summarize(dat,
              days_in_office = mean(end-start),
              max = max(end-start),
              min = min(end-start))
    
    # Helpful Summary Functions: n() & n_distinct()
    summarize(dat,
              N = n(),
              N_party = n_distinct(party))
    
    
# Group By + Summarizing --------------------------------------------------
    
    # Group by a variable
    x <- group_by(dat,party)
    
    # Then summarize by group
    summarize(x,min_in_office = min(end-start))
    
    
# Group by + Mutate -------------------------------------------------------
    
    x <- group_by(dat,party)
    
    mutate(x,min_in_office = min(end-start))
    
# Piping ------------------------------------------------------------------

  # Method 1: passing manipulations through an intermediary object
    x <- filter(dat,party=='Republican')
    x <- group_by(x,name)
    x <- transmute(x,t_in_office = end-start)
    x <- arrange(x,t_in_office)
    x <- ungroup(x)
    x
  
  # Method 2: Nesting functions
    ungroup(
      arrange(
        transmute(
          group_by(
            filter(dat,party=='Republican'),name),
          t_in_office = end-start),
        t_in_office
      )
    )
    
  # Method 3: Piping (do this!)
    dat %>%
      filter(party=='Republican') %>%
      group_by(name) %>%
      transmute(t_in_office = end-start) %>%
      arrange(t_in_office) %>% 
      ungroup()
    
# Joining and Reshaping ---------------------------------------------------

  # See slides 45 - 80

# Data Visualization ------------------------------------------------------

  # Fun Data. 
  library(palmerpenguins) # https://github.com/allisonhorst/palmerpenguins
    
  # Includes measurements for penguin species, island in Palmer Archipelago,
  # size (flipper length, body mass, bill dimensions), and sex. This is a
  # subset of penguins_raw.

  #' **Anatomy of a GGPLOT 2 visualization**
  #'
  #' (1) Data -- a given, but data often needs to be reformatted to accomplish
  #' different types of visualizations
  #'
  #' (2) Coordinate System -- `ggplot()` -- what is the space you're mapping to.
  #' Another given with the package.
  #' 
  #' (3) Mappings -- what variables do we want to map to the projected space? `aes()`
  #' 
  #' (4) Projection -- how should your mappings be projected onto the coordinate space? `geom_`
  #' 
    
  # Look at data
  penguins %>% glimpse()
  
  # simple visualization
  penguins %>% 
    ggplot(aes(x = bill_length_mm, y=flipper_length_mm)) +
    geom_point()
  
  # Add additional variables as aesthetics
  penguins %>% 
    ggplot(aes(x = bill_length_mm, y=flipper_length_mm,color=species)) +
    geom_point()
  
  # Control presentation and aesthetic elements with `theme_`, `scale_`, and
  # `labs` functions
  penguins %>% 
    ggplot(aes(x = bill_length_mm, y=flipper_length_mm,color=species)) +
    geom_point() +
    labs(x = 'Bill Length\n(millimeters)',y='flipper Length\n(millimeters)',color="") +
    scale_color_manual(values = c("red","pink","purple")) +
    theme_bw() +
    theme(legend.position = 'bottom')
  
  # Upgrade the presentation of your plot with ggthemes package
  penguins %>% 
    ggplot(aes(x = bill_length_mm, y=flipper_length_mm,color=species)) +
    geom_point() +
    labs(x = 'Bill Length\n(millimeters)',y='flipper Length\n(millimeters)',color="") +
    ggthemes::scale_color_economist() +
    ggthemes::theme_economist() +
    theme(legend.position = 'bottom')
  
  # Facet plots
  penguins %>% 
    drop_na() %>% 
    ggplot(aes(x = bill_length_mm, y=flipper_length_mm,color=species)) +
    geom_point() +
    labs(x = 'Bill Length\n(millimeters)',y='flipper Length\n(millimeters)',color="") +
    facet_wrap(~sex,ncol=2) +
    ggthemes::scale_color_colorblind() +
    theme_minimal() +
    theme(legend.position = 'bottom') 
  
  # facets are really useful for showing within unit variation
  penguins %>% 
    drop_na() %>% 
    ggplot(aes(bill_length_mm,fill=sex)) +
    geom_density(alpha=.5,color="white") +
    facet_wrap(~species,ncol=1) +
    theme(legend.position = 'top') 
    
  # Overlay multiple aesthetics. 
  penguins %>% 
    drop_na() %>% 
    ggplot(aes(sex,bill_length_mm,fill=sex,color=sex)) +
    geom_jitter(width = .1,show.legend = F) +
    geom_boxplot(alpha=.25,show.legend = F) +
    ggthemes::scale_color_gdocs() +
    ggthemes::scale_fill_gdocs() +
    ggthemes::theme_hc()
    
  # Easy manipulation for many models with geom_smooth
  penguins %>% 
    drop_na() %>% 
    ggplot(aes(x = bill_length_mm, y=flipper_length_mm,color=species)) +
    geom_point() +
    labs(x = 'Bill Length\n(millimeters)',y='flipper Length\n(millimeters)',color="") +
    ggthemes::scale_color_gdocs() +
    geom_smooth(method = "lm") +
    geom_smooth(method = "loess",se=F) +
    theme_minimal() +
    theme(legend.position = 'bottom') 
  
  

# Presenting Models -------------------------------------------------------


# Broom: from Model to Data Frame -----------------------------------------------------

  mod = lm(flipper_length_mm ~ bill_length_mm + sex + species, data=penguins)
  summary(mod)
  
  # Broom ==> model object to data frame
  broom::tidy(mod)
  
  # Once as a data frame, it's easy to visualize and work with.
  # For example, here is a coef plot
  broom::tidy(mod) %>% 
    filter(!str_detect(term,pattern="Intercept")) %>% 
    ggplot(aes(estimate,term)) +
    geom_point(size=2) +
    geom_vline(xintercept = 0,color="darkred",linetype=2) +
    geom_errorbarh(aes(xmin=estimate - std.error*1.96,
                       xmax=estimate + std.error*1.96,
                       y=term),
                   height=.05,size=1.5) +
    labs(title="Model 1: Coefficient Plot from OLS Model") +
    theme_minimal()
    

# Publishable Quality Tables ----------------------------------------------

  # Generating a publishable quality table
  require(stargazer)
  stargazer(mod,type = "text") # default latex
  
  # can handle multiple models
  mod1 = lm(flipper_length_mm ~ bill_length_mm, data=penguins)
  mod2 = lm(flipper_length_mm ~ bill_length_mm + sex + species, data=penguins)
  stargazer(mod1,mod2,type = "text")
  
  # Customization
  stargazer(mod1,mod2,type = "text",
            title = "Flipper Length on Bill length by species and sex",
            dep.var.labels = "Flipper Length (mm)",
            covariate.labels = c("Bill Length (mm)","Male","Chinstrap","Gentoo"),
            keep.stat = c('n','rsq'),
            notes = "Baseline is a Female Adelie")
