# This script compares the indices from the SAGA shiny app to
# the same indices in StockEff for GOM haddock.
# Initial comparison is without the measured-swept-area adjustment.
library(dplyr)
library(tidyr)
library(ggplot2)

directory <- "C:/Users/charles.perretti/Projects/SAGA/compare_saga_to_stockeff/"

saga_file_spring <- paste0(directory,"HADDOCK_SPRING_2_96_.csv")
saga_file_fall   <- paste0(directory,"HADDOCK_FALL_2_96_.csv")

saga_file_spring_ws <- paste0(directory,"HADDOCK_SPRING_2_96_withswept.csv")
saga_file_fall_ws   <- paste0(directory,"HADDOCK_FALL_2_96_withswept.csv")




# Load GOM haddock StockEff stratified mean indices (numbers and biomass)
stockeff_indices <- 
  read.csv(paste0(directory,"ADIOS_SV_164744_GOM_NONE_strat_mean.csv")) %>%
  select(YEAR, SEASON, INDEX_TYPE, INDEX, LOWER_90_CI, UPPER_90_CI) %>%
  mutate(source = "StockEff") %>%
  filter(INDEX_TYPE == "Abundance (numbers/tow)")

saga_indices_SPRING <- 
  read.csv(saga_file_spring, skip = 55, nrows = 52) %>%
  select(Year, Total) %>%
  mutate(INDEX_TYPE = "Abundance (numbers/tow)") %>%
  rename(YEAR = Year, INDEX = Total) %>% #, Var = VarNum) %>%
  # rbind({read.csv(paste0(directory, "HADDOCK_SPRING_2_96_.csv"), skip = 1, nrows = 52) %>%
  #        select(Year, Wt, VarWt) %>%
  #        mutate(INDEX_TYPE = "Biomass (kg/tow)") %>%
  #        rename(YEAR = Year, INDEX = Wt, Var = VarWt)}) %>%
  mutate(`LOWER_90_CI` = NA,#INDEX - 1.645*Var,
         `UPPER_90_CI` = NA,#INDEX + 1.645*Var,
         SEASON = "SPRING") %>%
  mutate(source = "SAGA Shiny App")

saga_indices_FALL <- 
  read.csv(saga_file_fall, skip = 55, nrows = 52) %>%
  #select(Year, Num, VarNum) %>%
  select(Year, Total) %>%
  mutate(INDEX_TYPE = "Abundance (numbers/tow)") %>%
  rename(YEAR = Year, INDEX = Total) %>% #, Var = VarNum) %>%
  # rbind({read.csv(paste0(directory,"HADDOCK_FALL_2_96_.csv"), skip = 1, nrows = 52) %>%
  #     select(Year, Wt, VarWt) %>%
  #     mutate(INDEX_TYPE = "Biomass (kg/tow)") %>%
  #     rename(YEAR = Year, INDEX = Wt, Var = VarWt)}) %>%
  mutate(LOWER_90_CI = NA, #INDEX - 1.645*Var,
         UPPER_90_CI = NA, #INDEX + 1.645*Var,
         SEASON = "FALL") %>%
  mutate(source = "SAGA Shiny App")
  
         
both_indices <- rbind(stockeff_indices, saga_indices_SPRING, saga_indices_FALL)

# Plot both indices together
ggplot(both_indices, aes(x = YEAR, y = INDEX, color = source)) +
  geom_line() +
  geom_point() +
  facet_grid(SEASON ~ INDEX_TYPE) +
  theme_bw() +
  ylab("") +
  xlab("")

# Plot percent differences in indices
pc_diff_index <-
  both_indices %>%
  select(-LOWER_90_CI, -UPPER_90_CI) %>%
  spread(source, INDEX) %>%
  mutate(`Percent difference` = 100 * (`SAGA Shiny App` - StockEff)/StockEff)

ggplot(pc_diff_index, aes(x = YEAR, y = `Percent difference`)) +
  geom_line() +
  geom_point() +
  facet_grid(SEASON ~ INDEX_TYPE) +
  theme_bw()


# Load StockEff and Saga NAA
stockeff_naa <- 
  read.csv(paste0(directory,"ADIOS_SV_164744_GOM_NONE_strat_mean_age_auto.csv")) %>%
  select(YEAR, SEASON, AGE, NO_AT_AGE) %>%
  filter(AGE < 9) %>%
  mutate(AGE = as.character(AGE)) %>%
  bind_rows({read.csv(paste0(directory,"ADIOS_SV_164744_GOM_NONE_strat_mean_age_auto.csv")) %>%
        select(YEAR, SEASON, AGE, NO_AT_AGE) %>% 
        group_by(YEAR, SEASON) %>% 
        filter(AGE >= 9) %>% 
        summarise(NO_AT_AGE = sum(NO_AT_AGE)) %>%
        mutate(AGE = "9+")}) %>%
  mutate(source = "StockEff")

saga_naa_SPRING <-
  read.csv(saga_file_spring, skip = 163, nrows = 52) %>%
  select(-nTows, -Total) %>%
  gather(AGE, NO_AT_AGE, -Year) %>%
  rename(YEAR = Year) %>%
  mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
  filter(AGE < 9) %>%
  mutate(AGE = as.character(AGE)) %>%
  bind_rows({read.csv(saga_file_spring, skip = 163, nrows = 52) %>%
             select(-nTows, -Total) %>%
             gather(AGE, NO_AT_AGE, -Year) %>%
             rename(YEAR = Year) %>%
             mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
             filter(AGE >= 9) %>% 
             group_by(YEAR) %>% 
             summarise(NO_AT_AGE = sum(NO_AT_AGE)) %>%
             mutate(AGE = "9+")}) %>%
  filter(NO_AT_AGE > 0) %>%
  mutate(SEASON = "SPRING",
         source = "SAGA Shiny App")

saga_naa_SPRING_ws <-
  read.csv(saga_file_spring_ws, skip = 163, nrows = 52) %>%
  select(-nTows, -Total) %>%
  gather(AGE, NO_AT_AGE, -Year) %>%
  rename(YEAR = Year) %>%
  mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
  filter(AGE < 9) %>%
  mutate(AGE = as.character(AGE)) %>%
  bind_rows({read.csv(saga_file_spring_ws, skip = 163, nrows = 52) %>%
      select(-nTows, -Total) %>%
      gather(AGE, NO_AT_AGE, -Year) %>%
      rename(YEAR = Year) %>%
      mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
      filter(AGE >= 9) %>% 
      group_by(YEAR) %>% 
      summarise(NO_AT_AGE = sum(NO_AT_AGE)) %>%
      mutate(AGE = "9+")}) %>%
  filter(NO_AT_AGE > 0) %>%
  mutate(SEASON = "SPRING",
         source = "SAGA Shiny App with swept area")

saga_naa_FALL <-
  read.csv(saga_file_fall, skip = 163, nrows = 52) %>%
  select(-nTows, -Total) %>%
  gather(AGE, NO_AT_AGE, -Year) %>%
  rename(YEAR = Year) %>%
  mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
  filter(AGE < 9) %>%
  mutate(AGE = as.character(AGE)) %>%
  bind_rows({read.csv(saga_file_fall, skip = 163, nrows = 52) %>%
      select(-nTows, -Total) %>%
      gather(AGE, NO_AT_AGE, -Year) %>%
      rename(YEAR = Year) %>%
      mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
      filter(AGE >= 9) %>% 
      group_by(YEAR) %>% 
      summarise(NO_AT_AGE = sum(NO_AT_AGE)) %>%
      mutate(AGE = "9+")}) %>%
  filter(NO_AT_AGE > 0) %>%
  mutate(SEASON = "FALL",
         source = "SAGA Shiny App")

saga_naa_FALL_ws <-
  read.csv(saga_file_fall_ws, skip = 163, nrows = 52) %>%
  select(-nTows, -Total) %>%
  gather(AGE, NO_AT_AGE, -Year) %>%
  rename(YEAR = Year) %>%
  mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
  filter(AGE < 9) %>%
  mutate(AGE = as.character(AGE)) %>%
  bind_rows({read.csv(saga_file_fall_ws, skip = 163, nrows = 52) %>%
      select(-nTows, -Total) %>%
      gather(AGE, NO_AT_AGE, -Year) %>%
      rename(YEAR = Year) %>%
      mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
      filter(AGE >= 9) %>% 
      group_by(YEAR) %>% 
      summarise(NO_AT_AGE = sum(NO_AT_AGE)) %>%
      mutate(AGE = "9+")}) %>%
  filter(NO_AT_AGE > 0) %>%
  mutate(SEASON = "FALL",
         source = "SAGA Shiny App with swept area")
  

both_naa <- 
  rbind(stockeff_naa, saga_naa_SPRING, saga_naa_FALL,
        saga_naa_SPRING_ws, saga_naa_FALL_ws) %>%
  mutate(AGE = paste0("Age-", AGE)) %>%
  filter(YEAR >= 2009)

# Plot spring NAA comparison
ggplot(both_naa %>% filter(SEASON == "SPRING"),
       aes(x = YEAR, y = NO_AT_AGE, color = source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AGE, scales = "free_y") +
  theme_bw() +
  ylab("Stratified mean numbers-at-age") +
  xlab("Year") +
  ggtitle("Spring NAA comparison")

# Plot fall NAA comparison
ggplot(both_naa %>% filter(SEASON == "FALL"),
       aes(x = YEAR, y = NO_AT_AGE, color = source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AGE, scales = "free_y") +
  theme_bw() +
  ylab("Stratified mean numbers-at-age") +
  xlab("Year") +
  ggtitle("Fall NAA comparison")

# Facet by year rather than age
ggplot(both_naa %>% 
         filter(SEASON == "SPRING") %>%
         mutate(AGE = as.numeric(substr(AGE,5,5))),
       aes(x = AGE, y = NO_AT_AGE, color = source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ YEAR, scales = "free_y") +
  theme_bw() +
  ylab("Stratified mean numbers-at-age") +
  xlab("Age") +
  ggtitle("Spring NAA comparison")

# Facet by year rather than age
ggplot(both_naa %>% 
         filter(SEASON == "FALL") %>%
         mutate(AGE = as.numeric(substr(AGE,5,5))),
       aes(x = AGE, y = NO_AT_AGE, color = source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ YEAR, scales = "free_y") +
  theme_bw() +
  ylab("Stratified mean numbers-at-age") +
  xlab("Age") +
  ggtitle("Fall NAA comparison")


# Plot percent differences in indices
pc_diff_naa <-
  both_naa %>%
  spread(source, NO_AT_AGE) %>%
  mutate(`SAGA without adjustment vs StockEff` = 100 * (`SAGA Shiny App` - StockEff)/StockEff,
         `SAGA without adjustment vs SAGA with adjustment` = 
           100 * (`SAGA Shiny App` - `SAGA Shiny App with swept area`)/`SAGA Shiny App with swept area`) %>%
  select(YEAR, SEASON, AGE, `SAGA without adjustment vs StockEff`, `SAGA without adjustment vs SAGA with adjustment`) %>%
  gather(comparison, `Percent difference`, -YEAR, -SEASON, -AGE) %>%
  mutate(`Absolute percent difference` = abs(`Percent difference`))

ggplot(pc_diff_naa %>% filter(SEASON == "SPRING"), 
       aes(x = YEAR, y = `Absolute percent difference`, color = comparison)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AGE) +
  theme_bw() +
  ggtitle("Spring NAA differences")

ggplot(pc_diff_naa %>% filter(SEASON == "FALL"), 
       aes(x = YEAR, y = `Absolute percent difference`, color = comparison)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AGE) +
  theme_bw() +
  ggtitle("Fall NAA differences")


# Plot proportions-at-age
propaa <-
  both_naa %>%
  group_by(YEAR, SEASON, source) %>%
  mutate(PROP_AT_AGE = NO_AT_AGE/sum(NO_AT_AGE)) %>%
  select(YEAR, SEASON, source, AGE, PROP_AT_AGE) 

ggplot(propaa %>%
         filter(SEASON == "FALL") %>%
         mutate(AGE = as.numeric(substr(AGE,5,5))), 
       aes(x = AGE, y = PROP_AT_AGE, color = source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~YEAR, scales = "free_y") +
  theme_bw() +
  ylab("Proportion-at-age") +
  xlab("Age") +
  ggtitle("Fall proportion-at-age comparison")

ggplot(propaa %>%
         filter(SEASON == "SPRING") %>%
         mutate(AGE = as.numeric(substr(AGE,5,5))), 
       aes(x = AGE, y = PROP_AT_AGE, color = source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~YEAR, scales = "free_y") +
  theme_bw() +
  ylab("Proportion-at-age") +
  xlab("Age") +
  ggtitle("Spring proportion-at-age comparison")


# Plot differences in prop-at-age
diff_propaa <-
  propaa %>%
  spread(source, PROP_AT_AGE) %>%
  mutate(`SAGA without adjustment vs StockEff` = `SAGA Shiny App` - StockEff,
         `SAGA without adjustment vs SAGA with adjustment` = `SAGA Shiny App` - `SAGA Shiny App with swept area`) %>%
  select(YEAR, SEASON, AGE, 
         `SAGA without adjustment vs StockEff`, `SAGA without adjustment vs SAGA with adjustment`) %>%
  gather(comparison, Difference, -YEAR, -SEASON, -AGE) %>%
  mutate(`Absolute difference` = abs(Difference))

ggplot(diff_propaa %>% filter(SEASON == "SPRING"), 
       aes(x = YEAR, y = `Absolute difference`, color = comparison)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AGE) +
  theme_bw() +
  ylab("Absolute difference") +
  ggtitle("Spring proportion-at-age differences")

ggplot(diff_propaa %>% filter(SEASON == "FALL"), 
       aes(x = YEAR, y = `Absolute difference`, color = comparison)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AGE) +
  theme_bw() +
  ylab("Absolute difference") +
  ggtitle("Fall proportion-at-age differences")

