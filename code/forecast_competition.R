rm(list = ls())

library(tidyverse)

# Function to find N years with the most similar escapement numbers
find_nearest_years <- function(data, Brood.Year,N,idx_age) {
  
  data_use <- data[data$Brood.Year<Brood.Year,] # remove data more than the brood year, only use historical data not future data
  idx <- which(data$Brood.Year==Brood.Year)
  target_esc <- data$Escapement[idx]
  
  # get rid of NA
  data_use_NAfree <-data_use[!is.na(rowSums(data_use[idx_age])),]
  
  # find the abs of the difference
  esc_diff <- abs(data_use_NAfree$Escapement-target_esc)
  esc_order <- order(esc_diff) # order them
  
  closestN <- rbind(data_use_NAfree[esc_order[1:N],],data[idx,])
  
  return(closestN)
}

Forecast.Year <- 2025

# read data in
#raw_data <- read_csv("combined_brood.csv") 
raw_data <- read_csv(here::here("data/Original_Data_Pack/Combined_Brood_Bristol_Columbia_Fraser.csv"))

esc_data <- read_csv(here::here("data/Additional_Data_Spawners/Additional_Data_Spawners/GENERATED_SRDataSet_LongForm.csv"),skip = 3)


clean_data <- raw_data |>
  rename_with(~ str_replace(., "^AgeClass_", "X"), starts_with("AgeClass_")) |>
  rename(stock = River, Brood.Year = BroodYear, Recruits = Total_Recruits) |>
  select(-1) |>
  left_join(esc_data |>
              select(River,BroodYear, Escapement = Total_Spawners_BroodYear),
            by = c("stock" = "River", "Brood.Year" = "BroodYear"))
write_csv(clean_data,file = here::here("results/clean_data.csv"))
# read age conversion table
age_table <- read_csv(here::here("data/age_conversion.csv"))

age_table <- age_table |>
             mutate(Brood.Year = Forecast.Year - TotalAge)

stocks <- unique(clean_data$stock)

for(istock in 1:length(stocks)) {

stock <- clean_data |>
                filter(stock == stocks[istock] & Brood.Year > 1990)

# find the Brood years to forecast (only forecast dominate ages))
target_age <- stock |>
               pivot_longer(cols = X0.1:X3.4, 
                            values_to = "recruit", names_to = "ages") |>
               rename(total_recruits = Recruits) |>
               mutate(perc = recruit/total_recruits)  |>
               filter(recruit >= 1 & perc > 0.05) |># at least 5 years that are more than 5%
               count(ages) |>
               filter(n>5) |>
               left_join(age_table, by = c("ages"="EUAgeName")) |>
               select(Age = ages, Brood.Year) |>
               mutate(Forecast = NA)
#max_age <- Forecast.Year-min(target_age$Brood.Year)

brief <- stock |>
               select(Brood.Year,target_age$Age,Escapement) 
 
 # methodology: find two nearest neighbor years, then do the average for target age, need to calculate for different age
 
 # Step 1 find nearest two years based on escapement
 N <- 2  # Set the number of years to find
 s = 0 # sum of all forecast
 
 forecast <- data.frame(Brood.Year = unique(target_age$Brood.Year), 
                        Forecast = NA)
 
 for (iyear in unique(target_age$Brood.Year)){

 # average the N years of target age, then predict using the same R/S rate
 idx_age <- target_age$Age[target_age$Brood.Year == iyear] # find the age

  # Apply the function to your data
 nearest_years <- find_nearest_years(brief, iyear, N,idx_age)
 
 total_recruit <- sum(nearest_years[1:N,idx_age])
 total_esc <- sum(nearest_years$Escapement[1:N])
 forecast_age <- nearest_years$Escapement[N+1]*total_recruit/total_esc
 forecast$Forecast[forecast$Brood.Year==iyear] <- forecast_age
 s = s + forecast_age
 
 }
 #print(forecast)
 print(paste0(stocks[istock]," forecast is ",round(s)))
 
 }

### Retrospective

retro_results <- read_csv(here::here("results/Retrospective.csv"),show_col_types = FALSE)
for(Forecast.Year in 2020:2024){
  age_table <- age_table |>
    mutate(Brood.Year = Forecast.Year - TotalAge)
  
  stocks <- unique(clean_data$stock)
  
  for(istock in 1:length(stocks)) {
    
    stock <- clean_data |>
      filter(stock == stocks[istock] & Brood.Year > 1990)
    
    # find the Brood years to forecast (only forecast dominate ages))
    target_age <- stock |>
      pivot_longer(cols = X0.1:X3.4, 
                   values_to = "recruit", names_to = "ages") |>
      rename(total_recruits = Recruits) |>
      mutate(perc = recruit/total_recruits)  |>
      filter(recruit >= 1 & perc > 0.05) |># at least 5 years that are more than 5%
      count(ages) |>
      filter(n>5) |>
      left_join(age_table, by = c("ages"="EUAgeName")) |>
      select(Age = ages, Brood.Year) |>
      mutate(Forecast = NA)
    
    brief <- stock |>
      select(Brood.Year,target_age$Age,Escapement)
    
    # find two nearest neighbor years, then do the average for target age, need to calculate for different age
    N <- 2  # Set the number of years to find
    s = 0 # sum of all forecast
    
    forecast <- data.frame(Brood.Year = unique(target_age$Brood.Year), 
                           Forecast = NA)
    
    for (iyear in unique(target_age$Brood.Year)){
      
      
      
      #  average the N years of target age, then predict using the same R/S rate
      idx_age <- target_age$Age[target_age$Brood.Year == iyear] # find the age
      
      # Step 1 find nearest two years based on escapement
      
      # Apply the function to your data
      nearest_years <- find_nearest_years(brief, iyear, N,idx_age)
      
      total_recruit <- sum(nearest_years[1:N,idx_age])
      total_esc <- sum(nearest_years$Escapement[1:N])
      forecast_age <- nearest_years$Escapement[N+1]*total_recruit/total_esc
      forecast$Forecast[forecast$Brood.Year==iyear] <- forecast_age
      s = s + forecast_age
      
    }
    #print(forecast)
    
    #print(paste0(Forecast.Year,stocks[istock]," forecast is ",round(s)))
    idx_col <- which(colnames(retro_results)==Forecast.Year)
    idx_row <- which(retro_results$River==stocks[istock])
    retro_results[idx_row,idx_col] <- round(s)
  }
}

write_csv(retro_results,here::here("results/retrospective_results.csv"))
