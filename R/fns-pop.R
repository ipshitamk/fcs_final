library(readr)
library(rvest)
library(tidyr)

# Population functions
# Defining the UN Data and citypopulation.de pop function because it is used as backup in Oxford and in Density
get_un_pop_growth <- function(city, country = country) {
  # UN Data
  if (!file.exists(paths$undata_file)) {
    warning(paste0("undata_file (", paths$undata_file, ") does not exist."))
    return(NULL)
  } else {
    pop_growth_undata <- read_csv(paths$undata_file,
                                  col_types = "cdccccccddc", n_max = 69490) %>%
      filter(`Country or Area` == country) %>%
      filter(Sex == "Both Sexes") %>%
      filter(str_detect(tolower(tolatin(City)), tolower(tolatin(city)))) %>%
      select(Location = City, Year, Population = Value) %>%
      mutate(Source = "UN Data") %>%
      arrange(Year)
    return(pop_growth_undata)
  }
}  

# citypopulation.de
get_de_pop_growth <- function(city, country = country) {
  url <- paste0("https://www.citypopulation.de/en/", str_replace_all(tolower(country), " ", ""), "/cities/")
  table_ids = c("citysection", "largecities", "adminareas")
  for (id in table_ids) {
    de <- read_html(url) %>%
      html_node("section#citysection") %>%
      html_node("table") %>%
      html_table()
      if (any(str_detect(tolatin(de$Name), tolatin(city)))) break
    }

  pop_growth_de <- de %>%
    select(Location = Name, contains("Population"), Area = starts_with("Area")) %>%
    filter(str_detect(tolatin(Location), tolatin(city))) %>%
    pivot_longer(cols = contains("Population"), values_to = "Population", names_to = "Year") %>%
    mutate(
      Location = Location,
      Country = country,
      Year = str_extract(Year, "\\d{4}") %>% as.numeric(),
      Population = str_replace_all(Population, ",", "") %>% as.numeric(),
      Source = "citypopulation.de",
      Area_km = as.numeric(Area)/100,
      .keep = "unused") %>%
    arrange(Year)
  
  if (nrow(pop_growth_de) == 0) warning(glue::glue("No population data detected for {city} in citypopulation.de table"))
  if (length(unique(pop_growth_de$Location)) > 1) warning(glue::glue("More than one '{city}' detected in citypopulation.de table"))
  
  return(pop_growth_de)
}

un_de_pop_growth <- function(city, country) {
  # Select whether to use citypopulation.de or UN data based on which has more data
  # Alternatively, can plot both, coloring each line by Source column
  pop_growth <- bind_rows(get_un_pop_growth(city, country),
                          get_de_pop_growth(city, country))
  return(pop_growth)
}

tolatin <- function(x) stringi::stri_trans_general(x, id = "Latin-ASCII")


rate_of_change <- function(data) {
  # Get the first and last columns of numeric data (assume columns are in chronological order)
  first_year <- data[, 2]
  last_year <- data[, ncol(data)]
  
  # Calculate the rate of change for each scenario
  roc_values <- ((last_year - first_year) / first_year) * 100
  
  # Create a data frame with the Scenario and ROC
  roc_data <- data.frame(
    Scenario = data$Scenario,
    Rate_of_Change = roc_values
  )
  
  # Identify highest positive and highest negative rate of change
  highest_positive <- roc_data[which.max(roc_data$Rate_of_Change), ]
  highest_negative <- roc_data[which.min(roc_data$Rate_of_Change), ]
  
  # Print summary information
  # cat("Scenario with the highest positive rate of change:\n")
  # cat("Scenario:", highest_positive$Scenario, 
  #     " | Rate of Change:", round(highest_positive$Rate_of_Change, 2), "%\n\n")
  # 
  # cat("Scenario with the highest negative rate of change:\n")
  # cat("Scenario:", highest_negative$Scenario, 
  #     " | Rate of Change:", round(highest_negative$Rate_of_Change, 2), "%\n\n")
  
  return(roc_data)
}


# Define function to calculate and print rate of change for a specific SSP scenario and year range
rate_of_change_ssp <- function(data, ssp, start_year, end_year) {
  # Clean up column names to remove any unwanted characters
  data <- data %>% rename_with(~ gsub("^\\s+|\\s+$", "", .))  # Trim whitespace from column names
  
  # Ensure the specified years are in the column names
  if (!(as.character(start_year) %in% colnames(data)) || !(as.character(end_year) %in% colnames(data))) {
    cat("Error: Specified start or end year not found in the data.\n")
    return(NULL)
  }
  
  # Filter the data for the specified SSP scenario
  ssp_data <- data[data$Scenario == ssp, ]
  
  # Check if the specified SSP exists in the data
  if (nrow(ssp_data) == 0) {
    cat("Error: Specified SSP scenario not found in the data.\n")
    return(NULL)
  }
  
  # Extract population values for the specified start and end years
  initial_value <- as.numeric(ssp_data[[as.character(start_year)]])
  final_value <- as.numeric(ssp_data[[as.character(end_year)]])
  
  # Calculate the rate of change for the specified SSP and year range
  roc_value <- ((final_value - initial_value) / initial_value) * 100
  
  # Print the rate of change information
  # cat("Rate of change for", ssp, "from", start_year, "to", end_year, ":\n")
  # cat("Initial Year:", start_year, " | Value:", initial_value, "\n")
  # cat("Final Year:", end_year, " | Value:", final_value, "\n")
  # cat("Rate of Change:", round(roc_value, 2), "%\n\n")
  
  # Return the rate of change as a single value
  return(roc_value)
}
