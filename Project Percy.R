#download library
library(tidyverse)
library(lubridate)
#download the zip file of private elementary and secondary
#schools revenues, by direct source of funds
temp_file <- tempfile()
download.file("https://www150.statcan.gc.ca/n1/tbl/csv/37100084-eng.zip",temp_file)
(file_list <- as.character(unzip(temp_file, list = TRUE)$Name))

#Clean the data
private_data <- read_csv(unz(temp_file, "37100084.csv"))
unlink(temp_file) #Deleting temporary file

#Organize the private data set
private <- private_data %>%
  mutate(Year = as.Date(paste0(substr(REF_DATE, 1, 4), "-01-01"))) %>%
  rename(Revenue = VALUE) %>%
  select(Year, Revenue)

#Visualize the data to see how private school revenues originating
#from board and lodging fees changed over the years

library(ggplot2)

ggplot(private, aes(x = Year, y = Revenue)) +
  geom_col(fill = "black") +  # Use geom_col() for actual values, not counts
  ggtitle("Private School Revenues from Board & Lodging Fees") +
  labs(x = "Year", y = "Revenue (CAD)") +
  theme_minimal()



# Summarize revenue over time and by province
funding_trends <- private_data %>%
  mutate(Date = as.Date(paste0(substr(REF_DATE, 1, 4), "-01-01"))) %>%  # Convert year format
  filter(GEO != "Canada") %>%  # Remove national total
  group_by(Date, GEO) %>%
  summarise(TotalFunding = sum(VALUE, na.rm = TRUE)) %>%
  ungroup()

# Line chart by province
ggplot(funding_trends, aes(x = Date, y = TotalFunding, color = GEO)) +
  geom_line(size = 1) +
  labs(title = "Private School Revenues by Province Over Time",
       x = "Year",
       y = "Revenue (CAD)",
       color = "Province") +  # Legend label
  theme_minimal()

