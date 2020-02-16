library(tidyverse)
library(readxl)

# Read in dataset
# Machine outputs many different tables on one excel sheet
# Need to find the individual tables on the sheet before calculations
dat <- read_xlsx(here::here('machine_output.xlsx'))

# Determine where the blank rows of the data set are
blankrows <- data_frame(
            blanks = which(is.na(dat))) %>% # what row is blank
            mutate(dif = blanks - lag(blanks),# difference in consecutive blank rows
                   rownum = row_number(), # new variable with the row number of new data frame
                   startrow = ifelse(rownum == 1, 1, NA), # If the row number is 1, then start row is 1
                   # if the difference between consecutive blank rows is 1, the start row is 1
                   # if it is not 1, then the start row is the blanks value from previous row plus 1
                   startrow = coalesce(ifelse(dif == 1, lag(startrow, default = 1), lag(blanks + 1)), 1)) 

# define the startrow and endrow of each table on the excel sheet
tableindex <- blankrows %>%
              group_by(startrow) %>%
              summarize(endrow = min(blanks)) %>%
              ungroup() %>%
              distinct(startrow, endrow)

# Donor and Acceptor tables are the first two on the sheet
fluorophore_readings <- map(2:3, function(.x) {
  # Select the tables from the excel sheet based off of the start and end rows
  x <- dat[tableindex$startrow[.x]:tableindex$endrow[.x],]
  # Remove rows that have an NA
  x <- x %>% 
    as_tibble() %>%
    drop_na()
  # Values from first row as column names and then drop the row
  colnames(x) <- unlist(x[1,])
  x <- x[-1,]
}) 

# Name elements of list
names(fluorophore_readings) <- c('donor', 'acceptor')

# Pivot each table to tidy format
donor <- fluorophore_readings[['donor']] %>% 
            mutate_all(funs(as.numeric(.))) %>%
              pivot_longer(cols = 2:ncol(.), names_to = 'Well', values_to = 'Count1')

acceptor <- fluorophore_readings[['acceptor']] %>% 
              mutate_all(funs(as.numeric(.))) %>%
              pivot_longer(cols = 2:ncol(.), names_to = 'Well', values_to = 'Count2')


bret_ratio <- left_join(donor, acceptor, by = c("Time [s]", "Well")) %>% # Join the two tables by Time and Well
                      drop_na() %>% # Remove rows with an NA value
                      mutate(bret_ratio = Count2 / Count1, # Calculate BRET ratio
                             injection = ifelse(`Time [s]` <= 40, "pre", "post")) %>% # Define pre and post treatment injection time points
                      group_by(Well, injection) %>%
                      summarize(avg_injection = mean(bret_ratio)) %>% # Find average BRET pre and post injection
                      pivot_wider(names_from = injection, values_from = avg_injection) %>% # Spread dataset
                      mutate(delta = post - pre) # Calculate delta BRET due to treatment injection
         
