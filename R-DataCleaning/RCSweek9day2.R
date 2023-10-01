library(tidyverse)
library(readxl)

# Description: You are working as a Data Scientist in Electricity Provider Company.
# “Electricity Usage” data has been given. Data is not clean enough for further analysis. You
# are expected to preprocess Data. Your task is to decompose data in delimited format.
# Hint: In order to read data in sheet 2 you may use following code:
#   raw <- read_xlsx("MO14-Round-1-Dealing-With-Data-Workbook.xlsx", na = "", sheet = 2,
#                    col_names = F); names(raw) <- 'col'


raw <- read_xlsx("C:/Users/ADMIN/Downloads/MO14-Round-1-Dealing-With-Data-Workbook.xlsx", na = "", sheet = 2,
                 col_names = F); names(raw) <- 'col'

#
df <- raw

#Replacing '_' with " "
df$col <- gsub('_', ' ', df$col)
df$col <- gsub('  ', ' ', df$col)

#replacing PM|AM|kwh
df <- df %>% mutate(meridian = case_when(grepl('AM', col) ~'AM',
                                         grepl('PM', col) ~'PM'),
                    unit=case_when(grepl('kwh', col) ~ 'kwh'))

df$col <- gsub('AM|PM|kwh', '', df$col)

df <- df %>% mutate(weekdays=case_when(grepl('Monday|Mon', col)~'Monday',
                                       grepl('Tuesday|Tue', col)~'Tuesday',
                                       grepl('Wednesday|Wed', col)~'Wednesday',
                                       grepl('Thursday|Thu', col)~'Thursday',
                                       grepl('Friday|Fri', col)~'Friday',
                                       grepl('Saturday|Sat', col)~'Saturday',
                                       grepl('Sunday|Sun', col)~'Sunday'))

df$col <- gsub("Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|Mon|Tue|Thu|Wed|Fri|Sat|Sun|Suay|Moay|uay", "", df$col)

#delete rd|th|st|nd
df$col <- gsub('rd|th|st|nd', '', df$col)

#trimming first spaces so that to delete first and last blanks in data
df$col <- trimws(df$col)

#create new columns and delete col
df <- df %>% separate(col, c('Hour', 'Date', 'Usage'), sep = ' ')

# separate(df, col, c('hour', 'date', 'usage'), sep = ' ')

#correct data types
df$Hour <- as.integer(df$Hour) 

df$Date <- df$Date %>% as.Date('%d-%b-%y')

df$Usage <- as.numeric(df$Usage)

#sorting data by date 
df <- df %>% arrange(Date)

#additional
# install.packages("timetk")

library(timetk)
df1 <-  df %>% timetk::tk_augment_timeseries_signature()
df <- df %>% cbind(df1$wday.lbl)

#deleting weekdays column
df <- df %>% select(-weekdays)

#filling NAs
df <- df %>% rename('weekdays'='df1$wday.lbl')

#changing places of columns
df <- df %>% select(Hour, meridian, Date, weekdays,Usage,unit)
