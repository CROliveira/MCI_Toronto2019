---
# Title: Toronto Major Crime Indicators
# Subtitle: Crime by Neighborhoods
# Purpose: Use Toronto Police Service Public Safety Data Portal to get crime data from
# Toronto by Major Crime Indicators
# Author: Celio Oliveira
# Contact: oliveira.celior@gmail.com
# Date: r format(Sys.time(), "%d %B %Y")
# Pre-requisites: None
# Output:bookdown::pdf_document2:toc: yes
# thanks: "Code and data are available at: https://github.com/CROliveira/MCI_Toronto2019"
# bibliography: references.bib
# output:
#   word_document: default
# pdf_document: default
# html_document:
#   df_print: paged
---
  

# MAJOR CRIME INDICATORS IN TORONTO 2019


## Setting up the environment

# check.packages function: install and load multiple R packages.
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
check_for_packages <- function( pkg){
  new.pkg <- pkg[!( pkg % in % installed.packages()[, "Package"])]
  if ( length( new.pkg)) 
    install.packages( new.pkg, dependencies = TRUE)
  sapply( pkg, require, character.only = TRUE)
}

# Passing the libraries call to the check.packages function
packages<-c( "tidyverse", "here", "dplyr", "ggplot2",  "ggmap")
check_for_packages( packages) # Return TRUE if packages are installed, FALSE otherwise

here::here()

# Introduction

# The Data

# library(opendatatoronto)
# 
# mci_data_description <-
#   opendatatoronto::search_packages( "Major Crime Indicators") %>%
#   opendatatoronto::list_package_resources() %>%
#   filter( name == "data_dictionary.csv") %>%
#   select( id) %>%
#   opendatatoronto::get_resource()

# package <- show_package( "247788f6-ca20-42e8-b00f-894ac43053e5")
# write.csv(here::here( "inputs/data/mci_data_description.csv"))
# View( mci_data_description)

# url <- "https://opendata.arcgis.com/datasets/56a0d46ae5f141269f2598a8c07e25c1_0.geojson"
# output_file <- "/Users/celio/Desktop/UofT/Course load/4th_Term/INF2178H - Experimental Design for DS/Assignment/inputs/data"
# download.file( url, output_file)


# data <- read_csv(here( "inputs/data/MCI_2014_to_2019.csv"))
# drop <- c( "X","Y")
# data = data[,!( names(data) %in% drop)] # Dropping columns X, Y.

# filetered_mci <- filter( data, occurrenceyear == "2019")
# write.csv(here::here( "inputs/data/filetered_mci.csv"))



## Data Description

# 
# 1. *Index_:*	Record Unique Identifier
# 2. *event_unique_id:*	Event Unique Identifier
# 3. *occurrencedate:*	Date of occurrence
# 4. *reporteddate:*	Date occurrence was reported
# 5. *premisetype:*	Premise where occurrence took place
# 6. *ucr_code:*	URC Code
# 7. *ucr_ext:*	URC Code Extension
# 8. *offence:*	Offence related to the occurrence
# 9. *reportedyear:*	Year occurrence was reported
# 10. *reportedmonth:*	Month occurrence was reported
# 11. *reportedday:*	Day occurrence was reported
# 12. *reporteddayofyear:*	Day of year Occurrence was reported. 
# 13. *reporteddayofweek:*	Day of week occurrence was reported
# 14. *reportedhour:*	Hour occurrence was reported
# 15. *occurrenceyear:*	Occurrence year
# 16. *occurrencemonth:*	Occurrence month
# 17. *occurrenceday:*	Occurrence day
# 18. *occurrencedayofyear:*	Occurrence day of year
# 19. *occurrencedayofweek:*	Occurrence day of week
# 20. *occurrencehour:*	Occurrence hour
# 21. **MCI:*	Major Crime Indicator related to the offence
# 22. *Division:*	Division where event occurred
# 23. *Hood_ID:*	Neighbourhood Name
# 24. *Neighborhood:*	Neighborhood Identificator
# 25. *Long:*	Longitude of point extracted after offsetting X and & Coordinates to nearest intersection node
# 26. *Lat:*	Latitude of point extracted after offsetting X and & Coordinates to nearest intersection node
# 27. *ObjectId:* 	Id of the report

mci <- read_csv( here( "inputs/data/filetered_mci.csv"))
# head( mci) # display first six rows
# names( mci) # column names

# mci_type <- table(mci$MCI, mci$occurrencedate) # Distinct value count for MCI column

ggplot(mci, aes(y = MCI, fill = occurrencemonth)) +
  geom_bar(stat = "count", position = position_dodge2(preserve = "single")) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = "Major Crimes Indicator TORONTO 2019", caption = " Assault:19600; Auto-theft:5137; Break and Enter:8278; Robbery:3408; Theft-over:1251 ")

dim( mci) # Get the data size

object.size( mci) # Get the memory size of the data



# Research Questions

## At what time most crimes happen in Toronto?


hour_crime <- summarise( group_by( mci, occurrencehour, MCI), n = n())
ggplot( hour_crime, aes( x = occurrencehour, y = n, color = MCI)) +
  geom_line( size = 1) +
  ggtitle( " Hourly Crimes in Toronto 2019") +
  ylab( "# Occurrences") +
  xlab( "24 hour clock") +
  theme_bw() + labs( title = " Hourly Crime Types in Toronto 2019") +
  theme( plot.title = element_text( size = 16),
         axis.title = element_text( size = 12, face = "bold"))



## What is the relation between time of the day and crime type?

# indicator_group <- group_by( mci, MCI)
# crime_by_indicator <- summarise( indicator_group, n = n())
# crime_by_indicator <- crime_by_indicator[ order( crime_by_indicator$n, decreasing = TRUE),]
# ggplot( aes( x = reorder( MCI, n), y = n), data = crime_by_indicator) +
#   geom_bar( stat = 'identity', width = 0.5) +
#   geom_text( aes( label = n), stat = 'identity', data = crime_by_indicator, hjust = -0.1, size = 2.3) +
#   coord_flip() +
#   xlab( 'Major Crime Indicators') +
#   ylab( 'Number of Occurrences') +
#   ggtitle( 'Major Crime Indicators Toronto 2016') +
#   theme_bw() +
#   theme( plot.title = element_text( size = 16),
#          axis.title = element_text( size = 12, face = "bold"))



## What are the top 10 Assault subgroups


assault_sub_group <- summarise( group_by( mci[ mci$MCI == 'Assault', ], offence), n = n( ))[ order( summarise( group_by( mci[ mci$MCI == 'Assault', ], offence), n = n( ))$n, decreasing = TRUE), ]

assault_sub_group <- assault_sub_group %>% slice_max( offence, n = 10) # Top 10 values

ggplot( aes( x = reorder( offence, n), y = n), data = assault_sub_group) +
  geom_bar( stat = 'identity', width = 0.6) +
  geom_text( aes( label = n), stat = 'identity', data = assault_sub_group, hjust = -0.1, size = 2) +
  coord_flip( ) +
  xlab( "Types of Assault") +
  ylab( "# Occurrences") +
  ggtitle( " Offences in Toronto 2019") +
  theme_bw( ) +
  theme( plot.title = element_text( size = 16),
         axis.title = element_text( size = 12, face = "bold"),
         axis.title.y = element_blank( ))



## Where in the city these crimes are most likely to happen?

crime_location <- summarise( group_by( mci, Neighbourhood), n = n( ))[ order( summarise( group_by( mci, Neighbourhood), n = n( ))$n, decreasing = TRUE), ]
top25_location <- head( summarise( group_by( mci, Neighbourhood), n=n( )), 25)
ggplot( aes( x = reorder( Neighbourhood, n), y = n), data = top25_location) +
  geom_bar( stat = 'identity', width = 0.5) +
  geom_text( aes( label = n), stat = 'identity', data = top25_location, hjust = -0.1, size = 2.5) +
  coord_flip( ) +
  ggtitle( "Top 25 Neighbourhood by crime type") +
  theme( plot.title = element_text( size = 10),
         axis.title = element_text( size = 8),
         axis.title.y = element_blank( ),
         axis.title.x = element_blank( ))


map_data <- data.frame( mci$MCI, mci$Lat, mci$Long)
colnames( map_data) <- c("Crimes", "Lat", "Lon")
sbbox <- make_bbox( lon = mci$Long, lat = mci$Lat, f = 0.01)
my_map <- get_map( location = sbbox, maptype = "roadmap", scale = 2, color = "bw", zoom = 9)
ggmap( my_map) +
  geom_point( data=map_data, aes( x = mci$Long, y = mci$Lat, color = "#27AE60"), 
              size = 0.5, alpha = 0.05) + ggtitle("       Toronto's Crime Map 2019") +
  guides( color = FALSE) + facet_wrap( ~ Crimes, nrow = 2) +
  theme( axis.line = element_blank( ), axis.text = element_blank( ),
         axis.ticks = element_blank( ), axis.title.x = element_blank( ), 
         axis.title.y = element_blank( ), plot.margin = unit( c( 0, 0, -1, -1), "lines"))



# Conclusion



# References
