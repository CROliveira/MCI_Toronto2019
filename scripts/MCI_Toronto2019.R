# ---
#   Title: Toronto Major Crime Indicators
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
#   html_document:
#   df_print: paged
# pdf_document: default
# word_document: default
# ---


## Setting up the environment



# Check.packages function: install and load multiple R packages.
check_for_packages <- function( pkg){
  # Check to see if packages are installed. Install them if they are not, then load them into the R session.  
  new.pkg <- pkg[!( pkg %in% installed.packages()[, "Package"])]
  if ( length( new.pkg)) 
    install.packages( new.pkg, dependencies = TRUE)
  sapply( pkg, require, character.only = TRUE) 
}

# Passing the library calls to the check.packages function
packages<-c( "tidyverse", "here", "dplyr", "ggplot2",  "ggmap", "kableExtra")

check_for_packages( packages) # Returns TRUE if packages are installed, FALSE otherwise

here::here()

# Introduction


# MAJOR CRIME INDICATORS IN TORONTO 2019


# The Data

# library(opendatatoronto)
# 
# mci_data_description <-
#    opendatatoronto::search_packages( "Major Crime Indicators") %>% # Query based on the term/phrase
#    opendatatoronto::list_package_resources() %>% # Returns a list of the findings
#    filter( name == "data_dictionary.csv") %>% # Get this specific file name
#    select( id) %>% # Selects the row from the list to download
#    opendatatoronto::get_resource() # Download the package/data/file
# 
# write.csv(here::here( "inputs/data/mci_data_description.csv")) # Write the downloaded file on a specific directory
# View( mci_data_description)


#### ATTENTION: IT IS NOT RECOMMENDED TO RUN THIS PART OF THE CODE IF YOU HAVE A FREE ACCOUNT ON GITHUB AS THE FILE IS 168 MB. HENCE, TOO LARGE TO COMMIT. #####
# url <- "https://opendata.arcgis.com/datasets/56a0d46ae5f141269f2598a8c07e25c1_0.geojson" # File of interest's URL
# output_file <- here::here("inputs/data/MCI_2014_to_2019.csv") # Destination directory to store the new file 
# download.file( url, output_file) # Download the file
# 
# data <- read_csv(here( "inputs/data/MCI_2014_to_2019.csv")) # Read the dataset stored on this directory
# drop <- c( "X","Y") # Creates a variable with columns to be dropped
# data = data[,!( names(data) %in% drop)] # Dropping columns X, Y.
#
# filetered_mci <- filter( data, occurrenceyear == "2019") # Filter the dataset by occurrence year (2019) ##### > 50 MB MEMORY SIZE
# write.csv(here::here( "inputs/data/filetered_mci.csv")) # Write the filtered dataset 

################## END OF GIGANTIC FILE ###############################

## Data Description


mci <- read_csv( here( "inputs/data/filetered_mci.csv"))
# head( mci) # Displays first six rows
# names( mci) # Get column names
mci$occurrencedate <- as.Date( mci$occurrencedate) # Converts a column to date format
mci$reporteddate <- as.Date( mci$reporteddate) # Converts a column to date format

# mci_type <- table(mci$MCI, mci$occurrencedate) # Distinct value count for MCI column
mci_selected <- mci %>% select("MCI", "occurrencedate", "reporteddate", "offence", "Neighbourhood", "Lat", "Long" ) # Slice a dataset by columns

mci_selected <- mci_selected[1:10, 1:7] # Slice by rows (1-10), columns (1-7)
mci_selected$Lat <- format(round(mci_selected$Lat, 2), nsmall = 2) # Round to 2 decimals
mci_selected$Long <- format(round(mci_selected$Long, 2), nsmall = 2) # Round to 2 decimals

mci_selected <- mci_selected %>% rename( 
  Occurrence = occurrencedate, Reported = reporteddate, Offense = offence) # Rename columns 


# Creates a kable table and style the lines by stripes, font size 8, with a full width to the page, arrange the position to the left. It also gets the row 0 (header) in angle (0-360) including negative for anti-clock direction.
kbl(mci_selected) %>% kable_styling( 
  bootstrap_options = "striped", font_size = 8, full_width = F, position = "left") %>% 
  # add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2, "Group 4" = 2)) %>%
  row_spec( 0, angle = 0)


#Creates a bar plot with aesthetics on y axis MCI column, x axis by count, filling by occurrence month
ggplot(mci, aes(y = MCI, fill = occurrencemonth)) +
  geom_bar(stat = "count", position = position_dodge2(preserve = "single")) + # makes stacked bar sideways 
  theme(plot.title = element_text( size = 20, face = "bold", hjust = 0.5), # Set font size 20, in bold and centralized
        axis.title.y = element_blank(), # Do not print y nor x lable titles
        axis.title.x = element_blank()) +
  labs(title = "Major Crimes Indicator TORONTO 2019", caption = " Assault:19600; Auto-theft:5137; Break and Enter:8278; Robbery:3408; Theft-over:1251 ") # Inserts a titleat the top and a caption below the image


dim( mci) # Get the data dimensions/size

object.size( mci) # Get the memory size of the data

#There are many summary statistics available in R; Psych function provides the ones most useful for scale construction and item analysis in classic [Psychometrics](https://www.rdocumentation.org/packages/psych/versions/2.0.12/topics/describe) that brings a Summary Statistics about the non-categorical features.


# describe( mci, omit = TRUE, fast = NULL) # Summary statistics


# Research Questions

## At what time most crimes happen in Toronto?



hour_crime <- summarise( group_by( mci, occurrencehour, MCI), n = n()) # Group the data by mci and occurrence hour
ggplot( hour_crime, aes( x = occurrencehour, y = n, color = MCI)) +
  geom_line( size = 1) + # Set the line size of 1 
  ggtitle( " Hourly Crimes in Toronto 2019") +
  ylab( "# Occurrences") + # Set y lable
  xlab( "24 hour clock") + # Set x lable
  theme_bw() + labs( title = " Hourly Crime Types in Toronto 2019") +
  theme( plot.title = element_text( size = 20, face = "bold", hjust = 0.5), # Set font size 20, in bold and centralized
         axis.title = element_text( size = 12, face = "bold"))  # Set the size of x and y lables



## What is the relation between time of the day and crime type?

# indicator_group <- group_by( mci, MCI) # Group by method if needed 
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

# Getting offenses as subcategories of assault and grouping it by mci on a decreasing order
assault_sub_group <- summarise( group_by( mci[ mci$MCI == 'Assault', ], offence), n = n( ))[ order( summarise( group_by( mci[ mci$MCI == 'Assault', ], offence), n = n( ))$n, decreasing = TRUE), ]

assault_sub_group <- assault_sub_group %>% slice_max( offence, n = 10) # Getting the top 10 values

ggplot( aes( x = reorder( offence, n), y = n), data = assault_sub_group) + # Reordering the offense group by count
  geom_bar( stat = 'identity', width = 0.6) + # Getting individual bars, without stacking and setting bar graph widtt to .6 cm
  geom_text( aes( label = n), stat = 'identity', data = assault_sub_group, hjust = -0.1, size = 2) +
  coord_flip( ) +
  xlab( "Types of Assault") +
  ylab( "# Occurrences") +
  ggtitle( "Offenses in Toronto 2019") +
  theme_bw( ) + # black and white background 
  theme( plot.title = element_text( size = 20, face = "bold", hjust = 0.5), # Set font size 20, in bold and centralized
         axis.title = element_text( size = 12, face = "bold"),
         axis.title.y = element_blank( ))


## Where in the city these crimes are most likely to happen?

# Getting subcategories of neighborhood and grouping it by mci on a decreasing order
crime_location <- summarise( group_by( mci, Neighbourhood), n = n( ))[ order( summarise( group_by( mci, Neighbourhood), n = n( ))$n, decreasing = TRUE), ]
top25_location <- head( summarise( group_by( mci, Neighbourhood), n=n( )), 25) # GEtting the top 25 locations
ggplot( aes( x = reorder( Neighbourhood, n), y = n), data = top25_location) +
  geom_bar( stat = 'identity', width = 0.5) + # Setting bar graph widtt to .5 cm
  geom_text( aes( label = n), stat = 'identity', data = top25_location, hjust = -0.1, size = 2.5) +
  coord_flip( ) + ggtitle( "Top 25 Neighbourhood by crime type") + theme_bw( ) +
  theme( plot.title = element_text( size = 15, face = "bold", hjust = 0.5), # Set font size 15, in bold and centralized
         # axis.title = element_text( size = 12), 
         axis.title.y = element_blank( ),
         axis.title.x = element_blank( ))


# Toronto's Crime Map

map_data <- data.frame( mci$MCI, mci$Lat, mci$Long) # Subgrouping a dataframe with 3 variates
colnames( map_data) <- c("Crimes", "Lat", "Lon") # Renaming columns
sbbox <- make_bbox( lon = mci$Long, lat = mci$Lat, f = 0.01)
my_map <- get_map( location = sbbox, maptype = "roadmap", scale = 2, color = "bw", zoom = 9)
ggmap( my_map) +
  geom_point( data=map_data, aes( x = mci$Long, y = mci$Lat, color = "#27AE60"), 
              size = 0.5, alpha = 0.08) + ggtitle("Toronto's Crime Map 2019") + 
  guides( color = FALSE) + facet_wrap( ~ Crimes, nrow = 2) +
  theme( plot.title = element_text( size = 20, face = "bold", hjust = 0.5), # Set font size 20, in bold and centralized
         axis.line = element_blank( ), axis.text = element_blank( ),
         axis.ticks = element_blank( ), axis.title.x = element_blank( ), 
         axis.title.y = element_blank( ), plot.margin = unit( c( 0, 0, -1, -1), "lines"))

# Conclusion

# References


