
# Reading in the data

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

# Reviewing the top five rows for the three cities
head(ny, 5)
head(chi, 5)
head(wash)

#Viewing the summary statistics and dimension for the NY dataset
summary(ny)
dim(ny)

#Viewing the summary statistics and dimension for the Chicago dataset
summary(chi)
dim(chi)

#Viewing the summary statistics and dimension for the Washington dataset
summary(wash)
dim(wash)

# The column names in the each dataset

names(ny) 
names(chi)
names(wash)

# Loading the ggplot library containing our plot functions

library('ggplot2')

# The qplot function - New York

qplot(x = Trip.Duration, data = subset(ny, !is.na(Gender)), # omitting NA values
    xlab = "Trip Duration By Gender - New York", ylab = "Number of Riders",    
    main = "Trip Durations For Males and Females - New York",
    color = I('black'), fill = I('#099002'), binwidth = 40) +
    scale_x_continuous(breaks = seq(0, 2500, 500), lim = c(0, 2500)) +
    facet_wrap(~Gender)


# Looking at summary statistics - New York

table(ny$Gender)
by(ny$Trip.Duration, ny$Gender, summary)

#Creating the boxplot - New York

qplot(x = Gender, y = Trip.Duration, data = subset(ny, !is.na(Gender)),
      main = "Boxplot Comparisons by Gender - New York", xlab = "Gender - New York", geom = 'boxplot') +
    coord_cartesian(ylim = c(0, 2500))

#New York
table(ny$Gender)
by(ny$Trip.Duration, ny$Gender, summary)

# The qplot function - Chicago

qplot(x = Trip.Duration, data = subset(chi, !is.na(Gender)), # omitting NA values
    xlab = "Trip Duration By Gender - Chicago", ylab = "Number of Riders",    
    main = "Trip Durations For Males and Females - Chicago",
    color = I('black'), fill = I('5938879'), binwidth = 40) +
    scale_x_continuous(breaks = seq(0, 2500, 500), lim = c(0, 2500)) +
    facet_wrap(~Gender)


# Looking at summary statistics - Chicago

table(chi$Gender)
by(chi$Trip.Duration, chi$Gender, summary)

#Creating the boxplot - Chicago

qplot(x = Gender, y = Trip.Duration, data = subset(chi, !is.na(Gender)),
     main = "Boxplot Comparisons by Gender - Chicago", xlab = "Gender - Chicago", geom = 'boxplot') +
    coord_cartesian(ylim = c(0, 2500))

# The qplot function - Washington

qplot(x = Trip.Duration, data = wash, 
    xlab = "Trip Duration - Washington", ylab = "Number of Riders",    
    main = "Trip Durations For Riders in Washington",
    color = I('black'), fill = I('#199434'), binwidth =20) +
    scale_x_continuous(breaks = seq(0, 2500, 500), lim = c(0, 2500)) 
    

head(chi, 5)

dim(chi)
summary(chi)

#loading ggplot2 and the dplyr libraries
library(dplyr)
library('ggplot2')

#filter function
filter(chi, Birth.Year > 1950)


# Saving the filtered dataset to a new dataframe.

write.csv(chi, 'chi_filtered.csv', row.names=FALSE)
chi_f = read.csv('chi_filtered.csv')
head(chi_f)

#the qplot function
qplot(x = Trip.Duration, data = subset(chi_f, Birth.Year > 1949 & Birth.Year < 1999),
    xlab = "Trip Duration", ylab = "Number of Riders",    
    color = I('black'), fill = I('#099002'), binwidth = 40) +
    scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 1250)) +
    facet_wrap(~Birth.Year)



#loading ggplot2 and the dplyr libraries
library(dplyr)
library('ggplot2')

#filter function
filter(ny, Birth.Year > 1950)

# Saving the filtered dataset to a new dataframe.

write.csv(ny, 'ny_filtered.csv', row.names=FALSE)
ny_f = read.csv('ny_filtered.csv')
head(ny_f)

#the qplot function
qplot(x = Trip.Duration, data = subset(ny_f, Birth.Year > 1949 & Birth.Year < 1999),
    xlab = "Trip Duration", ylab = "Number of Riders",    
    color = I('black'), fill = I('#099002'), binwidth = 40) +
    scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 1250)) +
    facet_wrap(~Birth.Year)

#function that returns top ten most visited end stations in Washington, Chicago, and New York

#loading libraries
library(ggplot2)
library("stringr")
library(dplyr)

#function definition
popular_stations_wash <- function(df) { 
    
    #code body of objects and variables
    df <- transform(df, freq= ave(seq(nrow(df)), End.Station, FUN=length))    
    df_new = data.frame(df$End.Station, df$freq)
    df_grp <- df_new %>% group_by(df$End.Station)    
    df_grp = df_grp[order(-df_grp$df.freq), ]
    df_top_ten = unique(df_grp, decreasing = TRUE)    
    df_final <- head(df_top_ten, 10)
     
    #barplot      
    ggp_final <- ggplot(df_final, aes(x = reorder(df_final$df.End.Station, +df_final$df.freq), y = df_final$df.freq)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      ggtitle("Top Ten Most Visited End Stations in Washington") +
      theme(axis.text = element_text(size = 6)) +
      theme(axis.title = element_text(size = 12)) +
      theme(plot.title = element_text(size = 15)) +        
      labs(x = "Bikeshare End Stations", y = "Frequency of Visits") +                        
      geom_bar(stat = "identity", fill = "dark red", width = 0.50)
      ggp_final + coord_flip()
}
#Washington data end station y-axis text (street names) were long so I used a string wrap function,
#from the stringr library that modifies the text to multiple lines.
#https://statisticsglobe.com/r-str_wrap-function-stringr-package                       

#function definition
popular_stations_ny <- function(df) { 
    
    #code body of objects and variables
    df <- transform(df, freq= ave(seq(nrow(df)), End.Station, FUN=length))    
    df_new = data.frame(df$End.Station, df$freq)
    df_grp <- df_new %>% group_by(df$End.Station)    
    df_grp = df_grp[order(-df_grp$df.freq), ]
    df_top_ten = unique(df_grp, decreasing = TRUE)    
    df_final <- head(df_top_ten, 10)
     
    #barplot     
    ggp_final <- ggplot(df_final, aes(x = reorder(df_final$df.End.Station, +df_final$df.freq), y = df_final$df.freq)) +
      ggtitle("Top Ten Most Visited End Stations in New York") +
      theme(axis.title = element_text(size = 12)) +
      theme(plot.title = element_text(size = 15)) +  
      labs(x = "Bikeshare End Stations", y = "Frequency of Visits") +    
      geom_bar(stat = "identity", fill = "dark blue", width = 0.50)
      ggp_final + coord_flip()
}
                       
#function definition                       
popular_stations_chi <- function(df) { 
    
    #code body of objects and variables
    df <- transform(df, freq= ave(seq(nrow(df)), End.Station, FUN=length))    
    df_new = data.frame(df$End.Station, df$freq)
    df_grp <- df_new %>% group_by(df$End.Station)    
    df_grp = df_grp[order(-df_grp$df.freq), ]
    df_top_ten = unique(df_grp, decreasing = TRUE)    
    df_final <- head(df_top_ten, 10)
     
    #barplot    
    ggp_final <- ggplot(df_final, aes(x = reorder(df_final$df.End.Station, +df_final$df.freq), y = df_final$df.freq)) +
      ggtitle("Top Ten Most Visited End Stations in Chicago") +
      theme(axis.title = element_text(size = 12)) +
      theme(plot.title = element_text(size = 15)) +  
      labs(x = "Bikeshare End Stations", y = "Frequency of Visits") +
      geom_bar(stat = "identity", fill = "dark green", width = 0.50)
      ggp_final + coord_flip()
}
                    

#calling the functions
popular_stations_wash(wash)
popular_stations_chi(chi)
popular_stations_ny(ny)    

system('python -m nbconvert Explore_bikeshare_data.ipynb')
