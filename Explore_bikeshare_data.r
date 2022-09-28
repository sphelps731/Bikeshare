
# Reading in the data

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

# Reviewing the top five rows for NY and Chicago

head(ny, 5)
head(chi, 5)

#Viewing the summary statistics and dimension for the NY dataset

summary(ny)
dim(ny)

#Viewing the summary statistics and dimension for the Chicago dataset
summary(chi)
dim(chi)

# The column names in the two datasets

names(ny) 
names(chi)

# Loading the ggplot library containing our plot functions

library('ggplot2')

# The qplot function - New York

qplot(x = Trip.Duration, data = subset(ny, !is.na(Gender)), # omitting NA values
    xlab = "Trip Duration By Gender - New York", ylab = "Number of Riders",    
    main = "Trip Durations For Males and Females - New York",
    color = I('black'), fill = I('#099002'), binwidth = 40) +
    scale_x_continuous(breaks = seq(0, 50, 2500), lim = c(0, 2500)) +
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
    scale_x_continuous(breaks = seq(0, 50, 2500), lim = c(0, 2500)) +
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
    scale_x_continuous(breaks = seq(0, 50, 2500), lim = c(0, 2500)) 
    

names(chi)

head(chi)

summary(chi)

library('ggplot2')


library(dplyr)
filter(chi, Birth.Year > 1950)


# Saving the filtered dataset to a new dataframe.

chi.to_csv('chi_filtered.csv')
write.csv(chi, 'chi_filtered.csv', row.names=FALSE)
chi_f = read.csv('chi_filtered.csv')
head(chi_f)


# qplot(x = Trip.Duration, data = subset(chi_f, Birth.Year > 1949 & Birth.Year < 1999),
#     xlab = "Trip Duration", ylab = "Number of Riders",    
#     color = I('black'), fill = I('#099002'), binwidth = 40) +
#     scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 1250)) +
#     facet_wrap(~Birth.Year)

# head(chi_f)
names(chi_f)
# summary(chi_f)
dim(chi_f)

qplot(x = Trip.Duration, data = subset(chi_f, Birth.Year > 1949 & Birth.Year < 1999),
    xlab = "Trip Duration", ylab = "Number of Riders",    
    color = I('black'), fill = I('#099002'), binwidth = 40) +
    scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 1250)) +
    facet_wrap(~Birth.Year)



# Your solution code goes here
#function, loop
# end_stat_uniq <- unique(wash$End.Station)


end_stat_sum <- summary(wash$End.Station)

# end_stat_sum <- data.frame(Station=c(summary(wash$End.Station)))

x = summary(wash$End.Station)

# head(end_stat_sum)

# last_four <- 4

# end_stat_sum$Count = substr(x, nchar(x) - last_four + 1, nchar(x))


# dim(end_stat_sum)

head(end_stat_sum)

# substr(end_stat_sum$Station, 1, 4)

# library(stringr)
# n <- c(end_stat_sum$Station)
# str_sub(n,1,nchar(n)-100)

# head(end_stat_sum)


wash_es <- popular_stations(wash$End.Station, 10)
 

def popular_stations(for i in wash_es, 10) {
    
    wash[order(wash_es)]
        
    
     return wash_es
         
}
         
     
    
   
    





system('python -m nbconvert Explore_bikeshare_data.ipynb')
