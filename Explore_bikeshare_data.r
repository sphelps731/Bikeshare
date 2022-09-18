
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

summary(ny)


dim(ny)

library('ggplot2')
names(ny)

# Your solution code goes here

qplot(x = Trip.Duration, data = subset(ny, !is.na(Gender)),
    xlab = "Trip Duration", ylab = "Number of Riders",    
    color = I('black'), fill = I('#099002'), binwidth = 40) +
    scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 1250)) +
    facet_wrap(~Gender)


table(ny$Gender)
by(ny$Trip.Duration, ny$Gender, summary)

#creating the boxplot
qplot(x = Gender, y = Trip.Duration, data = subset(ny, !is.na(Gender)),
     geom = 'boxplot') +
    coord_cartesian(ylim = c(0, 2500))

by(ny$Trip.Duration, ny$Gender, summary)

head(chi)

summary(chi)

library('ggplot2')
names(chi)


library(dplyr)
filter(chi, Birth.Year > 1950)

# Your solution code goes here


save(chi, file = 'chi_filtered.csv')

chi_f = read.csv(file = 'chi_filtered.csv')


# qplot(x = Trip.Duration, data = subset(chi, !is.na(Birth.Year)),
#     xlab = "Trip Duration", ylab = "Number of Riders",    
#     color = I('black'), fill = I('#099002'), binwidth = 40) +
#     scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 1250)) +
#     facet_wrap(~Birth.Year)


# Your solution code goes here

system('python -m nbconvert Explore_bikeshare_data.ipynb')
