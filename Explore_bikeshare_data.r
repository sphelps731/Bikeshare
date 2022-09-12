
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

# summary(ny)


# head(wash)

# head(chi)

dim(ny)

library('ggplot2')
names(ny)

# Your solution code goes here

# qplot(x = Trip.Duration, data = ny)


qplot(x = duration, data = subset(ny, !is.na(gender)),
      binwidth = 10) +
    scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 100, 10))+
    facet_wrap(~gender)



# Your solution code goes here


# Your solution code goes here

system('python -m nbconvert Explore_bikeshare_data.ipynb')
