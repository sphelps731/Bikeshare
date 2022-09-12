
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

summary(ny)


# head(wash)

# head(chi)

dim(ny)

library('ggplot2')
names(ny)

# Your solution code goes here

qplot(x = Trip.Duration, data = subset(ny, !is.na(Gender)),
      binwidth = 20) +
    scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 250))+
    facet_wrap(~Gender)


table(ny$Gender)
by(ny$Trip.Duration, ny$Gender, summary)

# Your solution code goes here


# Your solution code goes here

system('python -m nbconvert Explore_bikeshare_data.ipynb')
