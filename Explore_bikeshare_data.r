
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

# qplot(x = Trip.Duration, data = ny)


qplot(x = Trip.Duration, data = subset(ny, !is.na(Gender)),
      binwidth = 20) +
    scale_x_continuous(lim = c(0, 500), breaks = seq(0, 500, 100))+
    facet_wrap(~Gender)

# ggplot(aes(x = dob_day), data = fb) +
#   geom_histogram(binwidth = 1) +
#   scale_x_continuous(breaks = 1:31) +
#   facet_wrap(~gender) 



# Your solution code goes here


# Your solution code goes here

system('python -m nbconvert Explore_bikeshare_data.ipynb')
