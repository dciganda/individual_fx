# DESCRIPTIVES -----
shp <- read.csv(file.path("..", "data","final", "HT", "out", "sheps.csv"))
plot(shp$age, shp$fx/100)

# EATON
age <- c(17,22, 27, 32, 37, 42, 47)
fx <- c(366.7,405.9, 451.8,415.2, 355.0,238.9,23.5)
plot(age,fx/1000, col = "red")

# Eaton data is strange. ASFR are lower than Sheps for all groups. Also the peak is in the group 25-29,
# While in Shpes and our own results peak is clearly on ages 20-24.

# Robinson 1986 (sourece is EATON and MAYER 1954, cannot find this data on that article though)
age <- c(22, 27, 32, 37, 42, 47)
fx <- c(550,502,447,406, 222,61)

lines(age, fx/1000)


# COALE 1971

fx <- c(0.550, 0.502, 0.447, 0.406, 0.222, 0.061)
age <- c(22, 27, 32, 37, 42, 47)
plot(age,fx, col = "red")
