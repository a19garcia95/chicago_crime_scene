#DATA WRANGLING AND CLEANING 

crimes_01_to_04_raw <- read.csv("Chicago_Crimes_2012_to_2017.csv", stringsAsFactors = FALSE)

is.na(crimes_01_to_04_raw)

crimes_01_to_04 <- na.omit(crimes_01_to_04_raw)

#Columns to change: Date (maybe), Primary Type, Location Description, Arrest, Domestic, 

crimes_01_to_04$Primary.Type <- as.factor(crimes_01_to_04$Primary.Type) #29 Levels
crimes_01_to_04$Description <- as.factor(crimes_01_to_04$Description) #339 Levels
crimes_01_to_04$Location.Description <- as.factor(crimes_01_to_04$Location.Description) #145 Levels
crimes_01_to_04$IUCR <- as.factor(crimes_01_to_04$IUCR) #353 Levels

crimes_01_to_04$Arrest[which(crimes_01_to_04$Arrest == "True")] <- 1
crimes_01_to_04$Arrest[which(crimes_01_to_04$Arrest == "False")] <- 0

crimes_01_to_04$Domestic[which(crimes_01_to_04$Domestic == "True")] <- 1
crimes_01_to_04$Domestic[which(crimes_01_to_04$Domestic == "False")] <- 0

#STATISTICAL ANALYSIS

install.packages('Hmisc')
library(Hmisc)
describe(crimes_01_to_04$Primary.Type)
#Highest 5: Ritualism, Sex offense, Stalking, Theft, and Weapons Violation
#Lowest 4: Battery, Burglary, Criminal Sexual Assault, Criminal Damage
describe(crimes_01_to_04$Description)
#Highest 5: Vio Bail Bond: Dom Violence, Viol Charitable Game Act, Violate order of protection, Wireroom/horses, Wireroom/sports
#Lowest 5: $300 and Under, $500 and Under, Abuse/neglect: care facility, adultry, agg crim sex abuse fam member
describe(crimes_01_to_04$IUCR)
#Highest 5: 502R, 502T, 5093, 9901, Block
#Lowest 5: 0110, 0141, 0142, 0261, 0262
describe(crimes_01_to_04$Location.Description)
#Highest 5: Vestibule, Warehouse, Wooded Area, Yard, YMCA
#Lowest 5: Abandoned building and anypart of the airport (non terminal, terminal, secure area, etc)
describe(crimes_01_to_04$Beat)
#Highest 5: 932, 933, 934, 935, Domestic
#Lowest 5: 1011, 1012, 1013, 1014, 1021
describe(crimes_01_to_04$District)
#Highest 5: 6.0, 7.0, 8.0, 9.0, Beat
#Lowest 5: 1.0, 10.0, 11.0, 12.0, 14.0

#Which year had the highet amount of crimes commited?
describe(crimes_01_to_04$Year)
#2001 = 568,517 crimes 
#2002 = 490,879 crimes
#2003 = 475,913 crimes
#2004 = 388,205 crimes

#By what percentage does the number of crimes decrease per year?


attach(crimes_01_to_04)
mytable <- table(Primary.Type)
plot(mytable)
#do it with ggplot, flipcord() function
#object.size() function to find memory
#How to read a frequency table?


xx <- as.numeric(crimes_01_to_04$Arrest)
x1 <- xx[1:100]
yy <- as.numeric(crimes_01_to_04$Domestic)
y1 <- yy[1:100]
cor(x1,y1)
#What type of correlation should we use? Only numeric? Can it work with levels?

cor(crimes_01_to_04, use = "complete.obs", method = "spearman")
#What is the difference between pearson, spearman, and kendall?



t.test(xx ~ yy)
#Understanding t-tests and why are they important?

fit <- lm(x1 ~ y1, data = crimes_01_to_04)
summary(fit)
plot(fit)
#can linear regressions exist with categorical variables even if they have levels?

#calculate crime rate



install.packages('ggplot2')

library(ggplot2)

a <- ggplot(crimes_01_to_04, aes(Primary.Type))
a + geom_histogram(stat = "count") + coord_flip()


b <- ggplot(crimes_01_to_04, aes(IUCR))
b + geom_density() + facet_grid(~series)

sort(table(crimes_01_to_04$IUCR),decreasing=TRUE) #to find which ones are the most common values


#Pie chart
ggplot(crimes_01_to_04,
       aes(x = factor(""), fill = Year) ) +
  geom_bar() +
  coord_polar(theta = "y") +
  scale_x_discrete("")

ggplot(crimes_01_to_04, aes(Year, fill = Year ) ) +
  geom_bar() + coord_flip()

ggplot(crimes_01_to_04, aes(Year)) +
  geom_density()



crimes_01_to_04$Latitude <- na.omit(crimes_01_to_04$Latitude)
crimes_01_to_04$Longitude <- na.omit(crimes_01_to_04$Longitude)

ggplot(data = crimes_01_to_04) + 
  geom_point(mapping = aes(x = Primary.Type, y = Location.Description))






library(ggplot2)
data(crimes_01_to_04, package="ggplot2") # alternate source: "http://goo.gl/uEeRGu")
theme_set(theme_bw())  # pre-set the bw theme.

g <- ggplot(crimes_01_to_04, aes(Arrest, Domestic))

# Scatterplot
g + geom_point() + 
  geom_smooth(method="lm", se=F) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="domestics", 
       x="arrest", 
       title="Scatterplot with overlapping points", 
       caption="Source: midwest")




cor(crimes_01_to_04$Location.Description, crimes_01_to_04$Primary.Type)



numeric_arrest <- as.numeric(crimes_01_to_04$Arrest)

fit <- aov(numeric_arrest ~ IUCR, data = crimes_01_to_04)
#plot(fit)
summary(fit)

boxplot(crimes_01_to_04$Arrest, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(crimes_01_to_04$Arrest)$out))  # box plot for 'speed'

#MAPS


install.packages('ggplot2')
install.packages('devtools')
install.packages('dplyr')
install.packages('stringr')
install.packages('maps')
install.packages('mapdata')


devtools::install_github("dkahle/ggmap")

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

usa <- map_data("usa")
w2hr <- map_data("world2Hires")
states <- map_data("state")
west_coast <- subset(states, region %in% c("illinois", "oregon", "washington"))
illinois <- subset(states, region %in% c("illinois"))

ggplot(data = illinois) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3)



il_df <- subset(states, region == "illinois")
counties <- map_data("county")
il_county <- subset(counties, region == "illinois")


il_base <- ggplot(data = il_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")
il_base + theme_nothing()


il_base + theme_nothing() +
  geom_polygon(data = il_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)

require(ggplot2)
require(ggmap)
require(maps)
LA <- map_data("state", region="illinois")

salesCalls <- data.frame(State=rep("illinois",5), 
                         City=c("Chicago"),
                         Calls=c(10,5,8,13,2))

salesCalls <- cbind(geocode(as.character(salesCalls$City)), salesCalls)

salesCalls
#         lon      lat     State        City Calls
# 1 -91.14032 30.45828 louisiana Baton Rouge    10
# 2 -90.07153 29.95107 louisiana New Orleans     5
# 3 -93.75018 32.52515 louisiana  Shreveport     8
# 4 -92.01984 30.22409 louisiana   Lafayette    13
# 5 -90.06563 30.35825 louisiana  Mandeville     2

ggplot(LA, aes(x=long, y=lat)) +
  geom_polygon() +
  coord_map() +
  geom_point(data=salesCalls, aes(x=lon, y=lat, size=Calls), color="orange")

class(crimes_01_to_04$Year)


