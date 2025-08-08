library(tidyverse)

NEI <- readRDS("~/R/summarySCC_PM25.rds")
SCC <- readRDS("~/R/Source_Classification_Code.rds")

# 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
pm25_total <- NEI %>%
  group_by(year) %>%
  summarise(Total = sum(Emissions, na.rm = TRUE))

png("./R/plot1.png", width = 480, height = 480, units = 'px')
barplot(pm25_total$Total,
        names.arg = pm25_total$year,
        col = "grey",
        xlab = "Year",
        ylab = "PM2.5 Emissions",
        main = "Total PM2.5 Emissions per Year")
dev.off()

# 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question. 
pm25_city <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year) %>%
  summarise(Total = sum(Emissions, na.rm = TRUE))

png("./R/plot2.png", width = 480, height = 480, units = 'px')
barplot(pm25_city$Total,
        names.arg = pm25_city$year,
        col = "grey",
        xlab = "Year",
        ylab = "PM2.5 Emissions",
        main = "Total PM2.5 Emissions in Baltimore per Year")
dev.off()

# 3
# Of the four types of sources indicated by the type variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
pm25_total_type <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year, type) %>%
  summarise(Total = sum(Emissions, na.rm = TRUE))

plot3 <- ggplot(pm25_total_type, aes(x = pm25_total_type$year, y = pm25_total_type$Total, colour = pm25_total_type$type)) + geom_line() +
  labs(x = "Year", 
       y = "PM2.5 Emission Total",
       title = "Baltimore PM2.5 Emission Total per year",
       colour = "Type"
       )
ggsave(filename = "./R/plot3.png", width = 4, height = 4, plot3)

# 4 
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
coal <- unique(SCC[grepl("coal", SCC$Short.Name, ignore.case = TRUE), "SCC"])
nei_coal <- NEI[NEI$SCC %in% coal, ]

plot4 <- ggplot(nei_coal, aes(x = factor(year), y = Emissions)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", 
       y = "PM2.5 Emission Total",
       title = "Baltimore PM2.5 Emission Total per year -  Coal realted"
  )
ggsave(filename = "./R/plot4.png", width = 4, height = 4, plot4)


# 5
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
vh <- unique(SCC[grepl("vehicles", SCC$Short.Name, ignore.case = TRUE), "SCC"])
nei_vh <- NEI[NEI$SCC %in% vh, ] %>% 
  filter(fips == "24510")

plot5 <- ggplot(nei_vh, aes(x = factor(year), y = Emissions)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", 
       y = "PM2.5 Emission Total",
       title = "Baltimore PM2.5 Emission Total \nin Baltimore per year -  Vehicles"
  )
ggsave(filename = "./R/plot5.png", width = 4, height = 4, plot5)

# 6
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California ( fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
vh <- unique(SCC[grepl("vehicles", SCC$Short.Name, ignore.case = TRUE), "SCC"])

nei_vh_cities <- NEI[NEI$SCC %in% vh, ] %>% 
  filter(fips %in% c("24510", "06037")) %>%
  mutate(City = case_when(fips == "24510" ~ "Baltimore", 
                          fips == "06037" ~ "LA",
                          TRUE ~ "other")) %>%
  group_by(year, City) %>%
  summarise(Total = sum(Emissions, na.rm = TRUE))


plot6 <- ggplot(nei_vh_cities, aes(x = year, y = Total, colour = City)) + 
  geom_line() +
  labs(
    x = "Year", 
    y = "PM2.5 Emission Total",
    title = "Vehicle-Related PM2.5 Emissions\nBaltimore vs LA per Year -  Vehicles",
    colour = "City"
  )
ggsave(filename = "./R/plot6.png", width = 4, height = 4, plot6)

