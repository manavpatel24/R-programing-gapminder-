#Gapminder
#Contains global development data, life expectancy, and population for countries 

install.packages("gapminder")
library(gapminder)
data(gapminder)

gapminder

print(gapminder)

# Mean for life expectancy
mean(gapminder$lifeExp)
# Mean for GDP per capita
mean(gapminder$gdpPercap)



# Median for life expectancy
median(gapminder$lifeExp)
# Median for GDP per capita
median(gapminder$gdpPercap)




# Extract the 'lifeExp' column from the gapminder dataset
lifeExp_vector <- gapminder$lifeExp
# Custom function to calculate mode without data frames
calculate_mode <- function(x) {
  unique_vals <- unique(x)
  counts <- tabulate(match(x, unique_vals))
  mode_val <- unique_vals[which.max(counts)]
  return(mode_val)
}



# Mode for life expectancy
mode_lifeExp <- calculate_mode(lifeExp_vector)
mode_lifeExp
# Mode for GDP per capita
mode_gdpPercap <- calculate_mode(gapminder$gdpPercap)
mode_gdpPercap



# Maximum for life expectancy
max(gapminder$lifeExp)
# Maximum for GDP per capita
max(gapminder$gdpPercap)



# Minimum for life expectancy
min(gapminder$lifeExp)
# Minimum for GDP per capita
min(gapminder$gdpPercap)



# Variance of life expectancy
var_lifeExp <- var(gapminder$lifeExp)
var_lifeExp



# Standard deviation of life expectancy
sd_lifeExp <- sd(gapminder$lifeExp)
sd_lifeExp



#Categorical Data Analysis:
#Frequency Tables: Summarize counts within categories.
table(gapminder$continent)



#Cluster Analysis:
#K-Means Clustering: Group data points into clusters
set.seed(123)
kmeans_data <- gapminder[, c("lifeExp", "gdpPercap")]
kmeans_model <- kmeans(kmeans_data, centers = 3)
kmeans_model$cluster



#Inferential Statistics
#T-Tests: Compare means of two groups.
t.test(gapminder[gapminder$continent == "Asia", "lifeExp"], 
       gapminder[gapminder$continent == "Europe", "lifeExp"])



#ANOVA: Compare means of more than two groups.
anova(lm(lifeExp ~ continent, data = gapminder))

# Count the number of countries in each continent
table(gapminder$continent)

# summary 
summary(gapminder$lifeExp)



#graph 

#pie chart 
# Calculate mean life expectancy for each continent
continent_lifeExp <- aggregate(lifeExp ~ continent, data = gapminder, FUN = mean)
# Create a pie chart
pie(continent_lifeExp$lifeExp, labels = continent_lifeExp$continent, 
    main = "Mean Life Expectancy by Continent")


#bar graph 1
# Get unique country names and their life expectancy values
countries_lifeExp <- aggregate(lifeExp ~ country, data = gapminder, FUN = mean)
# Create a bar plot of life expectancy by country
barplot(countries_lifeExp$lifeExp, names.arg = countries_lifeExp$country, 
        main = "Life Expectancy by Country", xlab = "Country", ylab = "Life Expectancy", 
        col = "skyblue", las = 2, cex.names = 0.6)




#bar graph 2
# Count the number of countries in each continent
country_count <- table(gapminder$continent)
# Create a vertical bar plot
barplot(country_count, main = "Number of Countries in Each Continent", xlab = "Continent", ylab = "Count", col = "skyblue")




# Histogram with color
hist(gapminder$lifeExp, col = "skyblue", border = "black")


# Boxplot with color
boxplot(gapminder$lifeExp, col = "lightgreen")


# Create a scatter plot for life expectancy vs. GDP per capita
plot(gapminder$gdpPercap, gapminder$lifeExp, xlab = "GDP per Capita", ylab = "Life Expectancy", main = "Life Expectancy vs. GDP per Capita")














