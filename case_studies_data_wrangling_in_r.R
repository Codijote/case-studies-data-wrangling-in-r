# 1. Data Wrangling Case Study: Coal Consumption --------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

source_coal <- readr::read_csv('./data/coal.csv', skip = 2)

glimpse(source_coal)

head(source_coal)

tidy_coal <- tidyr::pivot_longer(source_coal, cols = !1, values_to = 'Coal', names_to = 'Year')

glimpse(tidy_coal)
head(tidy_coal)
names(tidy_coal)[1] = 'Country'

continents <- c('North America', 'Central America', 'South America', 'Europe', 'Africa',
                'Asia', 'Oceania', 'Central & South America', 'Middle East', 'Asia & Oceania',
                'World', 'Antartica', 'Eurasia')

continents_coal <- tidy_coal |> filter(Country %in% continents)
countries_coal <- tidy_coal |> filter(Country %in% continents)

tidy_coal <- tidy_coal |> mutate(Year = as.integer(Year))
tidy_coal <- tidy_coal |> mutate(Coal = as.numeric(Coal))
summary(tidy_coal)

unique(tidy_coal$Country)

coal_region <- tidy_coal |> 
  filter(Country %in% continents)
unique(coal_region$Country)

coal_country <- tidy_coal |> 
  filter(!(Country %in% continents))

coal_world <- tidy_coal |> filter(Country == 'World')
head(coal_world)

coal_region <- coal_region |> 
  filter(Country != 'World')
summary(coal_region)
unique(coal_region$Country)

ggplot(coal_region, aes(Year, Coal)) +
  geom_line(aes(colour = Country))


# 2. Data Wrangling Case Study: Water Quality -----------------------------

suppressMessages({library(lubridate)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(tidyr)}
)

source_water <- readr::read_csv('./data/austinwater.csv')

glimpse(source_water)

bench::mark(
  water <- source_water |> 
    select(SITE_NAME, SITE_TYPE, SAMPLE_DATE, PARAM_TYPE, PARAMETER, RESULT, UNIT),
  
  water <- source_water[ , c('SITE_NAME', 'SITE_TYPE', 'SAMPLE_DATE', 'PARAM_TYPE', 'PARAMETER', 'RESULT', 'UNIT')]
)

# Benchmark shows that base R is slightly faster than dplyr function for this data and used aprox. 1/6 of the memory.

object.size(water)
object.size(source_water)

system.time(
  water <- water |> 
    rename(siteName = SITE_NAME, siteType = SITE_TYPE, sampleDate = SAMPLE_DATE,
           parameterType = PARAM_TYPE, result = RESULT, unit = UNIT)
)

system.time(
  colnames(water) <- stringr::str_to_title(colnames(water))
)

# Comparison shows base R takes 1/4 of the time for column formatting with less code typed.

glimpse(water)

# Trimming extensive data down to have only pH and water temperature.

unique(water$Parameter)

water |> filter(str_detect(Parameter, 'PH')) |> 
  select(Parameter) |> 
  unique()

unique(water$Parametertype)

filtered_water <- water |> filter(Parametertype == 'Alkalinity/Hardness/pH' |
                                    Parametertype == 'Conventionals')

glimpse(filtered_water)
unique(filtered_water$Parameter)

bench::mark(
  
  filtered_water <- water |> filter(Parameter == 'PH' |
                                      Parameter == 'WATER TEMPERATURE'),
  
  filtered_water <- water[water$Parameter == 'PH' |
                            water$Parameter == 'WATER TEMPERATURE' , ])

# Benchmark shows that base R is slightly faster than tidyverse method using roughly have of the memory.

# Converting data types to appropriate formats.

summary(filtered_water)

filtered_water <- filtered_water |>
  mutate(Sitetype = as.factor(Sitetype),
         Parametertype = as.factor(Parametertype),
         Parameter = as.factor(Parameter),
         Unit = as.factor(Unit))

glimpse(filtered_water)
summary(filtered_water)

# Transforming dates to get statistics from a correct evaluation

filtered_water <- filtered_water |> 
  mutate(sampleDate = mdy_hms(sampleDate))

summary(filtered_water)

# Correcting data entry errors

summary(filtered_water)

# Units show temperature has been measured in Celsius and Fehrenheit, there are some Feet and MG/L.

bench::mark(
  filtered_water |> filter(Unit == "Feet"),
  filtered_water[filtered_water$Unit == 'Feet', ]
)

# Base R consumes less memory and gives a slightly best performance.

filtered_water <- filtered_water |> mutate(
  Unit = recode(Unit, 'Feet' = 'Deg. Fahrenheit'))

summary(filtered_water)

# Units shown as MG/L can be removed.

filtered_water <- filtered_water |> 
  filter(!Unit == 'MG/L')

filtered_water <- filtered_water |> 
  mutate(Unit = droplevels(Unit))

summary(filtered_water)

# Identifying and removing outliers

ggplot(filtered_water, mapping = aes(x = sampleDate, y = Result)) +
  geom_point()

# One outlier found, will be removed

filter(filtered_water, Result > 1e6)
filtered_water <- filtered_water |> 
  filter(Result < 1e6)
summary(filtered_water)

# Still we have results over 1000 that don't seem correct, will be removed.

filtered_water <- filtered_water |> 
  filter(Result < 1e3)
summary(filtered_water)

ggplot(filtered_water, aes(Unit, Result)) +
  geom_boxplot()

# Two temperature in celsius over 60 degree can be actual fahrenheit with wrong unit.

filtered_water <- filtered_water |>
  mutate(Unit = as.character(Unit)) |> 
  mutate(Unit = ifelse((Unit == 'Deg. Celsius' & Result > 60), 'Deg. Fahrenheit', Unit)) |> 
  mutate(Unit = as.factor(Unit))

summary(filtered_water)
ggplot(filtered_water, aes(Unit, Result)) +
  geom_boxplot()

# Converting temperature from Fahrenheit to Celsius

fahrenheit <- which(filtered_water$Unit == 'Deg. Fahrenheit')

filtered_water$Result[fahrenheit] <- 
  (filtered_water$Result[fahrenheit] - 32) * (5/9)

ggplot(filtered_water, aes(Unit, Result)) +
  geom_boxplot()

filtered_water$Unit[fahrenheit] <- 'Deg. Celsius'

ggplot(filtered_water, aes(Unit, Result)) +
  geom_boxplot()

summary(filtered_water)

filtered_water$Unit <- droplevels(filtered_water$Unit)

summary(filtered_water)

# Moving temperature and PH measure to their own columns

filtered_water <- filtered_water |> 
  select(-Parametertype, -Unit)

filtered_water_wide <- pivot_wider(filtered_water,
                                   names_from = Parameter,
                                   values_from = Result)

dupe_check <- filtered_water[, -5]
duplicated(dupe_check)
dupes <- which(duplicated(dupe_check))

filtered_water <- filtered_water[-dupes, ]

filtered_water_wide <- pivot_wider(filtered_water,
                                   names_from = Parameter,
                                   values_from = Result)

filtered_water_wide <- filtered_water_wide |> 
  rename(pH = PH, Temperature = 'WATER TEMPERATURE')

summary(filtered_water_wide)

# Some NAs in the values can still be improved, maybe a chance to use Mice.


# 3. Data Wrangling Case Study: Social Security Disability ----------------

suppressMessages({library(lubridate)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(tidyr)}
)

ssa <- readr::read_csv('./data/ssadisability.csv')

glimpse(ssa)

# Making dataset long

ssa_long <- pivot_longer(ssa,
                         !Fiscal_Year,
                         names_to = 'month',
                         values_to = 'applications')

head(ssa_long)

# Formatting dates

unique(ssa_long$month)

ssa_long <- ssa_long |> 
  separate(month, c('month', 'application_method'), sep = "_")

ssa_long <- ssa_long |> 
  mutate(month = substr(month, 1, 3))

ssa_long <- ssa_long |> 
  mutate(Fiscal_Year = str_replace(Fiscal_Year, 'FY', '20'))

ssa_long <- ssa_long |> 
  mutate(date = dmy(paste('01', ssa_long$month, ssa$Fiscal_Year)))

head(ssa_long)

# Adjusting Fiscal Years to Calendar Years

ssa_long <- ssa_long %>%
  mutate(Fiscal_Year = as.numeric(Fiscal_Year)) |> 
  mutate(Fiscal_Year = ifelse(month(date) >= 10, Fiscal_Year - 1, Fiscal_Year)) |> 
  mutate(date = dmy(paste("01", month, Fiscal_Year)))

# Widening the social security disability dataset

summary(ssa_long)

ssa_long <- ssa_long |> 
  select(-Fiscal_Year, -month)

ssa_long <- ssa_long |> 
  mutate(application_method = as.factor(application_method))

summary(ssa_long)

ssa <- pivot_wider(ssa_long, names_from = application_method, values_from = applications)

head(ssa)

print(ssa, n = 20)

# Visualizing the social security disability dataset

ssa <- ssa |> 
  mutate(online_percentage = Internet / Total * 100)

ggplot(ssa, aes(date, online_percentage)) +
  geom_point()

ggplot(ssa) +
  geom_point(aes(date, Total), color = 'blue') +
  geom_point(aes(date, Internet), color = 'green')

ggplot(ssa) +
  geom_area(aes(date, Total), color = 'lightblue', fill = 'lightblue') +
  geom_area(aes(date, Internet), color = 'green', fill = 'green') +
  ggtitle('Internet applications are a major proportion every year') +
  annotate('text', as.Date('2014-01-01'), 75000, label = 'Applications over the internet')
