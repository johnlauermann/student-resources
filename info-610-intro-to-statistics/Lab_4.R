# Lab 4: Scatterplots & correlation

# we'll use these libraries
library(corrplot)
library(dplyr)
library(ggplot2)
library(here)

#set up your working directory 
here::i_am("Lab_4.r")



# Download and prepare data from the Census
## in my example, I'll test whether there's a correlation between poverty and race

## poverty data come from variable S1701 (https://data.census.gov/table?q=S1701&g=010XX00US$0500000)
poverty <- read.csv("Poverty-ACS20235yr.csv") %>%
  select(GEO_ID, S1701_C03_046E) %>%
  rename(PovertyRate = S1701_C03_046E)

## race data come from variable B02001 (https://data.census.gov/table?q=b02001&g=010XX00US$0500000)
race <- read.csv("Race-ACS20235yr.csv") %>%
  select(GEO_ID, B02001_001E, B02001_002E,
         B02001_003E, B02001_004E, B02001_005E,
         B02001_006E, B02001_007E, B02001_008E) %>%
  rename(Total = B02001_001E,
         AIAN = B02001_004E,
         Asian = B02001_005E,
         Black = B02001_003E,
         NHPI = B02001_006E,
         Other = B02001_007E,
         TwoOrMore = B02001_008E,
         White = B02001_002E)

## calculate racial percentages
### one option is to use within()
race <- within(race, {
  AIANpct <- (AIAN / Total) * 100
  Asianpct <- (Asian / Total) * 100
  Blackpct <- (Black / Total) * 100
  NHPIpct <- (NHPI / Total) * 100
  Otherpct <- (Other / Total) * 100
  TwoOrMorepct <- (TwoOrMore / Total) * 100
  Whitepct <- (White / Total) * 100
})

### another is to use dplyr
race <- race %>%
  mutate(
    AIANpct = (AIAN / Total) * 100,
    Asianpct = (Asian / Total) * 100,
    Blackpct = (Black / Total) * 100,
    NHPIpct = (NHPI / Total) * 100,
    Otherpct = (Other / Total) * 100,
    TwoOrMorepct = (TwoOrMore / Total) * 100,
    Whitepct = (White / Total) * 100
  )


## join the data
### one option is to use merge(). By default, this will merge based on a shared column name.
joined <- merge(poverty, race)

### another is to use the join functions in dplyr, which give you more control (e.g. if the primary key columns are named different)
joined <- left_join(poverty, race, by = "GEO_ID")



# Q1: Create scatterplots
par(mfrow = c(2, 4))
plot(x=joined$AIANpct, 
     y=joined$PovertyRate, 
     main = "Poverty by Race", 
     xlab = "% American Indian or Alaska Native", 
     ylab = "Poverty Rate", 
     col = 'dark red')
plot(x=joined$Asianpct, 
     y=joined$PovertyRate, 
     main = "Poverty by Race", 
     xlab = "% Asian", 
     ylab = "Poverty Rate", 
     col = 'dark red')
plot(x=joined$Blackpct, 
     y=joined$PovertyRate, 
     main = "Poverty by Race", 
     xlab = "% Black or African American", 
     ylab = "Poverty Rate", 
     col = 'dark red')
plot(x=joined$NHPIpct, 
     y=joined$PovertyRate, 
     main = "Poverty by Race", 
     xlab = "% Native Hawiian or Pacific Islander", 
     ylab = "Poverty Rate", 
     col = 'dark red')
plot(x=joined$Otherpct, 
     y=joined$PovertyRate, 
     main = "Poverty by Race", 
     xlab = "% Other", 
     ylab = "Poverty Rate", 
     col = 'dark red')
plot(x=joined$TwoOrMorepct, 
     y=joined$PovertyRate, 
     main = "Poverty by Race", 
     xlab = "% Two or more races", 
     ylab = "Poverty Rate", 
     col = 'dark red')
plot(x=joined$Whitepct, 
     y=joined$PovertyRate, 
     main = "Poverty by Race", 
     xlab = "% White", 
     ylab = "Poverty Rate", 
     col = 'dark red')

## or, you can use ggplot to simplify things a bit
### name the variables
variables <- c("AIANpct", "Asianpct", "Blackpct", "NHPIpct", 
               "Otherpct", "TwoOrMorepct", "Whitepct")

### create a pivot table
long_table <- joined %>%
  select(PovertyRate, all_of(variables)) %>%
  pivot_longer(cols = all_of(variables),
               names_to = "Race",
               values_to = "Percentage") %>%
  mutate(RaceLabel = case_when(
    Race == "AIANpct" ~ "American Indian or Alaska Native",
    Race == "Asianpct" ~ "Asian",
    Race == "Blackpct" ~ "Black or African American",
    Race == "NHPIpct" ~ "Native Hawaiian or Pacific Islander",
    Race == "Otherpct" ~ "Other",
    Race == "TwoOrMorepct" ~ "Two or more races",
    Race == "Whitepct" ~ "White"
  ))

### create the scatterplot matrix
ggplot(long_table, aes(x = Percentage, y = PovertyRate)) +
  geom_point(color = "darkred", alpha = 0.7) +
  facet_wrap(~RaceLabel, scales = "free") +
  labs(title = "Poverty by Race",
       x = NULL,
       y = "Poverty Rate") +
  theme_minimal() 



# Part 2: Calculate correlations and derivative metrics
cor.test(joined$Whitepct, joined$PovertyRate, method = "pearson")
cor.test(joined$Blackpct, joined$PovertyRate, method = "pearson")
cor.test(joined$AIANpct, joined$PovertyRate, method = "pearson")
cor.test(joined$Asianpct, joined$PovertyRate, method = "pearson")
cor.test(joined$NHPIpct, joined$PovertyRate, method = "pearson")
cor.test(joined$Otherpct, joined$PovertyRate, method = "pearson")
cor.test(joined$TwoOrMorepct, joined$PovertyRate, method = "pearson")


# Part 3: Create a correlation matrix
## assemble relevant variables, then transform them into a matrix using corrplot
percentages <- select(joined, PovertyRate, AIANpct, Asianpct, Blackpct,
                      NHPIpct, Otherpct, TwoOrMorepct, Whitepct)
matrix <- cor(percentages, use = "complete.obs")
print(matrix)
corrplot(matrix)

## alternatively, you could use ggplot
### Convert matrix to long format for ggplot
long_matrix <- matrix %>%
  as.data.frame() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation")

### create a heatmap
ggplot(long_matrix, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkblue",
                       midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  labs(title = "Correlation Heatmap",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())
