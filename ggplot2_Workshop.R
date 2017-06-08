## Workshop on ggplot2
# Grammar of graphics - can use a few verbs in different ways to create plots
# data + coordinates [mapping] + geometric shapes = plot

## References

# Cheatsheets: https://www.rstudio.com/resources/cheatsheets/
# Documentation: http://ggplot2.tidyverse.org/reference/
# Tidy data: https://www.jstatsoft.org/article/view/v059i10
# Layered grammar of graphics: https://www.uvm.edu/~ngotelli/Rscripts/wickham.2009.pdf

# Load libraries
library(tidyverse)
library(lubridate)
library(GGally)
library(plotly)
library(scales)

# Load data from last workshop and clean data.frame so that only original pokemon are used
df <- read_rds("pkmn - final data.rds") %>%
  filter(generation == 1) %>%
  rename(special_attack = `special-attack`,
         special_defense = `special-defense`)

# What chart to use?
# One variable = geom_histogram (discrete or cont binned), geom_density (continuous)
ggplot(data = df, mapping = aes(x = Type1)) + geom_bar() # Discrete
ggplot(data = df, mapping = aes(x = Type1)) + geom_bar() + coord_flip() # Discrete - coord_flip
ggplot(data = df, mapping = aes(x = speed)) + geom_histogram() # Continuous
ggplot(data = df, mapping = aes(x = speed)) + geom_density() # Continuous
# Cont vs cont = geom_point, geom_smooth
ggplot(data = df, mapping = aes(x = attack, y = defense)) + geom_point()
ggplot(data = df, mapping = aes(x = attack, y = defense)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# Discrete vs cont = geom_boxplot
ggplot(data = df, mapping = aes(x = Type1, y = hp)) + geom_boxplot()
# Time series = geom_line - Note: Someone correctly pointed out that this plot is in the incorrect order [not fixed]
ggplot(data = df, mapping = aes(x = paste(year(date_caught), month(date_caught), sep = "-"), group = 1)) +
  geom_line(stat = "count")

df %>%
  mutate(year = year(date_caught), month = month(date_caught)) %>%
  mutate(year_month = paste(year, month, sep = "-")) %>%
  group_by(year_month) %>%
  summarise(total = n()) %>%
  ggplot(mapping = aes(x = year_month, y = total, group = 1)) + geom_line()

# What if you want to break your plot up by groups?
# Attack vs. Defense
ggplot(data = df, aes(x = attack, y = defense)) + 
  geom_point() + 
  geom_smooth(se = FALSE, method = "lm")

# Group/color or facet by Type1
ggplot(data = df, aes(x = attack, y = defense, color = Type1)) + 
  geom_point() + 
  geom_smooth(se = FALSE, method = "lm")

# Facet
ggplot(data = df, aes(x = attack, y = defense, color = Type1)) + 
  geom_point() + 
  geom_smooth(se = FALSE, method = "lm") + 
  facet_grid(. ~ Type1)

# Plotly
g <- ggplot(data = df, aes(x = attack, y = defense, color = Type1)) + 
  geom_point() + 
  geom_smooth(se = FALSE, method = "lm")

ggplotly(g, width = 1500, height = 800)

# ggpairs
df %>%
  select(-pokemon_id, -species_id, -name, -Type1, -Type2, -starter, -legendary) %>%
  ggpairs()

# Make a beautiful chart
df2 <- df %>%
  filter(Type1 %in% c("fire", "grass", "water")) %>%
  select(attack, defense, Type1) %>%
  drop_na()

ggplot(data = df2) + 
  geom_point(mapping = aes(x = attack, y = defense, color = Type1), size = 2) + 
  geom_smooth(mapping = aes(x = attack, y = defense), se = FALSE, method = "lm") + 
  ylab("Attack") + 
  xlab("Defense") +
  ggtitle("Original Pokemon Attack vs. Defense") + 
  geom_hline(yintercept = 125, color = "red", linetype = "longdash") + 
  annotate("text", x = 50, y = 130, color = "red", label = "Outliers above") + 
  annotate("text", x = 20, y = 100, fontface = "bold", 
           label = paste("Correlation Coefficient =", round(cor(select(df2, attack), select(df2, defense)),2 )))

# Remove the outlier and re-plot
df2 <- df2 %>%
  filter(defense < 150)

# ggplot2: Position argument - Used to arrange geoms that would occupy the same space
df %>%
  filter(Type1 %in% c("fire", "grass", "water")) %>%
  ggplot(data = ., mapping = aes(x = year(date_caught), y = speed, fill = Type1)) +
  geom_bar(stat = "identity", position = "stack")

df %>%
  filter(Type1 %in% c("fire", "grass", "water")) %>%
  ggplot(data = ., mapping = aes(x = year(date_caught), y = speed, fill = Type1)) +
  geom_bar(stat = "identity", position = "dodge")

df %>%
  filter(Type1 %in% c("fire", "grass", "water")) %>%
  ggplot(data = ., mapping = aes(x = year(date_caught), y = speed, fill = Type1)) +
  geom_bar(stat = "identity", position = "fill")

df %>%
  filter(Type1 %in% c("fire", "grass", "water")) %>%
  ggplot(data = ., mapping = aes(x = year(date_caught), y = speed, fill = Type1)) +
  geom_bar(stat = "identity", position = "fill") + 
  scale_y_continuous(labels = percent)

# ggplot2: Stat argument - Used to create new variables

df %>%
  ggplot(data = ., mapping = aes(x = attack)) + 
  geom_bar(stat = "bin")

df %>%
  ggplot(data = ., mapping = aes(x = attack)) + 
  geom_histogram()

df %>%
  ggplot(data = ., mapping = aes(x = attack)) + 
  geom_bar(stat = "bin", bins = 10)

df %>%
  ggplot(data = ., mapping = aes(x = attack)) + 
  geom_bar(stat = "count")

df %>%
  ggplot(data = ., mapping = aes(x = attack)) + 
  geom_histogram(stat = "bin", binwidth = 1)

df %>%
  group_by(attack) %>%
  summarise(count = n()) %>%
  ggplot(data = ., mapping = aes(x = attack, y = count)) +
  geom_bar(stat = "identity")
