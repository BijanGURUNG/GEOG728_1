# Tuesday, 02/08/2022
# Practice_4

#---------------------------------------------------------------------------#

# Thursday, 02/10/2022
# Practice_5

df1 <- read.csv("C:/Users/bijangurung/Documents/GEOG728_1/KS_BEA.csv")

head(df1)
summary(df1)
dim(df1)
colnames(df1)

library(tidyverse)

glimpse(df1)
head(df1)

View(df1)

summary(df1$Description)
str((df1$Description))

is.na(df1)

sum(is.na(df1))

df1 <- df1 %>%
  slice(1:3604)

tail(df1)

df1_tidy <- df1 %>%
  pivot_longer(cols = X2001:X2019, 
               names_to = "Year", 
               values_to = "Values",
               names_prefix = "X")

head(df1_tidy)
glimpse(df1_tidy)

df1_tidier <- df1_tidy %>%
  pivot_wider(id_cols = c("GeoFIPS", "Year"),
              names_from = Description,
              values_from = Values)

head(df1_tidier)
glimpse(df1_tidier)
View(df1_tidier)

colSums(is.na(df1_tidier))

str(df1_tidier)
dim(df1_tidier)
glimpse(df1_tidier)
summary(df1_tidier)

#df1_clean <- df1_tidier %>%
# transform(Government and government enterprises=as.numeric(Government and government enterprises))

df1_clean <- df1_tidier %>%
  mutate_at(vars(Year:"Private services-providing industries 3/"),
            ~as.numeric(.))

glimpse(df1_clean)

df1_clean <- df1_clean %>%
  rename(Mining = `  Agriculture, forestry, fishing and hunting`,
         Ag = `  Mining, quarrying, and oil and gas extraction`,
         PI = ` Private industries`,
         total = `All industry total`) %>%
  filter(GeoFIPS != "\"2000\"")

dim(df1_clean)
glimpse(df1_clean)

length(unique(df1_clean$GeoFIPS))
length(unique(df1_clean$Year))

View(df1_clean)

df1_clean <- df1_clean %>%
  filter(GeoFIPS != "\"20000\"" )

head(df1_clean)

dim(df1_clean)

df1_clean %>%
  group_by(Year) %>%
  summarise_all(~sum(is.na(.)))

df1_clean %>%
  group_by(GeoFIPS) %>%
  summarise_all(~sum(is.na(.)))

df1_clean %>%
  summarise_all(~mean(., na.rm=T))

df1_clean %>%
  summarise_all(~sd(., na.rm=T))


#Tuesday, 02/15/2022-------------------------------------

ggplot(df1_clean) +
  geom_histogram(aes(x=total))

ggplot(df1_clean) +
  geom_histogram(aes(x=Ag))

df1_clean %>%
  filter(Year == 2012) %>%
ggplot(.) +
  geom_histogram(aes(x=Ag))

ggplot(df1_clean) +
  geom_boxplot(aes(x=GeoFIPS, y= total), fill="lightblue", alpha=0.7)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, size = 5))

ggplot(df1_clean)+
  geom_point(aes(x=Year, y=Ag))+
  geom_smooth(aes(x=Year, y=Ag))

