#Tuesday, 02/15/2022 ---------------------------------------

install.packages("USAboundaries")
library(USAboundaries)
#library(USAboundariesData)
#data()

install.packages("USAboundariesData", repos = "https://ropensci.r-universe.dev", type = "source")
library(USAboundariesData)

counties <- us_counties(states = "KS")
class(counties)
#sf is simple feature geometry

head(counties)
