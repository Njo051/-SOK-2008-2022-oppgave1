#Oppgave 1.3.5
library(tidyverse)  
library(readxl) 
library(ineq)

Sys.setlocale(locale="no_NO")

setwd("/Users/nils/Downloads")

decile_data <- read_excel("GCIPrawdatatest.xlsx", skip = 2)  
#The data is now in a 'tibble' (like a spreadsheet for R). Let's use the head function to look at the first few rows:
head(decile_data)
#Now we use loops to complete our task. We begin by creating a new variable in our dataset, gini, which we initially set to 0 for all country-year combinations.
decile_data$gini <- 0

noc <- nrow(decile_data)

for (i in seq(1, noc)){
  # Go to Row I to get the decile data
  decs_i <- unlist(decile_data[i, 3:12])
  decile_data$gini[i] <- Gini(decs_i)
}
#With this code, we calculated 4,799 Gini coefficients without having to manually run the same command 4,799 times. We now look at some summary measures for the gini variable.
#First we use the subset function to select Nordic countries and save their data as temp_data. As an example, we have chosen four anglophone countries: the UK, the US, Ireland, and Australia.
temp_data <- subset(
  decile_data, Country %in% c("United States","Sweden","Finland","Norway", 
                              "Denmark"))
#Now we plot the data using ggplot.

ggplot(temp_data, 
       aes(x = Year, y = gini, color = Country)) +
  geom_line(size = 1) +
  theme_bw() +
  ylab("Gini") +
  ggtitle("Gini coefficients for Nordic countries")




