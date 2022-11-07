library(rlang)
library(dplyr)


library(tidyverse)
library(magrittr)
library(readxl)

#Clean the dataset
strawb <- read_xlsx("strawberries-2022oct30-a.xlsx", col_names = T)

cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]

T <- NULL
for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}
drop_cols <- cnames[which(T == 1)]
strawb %<>% select(!all_of(drop_cols))
strawb %<>% arrange(Year, State)

temp1 <- strawb %>% select(`Data Item`) %>% 
  distinct()

strawb %<>% separate(col=`Data Item`,
                     into = c("Strawberries", "type", "items", "units"),
                     sep = ",",
                     fill = "right")


# Q1
# LB = 100 x CWT
ans_1 <- 100 * 285


# Q2
# Margin of error (parameter) = Critical value x Standard deviation for the population
mean_ca<- as.numeric(strawb$Value[1])
cv<- as.numeric(strawb$`CV (%)`[1])/100
sd <- as.numeric(mean_ca) * as.numeric(cv)
Upper <- mean_ca + 1.96 * sd
Lower <- mean_ca - 1.96 * sd

# Q3 
# Answer is "NA" Because the column contains "(NA)", "(D)", and "(Z)"

# Q4
df_4.1 <- strawb %>% filter(`Domain`!= "ORGANIC STATUS" & `Domain`!= "TOTAL")

unique_chem <-unique(df_4.1$`Domain Category`)

df_total <- grep("TOTAL", 
            df_4.1$`Domain Category`, 
            ignore.case = T)

strawb_uniqchem <- length(unique_chem) - length(df_total)

#Q5
df_ca<- strawb %>% filter(`Domain`!= "ORGANIC STATUS" & `Domain`!= "TOTAL" & 
                            `State`== "CALIFORNIA")

ca_chem <-unique(df_ca$`Domain Category`)

df_fl<- strawb %>% filter(`Domain`!= "ORGANIC STATUS" & `Domain`!= "TOTAL" & 
                            `State`== "FLORIDA")
fl_chem <-unique(df_fl$`Domain Category`)

chem_diff<- length(ca_chem) - length(fl_chem)
