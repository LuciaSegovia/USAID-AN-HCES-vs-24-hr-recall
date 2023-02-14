library("tidyverse")
library("reshape")
raw_table <- read.table(url("https://www.ars.usda.gov/ARSUserFiles/80400535/Data/retn/retn06.txt"), sep = "^") #Imports the table from the internet, seperating columns by the "^" sign.

int_table_1 <- raw_table %>%
  mutate_all(funs(gsub("~", "", .))) #removes the '~' in the table

int_table_1$V5 <- paste0(int_table_1$V5, " (", int_table_1$V4, ")") #creates a combined nutrient/ID column
int_table_1$V4 <- NULL #removes the nutrient ID column

int_table_2 <- reshape(int_table_1, idvar = c("V1", "V2", "V3", "V7"), timevar = c("V5"), direction = "wide") #reshapes the table to a wide format instead of a long one

colnames_1 <- colnames(int_table_2) #copies the column names
colnames_2 <- gsub("V6.", "", colnames_1) #tidies the column names, removing the V6. from them if they're there


int_table_3 <- int_table_2 %>%  #This block separates out the cooking description either side of the first comma, which ends up being food in one column then cooking method in another
  mutate(V8 = unlist(lapply(strsplit(V3,", "), function(x) x[1])),
         V3 = gsub(",.*","", V3),
         V8 = gsub(".*?,", "", V8))

retention_factors_release_6_tidied <- int_table_3 %>% select(c(colnames_1[1:3], "V8", colnames_1[4:30])) #reorders the columns so metadata is first

colnames(retention_factors_release_6_tidied) <- c("Food ID", "Group ID", "Food Description", "Cooking Method Description", "Date", colnames_2[5:30]) #renames the columns using the tidied column names from colnames_2