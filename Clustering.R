library(readr)
library(dplyr)
setwd("H:/R exploration")
data = read_csv("PXS2021data.csv")



#rename columns to make more sense
data = rename(data,  "frequency" = `TC1B`, "tenure" = `TC5`, "mode" = `TC1EA2`, "vehicle access" = `TC3A`, "purpose" = `TC2`, "early AM" = `TCA2B_1`, "AM peak" = `TCA2B_2`, 
              "late morning" = `TCA2B_3`, "early afternoon" = `TCA2B_4`, "PM peak" = `TCA2B_5`, "early evening" = `TCA2B_6`, "late evening" = `TCA2B_7`, "very late" = `TCA2B_8`,
              "weekend" = `TCA2B_9`, "Link" = `TC1EA1_1`, "STX" = `TC1EA1_2`, "N Line" = `TC1EA1_3`, "S Line" = `TC1EA1_4`, "T Line" = `TC1EA1_5`)  
   

# cluster based on travel behaviors. then attach to other data to see grades and demographics. 

data1 <- data %>% select(frequency, tenure, `vehicle access`, purpose, `early AM`, `AM peak`, `late morning`, 
                         `early afternoon`, `PM peak`, `early evening`, `late evening`, `very late`, `weekend`, Link, STX, `N Line`, `S Line`, `T Line`)


#standardize data
data1 <- as.data.frame(scale(data1))

summary(data1) #check that mean of each column is zero

#create clusters
data_dist = dist(data1, method='euclidian') 

clusters = hclust(data_dist) 


plot(clusters) #create dendogram to decide how many clusters to use

data1$cluster = cutree(clusters, 6) #assign clusters

tab = table(data1$cluster) #check distribution of clusters
tab[5]/4890

#separate out clusters
clust1 = data1 %>% filter(cluster == 1) 
clust2 = data1 %>% filter(cluster == 2)
clust3 = data1 %>% filter(cluster == 3)
clust4 = data1 %>% filter(cluster == 4)
clust5 = data1 %>% filter(cluster == 5)
clust6 = data1 %>% filter(cluster == 6)

table(data$`vehicle access`)

#evaluate clusters compared to overall data to see how they are distinguished
summary(data1)
summary(clust1) #only 8 people
summary(clust2) #ride more frequently, some new riders, slightly less vehicle access, commuters, peak, Link & STX
summary(clust3) #less frequent, average tenure, average veh access, recreation and some work, peak and weekends, all modes
summary(clust4) # less frequent, longer tenure, avg veh access, rec/social and airport, weekends and afternoons, Link
summary(clust5) #more frequent, avg tenure, less veh access, rec/social and work, evenings and weekends, Link and STX
summary(clust6) #only 10 people


data2 = merge(data1$cluster, data, by="row.names", all.x=TRUE) #joins cluster vector to original, unscaled dataframe
data2 = (data2[order(data2$sys_RespNum), ])  #sorts it to match original df
colnames(data2)[2] <- "cluster" #rename column name


pax_data = data2 %>% select(gender, ageCat, zipCode, race_1, race_2, race_3, race_4, race_5, race_6, race_7, disability, income, cms1, overallGradeAccess, 
                            overallGradeDependable, overallGradeSafety, overallGradeClean, overallGradeInformation, overallGradePassengerCare, overallGradeAvailable, cluster)

table(data$tenure)


#race_1: AIAN
#race_2: Asian
#race_3: Black
#race_4: Hawaiian/PI
#race_5: white
#race_6: Other
#disability: 1 yes, 2 no
#income: 
#Less than $10,000 
#10,000 to $14,999 
#$15,000 to $19,999 
#$20,000 to $24,999 
#$25,000 to $34,999 
#35,000 to $49,999 
#50,000 to $74,999 
#75,000 to $99,999 
#100,000 to $149,999 
#150,000 to $199,999 
#200,000 or more 
#gender: 1 male, 2 female (?)

#frequency: 1) 4+ days, 2) 2-3 days, 3) 1/week, 4) 1/month, 5) occasional, 6) first time, 
#vehicle access: 1) none, 2) some, 3) always, 4) other
#purpose: 1- work, 2-school, 3-airport, 4-recreation/social, 5-errands, 6-appointments, 7- social services, 8 - other, 9 - na
#tenure: 6 - 3-4 yrs, 7 - 4-5 yrs, 8 - 5-10 yrs, 9 - 10+ years




#now separate again into clusters to facilitate the summaries. Ignore clusters 1 and 6, which are too small. 

clust2_full = pax_data %>% filter(cluster == 2) #demographics are about average. lower grades for CSM, dependable, clean, information
clust3_full = pax_data %>% filter(cluster == 3) #more men, slightly older, more with disabilities. lower grades for CSM, clean, information
clust4_full = pax_data %>% filter(cluster == 4) #slightly older, slightly higher income. higher CMS, other grades are about average or slightly above
clust5_full = pax_data %>% filter(cluster == 5) #more nonbinary, younger, lower income. lower grades for CSM, access, dependable, safety, clean, information, pax care


#random code for analysis


table(pax_data$cluster)
summary(clust5_full)

hist(clust5_full$gender)
hist(pax_data$gender)

hist(clust4_full$ageCat)
hist(pax_data$ageCat)

hist(clust4_full$disability)
hist(pax_data$disability)

hist(clust4_full$income)
hist(pax_data$income)

sort(table(clust2_full$zipCode))
sort(table(clust3_full$zipCode))
sort(table(clust4_full$zipCode))
sort(table(clust5_full$zipCode))



sort(table(pax_data$zipCode))

summary(pax_data)


