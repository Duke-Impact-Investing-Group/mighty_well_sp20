#library(RMySQL)
library(dplyr)
library(plyr)
#library(ggplot2)
library(tidyverse)
#setting my directory
setwd('C:\\Users\\Jayesh\\Downloads\\DIIG\\S20\\MW')

product_reviews_final <- read.csv("product.csv", stringsAsFactors = FALSE)
product_reviews_final2 <- read.csv("product.csv", stringsAsFactors = FALSE) #this is for double checking whether the data manipulating we do is correct by comparing it to the original 

#changing product handle names to fit 6 general categories and removing unwanted values 
product_reviews_final$product_handle[grep("shirt", product_reviews_final$product_handle)] <- "merch"
product_reviews_final$product_handle[grep("hat", product_reviews_final$product_handle)] <- "merch"
product_reviews_final$product_handle[grep("sticker", product_reviews_final$product_handle)] <- "merch"
product_reviews_final$product_handle[grep("line", product_reviews_final$product_handle)] <- "picc line cover"
product_reviews_final$product_handle[grep("medplanner", product_reviews_final$product_handle)] <- "medplanner"
product_reviews_final$product_handle[grep("up", product_reviews_final$product_handle)] <- "na"
product_reviews_final$product_handle[grep("perfect", product_reviews_final$product_handle)] <- "na"
product_reviews_final$product_handle[grep("2pack", product_reviews_final$product_handle)] <- "na"
product_reviews_final <- product_reviews_final[!(product_reviews_final$product_handle == "na"), ]
product_reviews_final <- product_reviews_final[!(product_reviews_final$product_handle == ""), ]
##<---------------------------------------------------------------------------------------------------------------------------->
## GENERATING ANALYSIS FOR DEVICE  
##<---------------------------------------------------------------------------------------------------------------------------->

product_reviews_devices <- as.data.frame(xtabs(~ device + product_handle, product_reviews_final)) #generating a count table of product_handle for each device 
product_reviews_devices <- product_reviews_devices[!(product_reviews_devices$device == "na"), ] #removing all na 
product_reviews_products_devices_count <- count(product_reviews_final, c("product_handle", "device")) #same table as 2 lines above but without 0s to avoid divide by 0 erros 
product_reviews_products_count <-count(product_reviews_devices, vars = "product_handle") #counting total number of product handle for each one for device analysis 
product_reviews_devices_count<- count(product_reviews_devices, vars = "device")#counting total number of device for each one for device analysis 
product_reviews_products_devices_count$device<-sub(", ", ",", product_reviews_products_devices_count$device) #removing space between comma and secondary device (run 3 times to fully solve this problem)
product_reviews_products_devices_count$device<-sub(", ", ",", product_reviews_products_devices_count$device)
product_reviews_products_devices_count$device<-sub(", ", ",", product_reviews_products_devices_count$device)
product_reviews_devices_count$device<-sub(", ", ",", product_reviews_devices_count$device)
product_reviews_devices_count$device<-sub(", ", ",", product_reviews_devices_count$device)
product_reviews_devices_count$device<-sub(", ", ",", product_reviews_devices_count$device)
#adding values from 2 or more device combinations per product handle to single device freq 
for (i in 1:nrow(product_reviews_products_devices_count)){
  if (grepl(",", product_reviews_products_devices_count$device[i])){
    z <- strsplit(product_reviews_products_devices_count$device[i], ",")
    for (string in z){
      for (q in string){
        print(q)
        indexIWanttoadd <- which(product_reviews_products_devices_count$device == q & product_reviews_products_devices_count$product_handle == product_reviews_products_devices_count$product_handle[i])
        product_reviews_products_devices_count$freq[indexIWanttoadd] <- product_reviews_products_devices_count$freq[i] + product_reviews_products_devices_count$freq[indexIWanttoadd]
      }

    }
  }
}
#repeating process above for device count
product_reviews_devices_count_2 <-  product_reviews_devices_count #this is to compare to previous values to make sure this is working 
for (i in 1:nrow(product_reviews_devices_count)){
  if (grepl(",", product_reviews_devices_count$device[i])){
    z <- strsplit(product_reviews_devices_count$device[i], ",")
    for (string in z){
      for (q in string){
        print(q)
        indexIWanttoadd <- which(product_reviews_devices_count$device == q)
        product_reviews_devices_count$freq[indexIWanttoadd] <- product_reviews_devices_count$freq[i] + product_reviews_devices_count$freq[indexIWanttoadd]
      }
      
    }
  }
}


rows = nrow(product_reviews_devices)
product_reviews_products_count[,"probability"] <- NA
product_reviews_devices_count[,"probability"] <- NA
product_reviews_products_devices_count[,"probability"] <- NA
for (i in 1:nrow(product_reviews_products_devices_count)){
  product_reviews_products_devices_count$probability[i] <- product_reviews_products_devices_count$freq[i] / rows #finding probability of selecting a device(s) and product handle
}
for (i in 1:nrow(product_reviews_products_count)){
  product_reviews_products_count$probability[i] <- product_reviews_products_count$freq[i] / rows #finding probability of selecting a product handle
}
for (i in 1:nrow(product_reviews_devices_count)){
  product_reviews_devices_count$probability[i] <- product_reviews_devices_count$freq[i] / rows #finding probability of selecting a device(s)
}
#removing empty rows 
product_reviews_products_devices_count <- product_reviews_products_devices_count[!(product_reviews_products_devices_count$device == ""), ]
product_reviews_products_count <- product_reviews_products_count[!(product_reviews_products_count$product_handle == ""), ]
product_reviews_devices_count <- product_reviews_devices_count[!(product_reviews_devices_count$device == ""), ]
#reindexing rows 
row.names(product_reviews_products_devices_count) <- 1:nrow(product_reviews_products_devices_count)
row.names(product_reviews_products_count) <- 1:nrow(product_reviews_products_count)
row.names(product_reviews_devices_count) <- 1:nrow(product_reviews_devices_count)
product_reviews_products_devices_count_2 <- product_reviews_products_devices_count


x <- '';
product_reviews_products_devices_count[,"Baye's"] <- NA
for (i in 1:nrow(product_reviews_products_devices_count)){
  s = product_reviews_products_devices_count$device[i] #selecting device 
  y <- sapply(s,function(s) which(apply(product_reviews_devices_count==s,1,any))[1]); #finding index at which device can be found in device count table 
  product_reviews_products_devices_count$`Baye's`[i] <- product_reviews_products_devices_count$probability[i] / product_reviews_devices_count$probability[y]
}
#write.csv(product_reviews_products_devices_count, "revised_device_analysis")

##<---------------------------------------------------------------------------------------------------------------------------->
## FILLING IN BLANKS MANUALLY 
##<---------------------------------------------------------------------------------------------------------------------------->

product_reviews_final[186,17] <- "looking good/feeling proud"
product_reviews_final[5,17] <- "air travel"
product_reviews_final[223,17] <- "tube feeding"
product_reviews_final[97,17] <- "IV bandage"
product_reviews_final[97,15] <- "sensitive skin"
product_reviews_final[7,17] <- "hold medical supplies"
product_reviews_final[233,17] <- "stay warm"
product_reviews_final[137,17] <- "hold PIIC line"
product_reviews_final[35,17] <- "hold medical supplies"
product_reviews_final[132,15] <- "chemotherapy"
product_reviews_final[83,15] <- "lyme disease"
product_reviews_final[258,17] <- "wheelchair"
product_reviews_final[241,15] <- "cancer"
product_reviews_final[153,15] <- "chemotherapy"
product_reviews_final[102,17] <- "tube feeding"
product_reviews_final[206,17] <- "hold PIIC line"
product_reviews_final[244,15] <- "blood tumor"
product_reviews_final[28,15] <- "lyme disease"
product_reviews_final[28,17] <- "hold medical supplies"
product_reviews_final[230,15] <- "lyme disease"
product_reviews_final[230,17] <- "looking good/feeling proud"
product_reviews_final[248,17] <- "looking good/feeling proud"
product_reviews_final[256,17] <- "looking good/feeling proud"
product_reviews_final[191,15] <- "sensitive skin"
product_reviews_final[53,15] <- "chemotherapy"
product_reviews_final[51,15] <- "cancer"
product_reviews_final[24,17] <- "hold medical supplies"
product_reviews_final[221,15] <- "lyme disease"
product_reviews_final[192,15] <- "lyme disease"
product_reviews_final[257,15] <- "fibromyalgia"
product_reviews_final= product_reviews_final[-c(56,264,265,266),]

##<---------------------------------------------------------------------------------------------------------------------------->
## GENERATING ANALYSIS FOR GENDER  
##<---------------------------------------------------------------------------------------------------------------------------->


product_reviews_final_gender <- as.data.frame(xtabs(~ gender + product_handle, product_reviews_final))
product_reviews_final_gender <- product_reviews_final_gender[!(product_reviews_final_gender$gender == "anonymous"), ] 
product_reviews_final_gender[,"probability"] <- NA
product_reviews_products_count_for_gender <-count(product_reviews_final, c("product_handle","gender"))
product_reviews_products_count_for_gender <- product_reviews_products_count_for_gender[!(product_reviews_products_count_for_gender$gender == "anonymous"), ] #removing anomymous 
product_reviews_products_count_for_gender <-count(product_reviews_products_count_for_gender, vars = "product_handle") #doing count of indiviual product handle for gender 
for (i in 1: nrow(product_reviews_final_gender)){
  s = product_reviews_final_gender$product_handle[i]
  indexIWanttoadd <- which(product_reviews_products_count_for_gender$product_handle == s)
  if (length(indexIWanttoadd) == 0){ #if index is not present 
    next 
  }
  product_reviews_final_gender$probability[i] <- product_reviews_final_gender$Freq[i]/product_reviews_products_count_for_gender$freq[indexIWanttoadd]
}

##<---------------------------------------------------------------------------------------------------------------------------->
## GENERATING ANALYSIS FOR MEDICAL CONDITION   
##<---------------------------------------------------------------------------------------------------------------------------->


product_reviews_final_condition <-  as.data.frame(xtabs(~ condition + product_handle, product_reviews_final))
product_reviews_final_condition <- product_reviews_final_condition[!(product_reviews_final_condition$condition == ""), ]
product_reviews_final_condition <- product_reviews_final_condition[!(product_reviews_final_condition$condition == "na"), ]
product_reviews_final_condition <- product_reviews_final_condition[!(product_reviews_final_condition$condition == "cancer"), ]
product_reviews_final_condition[,"probability"] <- NA
product_reviews_products_count_for_final_condition <-count(product_reviews_final, c("product_handle","condition"))
product_reviews_products_count_for_final_condition <- product_reviews_products_count_for_final_condition[!(product_reviews_products_count_for_final_condition$condition == ""), ]
product_reviews_products_count_for_final_condition <- product_reviews_products_count_for_final_condition[!(product_reviews_products_count_for_final_condition$condition == "na"), ]
product_reviews_products_count_for_final_condition <- product_reviews_products_count_for_final_condition[!(product_reviews_products_count_for_final_condition$condition == "cancer"), ]
product_reviews_products_count_for_final_condition <-count(product_reviews_products_count_for_final_condition, vars = "product_handle")
for (i in 1: nrow(product_reviews_final_condition)){
  s = product_reviews_final_condition$product_handle[i]
  indexIWanttoadd <- which(product_reviews_products_count_for_final_condition$product_handle == s)
  if (length(indexIWanttoadd) == 0){
    next
  }
  product_reviews_final_condition$probability[i] <- product_reviews_final_condition$Freq[i]/product_reviews_products_count_for_final_condition$freq[indexIWanttoadd]
}

##<---------------------------------------------------------------------------------------------------------------------------->
## GENERATING ANALYSIS FOR MEDICAL CASE   
##<---------------------------------------------------------------------------------------------------------------------------->

product_reviews_final_case <-  as.data.frame(xtabs(~ use.case + product_handle, product_reviews_final))
product_reviews_final_case <- product_reviews_final_case[!(product_reviews_final_case$use.case == ""), ]
product_reviews_final_case <- product_reviews_final_case[!(product_reviews_final_case$use.case == "na"), ]
product_reviews_final_case[,"probability"] <- NA
product_reviews_products_count_for_final_case <-count(product_reviews_final, c("product_handle","use.case"))
product_reviews_products_count_for_final_case <- product_reviews_products_count_for_final_case[!(product_reviews_products_count_for_final_case$use.case == ""), ]
product_reviews_products_count_for_final_case <- product_reviews_products_count_for_final_case[!(product_reviews_products_count_for_final_case$use.case == "na"), ]
product_reviews_products_count_for_final_case <-count(product_reviews_products_count_for_final_case, vars = "product_handle")
for (i in 1: nrow(product_reviews_final_case)){
  s = product_reviews_final_case$product_handle[i]
  indexIWanttoadd <- which(product_reviews_products_count_for_final_case$product_handle == s)
  if (length(indexIWanttoadd) == 0){
    next
  }
  product_reviews_final_case$probability[i] <- product_reviews_final_case$Freq[i]/product_reviews_products_count_for_final_case$freq[indexIWanttoadd]
}
product_reviews_final_case <- product_reviews_final_case[!is.na(product_reviews_final_case$probability), ]

##<---------------------------------------------------------------------------------------------------------------------------->
## GENERATING CSV FILES FOR PYTHON 
##<---------------------------------------------------------------------------------------------------------------------------->


write.csv(product_reviews_final_gender, "final_final_product_reviews_final_gender.csv")
write.csv(product_reviews_final_condition, "final_final_product_reviews_final_condition.csv")
write.csv(product_reviews_final_case, "final_final_product_reviews_final_case.csv")
write.csv(product_reviews_products_devices_count, "final_final_product_reviews_products_devices_count.csv")
#write.csv(product_reviews_final_raw_use_case, "product_reviews_final_raw_use_case.csv")