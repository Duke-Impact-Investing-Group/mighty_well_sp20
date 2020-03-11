#library(RMySQL)
library(dplyr)
library(plyr)
#library(ggplot2)
library(tidyverse)
setwd('Z:\\DIIG\\S20\\MW')

customers_export <- read.csv("customers_export_MW.csv", header = T)
orders_export <- read.csv("orders_export_1 MW.csv", header = T)
deleted_reviews<- read.csv("mighty-well-all-deleted-reviews-in-judgeme-format-2020-03-03-1583279019.csv")
not_all_published <- read.csv("mighty-well-all-not-published-reviews-in-judgeme-format-2020-03-03-1583279014.csv", header = T)

orders_export <- orders_export[order(orders_export$Lineitem.name),]

orders_export2 <- orders_export
orders_export2 <- orders_export2[order(orders_export2$Lineitem.name),]
orders_export2$Lineitem.name <- str_remove_all(orders_export2$Lineitem.name, "[-/,]")
orders_export2$Lineitem.name <- str_remove_all(orders_export2$Lineitem.name, "Blue")
orders_export2$Lineitem.name <- str_remove_all(orders_export2$Lineitem.name, "Black")
orders_export2$Lineitem.name <- str_remove_all(orders_export2$Lineitem.name, "Navy")
orders_export2$Lineitem.name <- str_remove_all(orders_export2$Lineitem.name, "Dark")
orders_export2$Lineitem.name <- str_remove_all(orders_export2$Lineitem.name, "Heather")
orders_export2$Lineitem.name <- str_remove_all(orders_export2$Lineitem.name, "Grey")
orders_export2$Lineitem.name <- str_remove_all(orders_export2$Lineitem.name, "Light")


#Doctors Say I'm The Illest TShirt 
selectedDSITITShirt <- orders_export2[grep("Doctors Say I'm The Illest", orders_export2$Lineitem.name), ] 
selectedDSITITShirt$Lineitem.name <- substr(selectedDSITITShirt$Lineitem.name, start = 0, stop = 35)

#Friend in the Fight Tshirts and Send a letter to Friend in the Fight
selectedFITFTShirt <- orders_export2[grep("Friend in the Fight", orders_export2$Lineitem.name), ] 
selectedSFITF <-selectedFITFTShirt[(selectedFITFTShirt$Lineitem.sku == 50), ] # Send a letter to Friend in the Fight
selectedFITFTShirt <-selectedFITFTShirt[!(selectedFITFTShirt$Lineitem.sku == 50), ] #Friend in the Fight Tshirts
selectedFITFTShirt$Lineitem.name <- substr(selectedFITFTShirt$Lineitem.name, start = 0, stop = 38)

#	Mighty Sleeves Refill Pack
selectedMSRP<- orders_export2[grep("Mighty Sleeves Refill Pack", orders_export2$Lineitem.name), ] 

#	Mighty Tee
selectedMT<- orders_export2[grep("Mighty Tee", orders_export2$Lineitem.name), ] 

#	Mug
selectedMug<- orders_export2[grep("Mug", orders_export2$Lineitem.name), ] 

#	Undefeated Baseball Hat
selectedUBH <- orders_export2[grep("Baseball Hat", orders_export2$Lineitem.name), ] 

#	Undefeated Tshirts 
selectedUTShirt<- orders_export2[grep("Undefeated", orders_export2$Lineitem.name), ] 
selectedUTShirt<- selectedUTShirt[grep("TShirt", selectedUTShirt$Lineitem.name), ]
selectedUTShirt$Lineitem.name <- substr(selectedUTShirt$Lineitem.name, start = 0, stop = 20)

#Doctors Say I'm The Illest Shirt 
selectedDSITIShirt <- orders_export2[grep("Doctor's Say I'm The Illest Shirt", orders_export2$Lineitem.name), ] 
selectedDSITIShirt$Lineitem.name <- substr(selectedDSITIShirt$Lineitem.name, start = 0, stop = 35)

#Donate a PICCPerfect PICC Line Cover
selectedDPPLC <- orders_export2[grep("Donate a PICCPerfect PICC Line Cover", orders_export2$Lineitem.name), ] 

#I Am Mighty Well 
selectedIAMW <- orders_export2[grep("I Am Mighty Well ", orders_export2$Lineitem.name), ] 
selectedIAMW$Lineitem.name <- substr(selectedIAMW$Lineitem.name, start = 0, stop = 16)

#MedPlanner Pouches Refill
selectedMPP <- orders_export2[grep("MedPlanner Pouches Refill", orders_export2$Lineitem.name), ] 
selectedMPP$Lineitem.name <- substr(selectedMPP$Lineitem.name, start = 0, stop = 25)

#New Day TShirt 
selectedNDTShirt <- orders_export2[grep("New Day TShirt", orders_export2$Lineitem.name), ] 
selectedNDTShirt$Lineitem.name <- substr(selectedNDTShirt$Lineitem.name, start = 0, stop = 16)

# PICCPerfect 1.0  
selectedPP1 <- orders_export2[grep("PICCPerfect 1.0", orders_export2$Lineitem.name), ] 
selectedPP1$Lineitem.name <- substr(selectedPP1$Lineitem.name, start = 0, stop = 15)

# PICCed Up Kids  
selectedPUK <- orders_export2[grep("PICCed Up Kids", orders_export2$Lineitem.name), ] 
selectedPUK$Lineitem.name <- substr(selectedPUK$Lineitem.name, start = 0, stop = 15)

# NEW PICCPerfect 2.0  
selectedNP2 <- orders_export2[grep("NEW PICCPerfect 2.0", orders_export2$Lineitem.name), ] 
  #Pack
selectedNP2P <- selectedNP2[grep("Pack", selectedNP2$Lineitem.name), ] 
selectedNP2P$Lineitem.name <- "NEW PICCPerfect 2.0 Pack"
  #Line Cover 
selectedNP2LC <- selectedNP2[grep("Line", selectedNP2$Lineitem.name), ] 
selectedNP2LC$Lineitem.name <- "NEW PICCPerfect 2.0 Line Cover"

#PICC
  #Cover
selectedPC <- orders_export2[grep("PICC Cover", orders_export2$Lineitem.name), ] 
selectedPC$Lineitem.name <- substr(selectedPC$Lineitem.name, start = 0, stop =14)
  #Me up 
selectedPMU <- orders_export2[grep("PICC Me Up", orders_export2$Lineitem.name), ] 
selectedPMU<-selectedPMU[!grepl("PICCPerfect", selectedPMU$Lineitem.name),]
selectedPMU<-selectedPMU[!grepl("PICC Perfect", selectedPMU$Lineitem.name),]
selectedPMU$Lineitem.name <- substr(selectedPMU$Lineitem.name, start = 0, stop =10)

#PICCPerfect PICC  
selectedPP <- orders_export2[grep("PICCPerfect PICC", orders_export2$Lineitem.name), ]
  #Me up 
selectedPPMU <- selectedPP[grep("Me Up", selectedPP$Lineitem.name), ]
selectedPPMU$Lineitem.name <- substr(selectedPPMU$Lineitem.name, start = 0, stop =23)
  #Donate Line cover 
selectedDPPLC <- selectedPP[grep("Line", selectedPP$Lineitem.name), ]
selectedDPPLC <- selectedDPPLC[grep("Donate", selectedDPPLC$Lineitem.name), ] 
  #Line Cover 
selectedPPLC <- selectedPP[grep("Line", selectedPP$Lineitem.name), ] 
selectedPPLC <- selectedPPLC[-1,]
selectedPPLC$Lineitem.name <- substr(selectedPPLC$Lineitem.name, start = 0, stop =16)

#PICCPerfect: Smart PICC Line Cover    
selectedPSPLC <- orders_export2[grep("PICCPerfect: Smart PICC Line Cover", orders_export2$Lineitem.name), ]
selectedPSPLC$Lineitem.name <- substr(selectedPSPLC$Lineitem.name, start = 0, stop =34)

#PICCPerfectÂ 
selectedPA <- orders_export2[grep("PICCPerfectÂ®", orders_export2$Lineitem.name), ]
  #PICC Line Covers
selectedPAPLC<- selectedPA[grep("Eco", selectedPA$Lineitem.name), ]
selectedPAPLC$Lineitem.name <- substr(selectedPAPLC$Lineitem.name, start = 0, stop =30)
  #Antimicrobial PICC Line Covers
selectedPAAPLC<- selectedPA[grep("Antimicrobial", selectedPA$Lineitem.name), ]
selectedPAAPLC$Lineitem.name <- substr(selectedPAAPLC$Lineitem.name, start = 0, stop =45)

#PICCPerfect Â
selectedP_A <- orders_export2[grep("PICCPerfect Â®", orders_export2$Lineitem.name), ]
selectedP_A$Lineitem.name <- substr(selectedP_A$Lineitem.name, start = 0, stop =46)

#Sponsor A Patient
selectedSAP <- orders_export2[grep("Sponsor A Patient", orders_export2$Lineitem.name), ]

#Spoonie Sickest Shirt
selectedSSShirt <- orders_export2[grep("Spoonie Sickest Shirt", orders_export2$Lineitem.name), ]
selectedSSShirt$Lineitem.name <- substr(selectedSSShirt$Lineitem.name, start = 0, stop =21)

#The Mighty MedCase
selectedTMMc <- orders_export2[grep("The Mighty MedCase", orders_export2$Lineitem.name), ]
selectedTMMc$Lineitem.name <- substr(selectedTMMc$Lineitem.name, start = 0, stop =18)

#The Mighty MedPlanner
selectedTMM <- orders_export2[grep("The Mighty MedPlanner", orders_export2$Lineitem.name), ]
selectedTMM$Lineitem.name <- substr(selectedTMM$Lineitem.name, start = 0, stop =21)

#The Mighty Pack 
selectedTMP <- orders_export2[grep("The Mighty Pack", orders_export2$Lineitem.name), ]
selectedTMP$Lineitem.name <- substr(selectedTMP$Lineitem.name, start = 0, stop =15)

#The Mighty Wrap 
selectedTMW <- orders_export2[grep("The Mighty Wrap", orders_export2$Lineitem.name), ]
selectedTMW$Lineitem.name <- substr(selectedTMW$Lineitem.name, start = 0, stop =15)

#The Mighty Patches Pack
selectedTMPP <- orders_export2[grep("The Mighty Patches Pack", orders_export2$Lineitem.name), ]

#The Undefeated Bundle 
selectedTUB <- orders_export2[grep("The Undefeated Bundle", orders_export2$Lineitem.name), ]
selectedTUB$Lineitem.name <- substr(selectedTUB$Lineitem.name, start = 0, stop =23)

#The Undefeated Holiday Bundle
selectedTUHB <- orders_export2[grep("The Undefeated Holiday Bundle", orders_export2$Lineitem.name), ]
selectedTUHB$Lineitem.name <- substr(selectedTUHB$Lineitem.name, start = 0, stop =30)


order_export_improved <- rbind(selectedDPPLC, selectedDSITIShirt, selectedDSITITShirt, selectedFITFTShirt, selectedIAMW, selectedMPP, selectedMSRP, selectedMT, selectedMug, selectedNDTShirt, selectedNP2LC, selectedNP2P, selectedP_A, selectedPAAPLC, selectedPAPLC, selectedPC, selectedPMU, selectedPP1, selectedPPLC, selectedPPMU, selectedPSPLC, selectedPUK, selectedSAP, selectedSFITF, selectedSSShirt, selectedTMMc, selectedTMPP, selectedTMW, selectedTUB, selectedTUHB, selectedUBH, selectedUTShirt)
order_export_improved <- order_export_improved[order(order_export_improved$Lineitem.name),]


#(unique(orders_export2$Lineitem.name))
published_reviews <- read.csv("mighty-well-all-published-reviews-in-judgeme-format-2020-02-26-1582741774.csv", header = T)
customers_export_spent <-customers_export[( customers_export$Total.Spent > 0),]


#$Lineitem.name <- str_remove_all(orders_export$Lineitem.name, " M ")
#orders_export$Lineitem.name <- sub("(\\w+\\s+\\w+).*", "\\1", orders_export$Lineitem.name)

customers_export_spent_zipcode <- aggregate(customers_export_spent$Total.Spent,by=list(customers_export_spent$Zip),sum)
customers_export_spent_city <- aggregate(customers_export_spent$Total.Spent, by=list(customers_export_spent$City), sum)
customers_export_orders_zipcode <- aggregate(customers_export_spent$Total.Orders, by = list(customers_export_spent$Zip), sum)
customers_export_orders_city <- aggregate(customers_export_spent$Total.Orders, by=list(customers_export_spent$City), sum)
customers_export_orders_city <- customers_export_orders_city[order(customers_export_orders_city$Group.1),]
customers_export_orders_zipcode <- customers_export_orders_zipcode[order(customers_export_orders_zipcode$Group.1),]
customers_export_spent_city <- customers_export_spent_city[order(customers_export_spent_city$Group.1),]
customers_export_spent_zipcode <- customers_export_spent_zipcode[order(customers_export_spent_zipcode$Group.1),]
customers_export_spentorder_city <- customers_export_orders_city
customers_export_spentorder_zipcode <- customers_export_orders_zipcode
customers_export_spentorder_zipcode$x <- customers_export_spent_zipcode$x / customers_export_orders_zipcode$x
customers_export_spentorder_city$x <- customers_export_spent_city$x / customers_export_orders_city$x
#write.csv(customers_export_spentorder_zipcode, "zipcodeanalysis.csv")
write.csv(customers_export_spentorder_city, "citycodeanalysis.csv")
colnames(customers_export)[colnames(customers_export) == "Email"] <- "reviewer_email"
merged_table <- merge( x = customers_export, y = published_reviews, by = "reviewer_email")
product_count_filtered_city_rating_fromreviews <- count(merged_table, vars=c("product_handle", "rating", "City"))
product_count_and_rating_fromreviews <- count(published_reviews, vars=c("product_handle","rating"))
product_count_fromreviews <- count(published_reviews, vars=c("product_handle"))
product_count_fromorders <- count(order_export_improved, vars=c("Lineitem.name"))
write.csv(product_count_fromorders, "productcountfromorders.csv")
#write.csv(product_count_fromreviews, "productcountfromreviews.csv")
