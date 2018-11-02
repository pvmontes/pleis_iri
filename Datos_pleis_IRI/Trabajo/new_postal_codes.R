#Script to paste the postal code of pleis_v12 in pleis, (for elements don't match in pleis_iri.R)
#Executing after data_analysis_v12.R


new_pleis_v12 <- pleis_v12

# for(i in 1:nrow(merge_iri_v12_pleis_v12)) {
#   if(new_pleis_v12$cod == merge_iri_v12_pleis_v12$cod.x) {
#     new_pleis_v12[i,"cp"] <- merge_iri_v12_pleis_v12[i,"Código Postal.y"]
#   }
# }
# 
# View(new_pleis_v12)
# 
# 
# 
# for(i in 1:nrow(pleis)) {
#   pleis[i,"cp"] <- 
# }
# 
# for(i in 1:nrow(new_pleis_v12)) {
#   if(new_pleis_v12$cod == merge_iri_v12_pleis_v12$cod.x) {
#     new_pleis_v12[i,"cp"] <- merge_iri_v12_pleis_v12[i,"Código Postal.y"]
#   }
# }


new_postal_code <- merge(new_pleis_v12, merge_iri_v12_pleis_v12, by.x = "cod", by.y = "cod.x" )

reduced_new_postal_code <- new_postal_code[,c("cod","Código Postal.y")]

reduced_new_postal_code$cod <- as.character(reduced_new_postal_code$cod)

reduced_new_postal_code$right_postal_code <- as.character(reduced_new_postal_code$right_postal_code)

colnames(reduced_new_postal_code)[2] <- "right_postal_code"


# View(reduced_new_postal_code)