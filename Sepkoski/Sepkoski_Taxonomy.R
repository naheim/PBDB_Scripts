### SET WORKING DIRECTORY
my.wd <- "~/Documents/PBDB_Scripts/Sepkoski/"
setwd(my.wd)

### LOAD CUSTOM FUNCTIONS
source("../my.functions.R")

### Load Libraries
library(sepkoski)
library(jsonlite)

### EXTERNAL DATA FILES FOR SEPKOSKI & MARINE AND TERRESTRAIL TAXA
timescale <- read.csv(file="../Timescale/timescale_2026.csv") # ICS 2024 used by PBDB

### General Set Up
api.base <- "https://paleobiodb.org/data1.2/taxa/list.json?"
api.base.auto <- "https://paleobiodb.org/data1.2/taxa/auto.json?"

params <- paste(c(
     "pres=regular", # only body fossils
     "taxon_status=accepted",
     "rank=genus",
     "show=class,attr,app",
     "limit=250",
     "vocab=pbdb"
), collapse="&")


###### ADD & CLEAN UP SEPKOSKI DATASET
sepkoski <- sepkoski::sepkoski # using dataset from sepkoski package not fossilbrush

# Drop genera with genus name == "incertae sedis"
sepkoski <- subset(sepkoski, genus != "incertae sedis")

# THERE are some genera with ranges of 0 ma -- assuming because max and min ints were swapped
temp <- subset(sepkoski, max_ma - min_ma == 0)
temp <- unique(temp[,is.element(colnames(temp), c("interval_max", "interval_min"))])
for(i in 1:nrow(temp)) {
     sepkoski[sepkoski$interval_max==temp[i,1] & sepkoski$interval_min==temp[i,2],match(c("interval_max", "interval_min"), colnames(sepkoski))] <- temp[i,2:1]
}

# add ICS 2024 timescale
sep.ints <- unique(c(sepkoski$interval_max, sepkoski$interval_min))
sepkoski$max_ma_2022 <- sepkoski$max_ma
sepkoski$min_ma_2022 <- sepkoski$min_ma
for(i in sep.ints) {
     sepkoski$max_ma[sepkoski$interval_max==i] <- timescale$age_bottom[timescale$interval_name==i]
     sepkoski$min_ma[sepkoski$interval_min==i] <- timescale$age_top[timescale$interval_name==i]
}


sepkoski <- cbind('index' = 1:nrow(sepkoski), sepkoski)
sepkoski$cl_gn <- paste(sepkoski$class, sepkoski$genus, sep="_")

mult.gen <- table(sepkoski$cl_gn)
repeats <- names(mult.gen[mult.gen > 1])
for(i in repeats) {
     temp <- subset(sepkoski, cl_gn == i)
     temp <- temp[order(temp$max_ma, decreasing=TRUE),]
     
     if(nrow(unique(temp[,-1])) == 1) {
          sepkoski <- subset(sepkoski, index != temp$index[1]) # ranges identical, keep first, drop rest
     } else if(diff(temp$max_ma) <= 0 & diff(temp$min_ma) >= 0) { # if ranges are fully overlapping
          sepkoski <- subset(sepkoski, index != temp$index[1]) # keep longer range, drop shorter
     } else { # if ranges are non-overlapping or partially overlapping
          sepkoski <- subset(sepkoski, !is.element(index, temp$index)) # drop both
     }
}

## DOWNLOAD PBDB ENTRIES FOR SEPKOSKI GENERA
# to prevent timing out or request strings that are too long, grab 120 genera at a time
# using JSON rather than CSV to cut out warnings when Sepkoski genus is not in PBDB
n.sepkoski <- nrow(sepkoski)
iterate <- c(seq(1,n.sepkoski, 120), n.sepkoski+1)

sep.pbdb <- list()
for(i in 1:(length(iterate)-1)) {
     url.genus <- URLencode(paste0(api.base, 
          params, "&taxon_name=",
          paste(sepkoski$genus[iterate[i]:(iterate[i+1]-1)], collapse=","))
     )
     temp <- fromJSON(url.genus)$records
     if(i == 1) {
          col.order <- colnames(temp)
     } else {
          if(!is.element("flags", colnames(temp))) {
               temp$flags <- NA
          }
          if(!is.element("type_taxon", colnames(temp))) {
               temp$type_taxon <- NA
          }
          if(!is.element("difference", colnames(temp))) {
               temp$difference <- NA
          }
          temp <- temp[,match(col.order, colnames(temp))]
     }
     sep.pbdb[[i]] <- temp
     if(i %% 50 == 0) {print(iterate[i+1]-1)} # just to keep track of progress
}
sep.pbdb <- do.call(rbind, sep.pbdb) # combine individual data frames into one
sep.pbdb <- clean.pbdb.taxa(sep.pbdb)


# ADD PBDB HIGHER TAXONOMY TO SEPKOSKI
sepkoski$pbdb_phylum <- NA
sepkoski$pbdb_class <- NA
sepkoski$pbdb_order <- NA
sepkoski$pbdb_family <- NA
sepkoski$pbdb_genus <- NA
sepkoski$pbdb_accepted_no <- NA

sep.anim.phy <- c("Annelida","Arthropoda","Brachiopoda","Bryozoa",
     "Chordata","Cnidaria","Echinodermata","Hemichordata","Hyolitha",
     "Mollusca","Porifera")
for(i in 1:nrow(sepkoski)) {
     if(is.na(sepkoski$phylum[i])) {
          temp.pbdb <- subset(sep.pbdb, accepted_rank == "genus" & !is.element(phylum, sep.anim.phy) & (genus_name == sepkoski$genus[i] | accepted_name == sepkoski$genus[i] | taxon_name == sepkoski$genus[i]))
     } else {
          temp.pbdb <- subset(sep.pbdb, accepted_rank == "genus" & phylum == sepkoski$phylum[i] & (genus_name == sepkoski$genus[i] | accepted_name == sepkoski$genus[i] | taxon_name == sepkoski$genus[i]))
     }
     if(nrow(temp.pbdb) == 1) {
          sepkoski$pbdb_phylum[i] <- temp.pbdb$phylum[1]
          sepkoski$pbdb_class[i] <- temp.pbdb$class[1]
          sepkoski$pbdb_order[i] <- temp.pbdb$order[1]
          sepkoski$pbdb_family[i] <- temp.pbdb$family[1]
          sepkoski$pbdb_genus[i] <- temp.pbdb$genus_name[1]
          sepkoski$pbdb_accepted_no[i] <- temp.pbdb$accepted_no[1]
     } else if (nrow(temp.pbdb) > 1) {
          temp.pbdb <- subset(temp.pbdb, taxon_name == sepkoski$genus[i])
          if(nrow(temp.pbdb) > 1) {print(i)}
          sepkoski$pbdb_phylum[i] <- temp.pbdb$phylum[1]
          sepkoski$pbdb_class[i] <- temp.pbdb$class[1]
          sepkoski$pbdb_order[i] <- temp.pbdb$order[1]
          sepkoski$pbdb_family[i] <- temp.pbdb$family[1]
          sepkoski$pbdb_genus[i] <- temp.pbdb$genus_name[1]
          sepkoski$pbdb_accepted_no[i] <- temp.pbdb$accepted_no[1]
     } 
     if(i %% 5000 == 0) {print(i)} # just to keep track of progress
}

unused.orders <- unique(subset(sepkoski, is.na(pbdb_genus) & !is.element(sepkoski$order, pbdb_order))$order)
for(i in unused.orders) {
     this.order <- subset(sepkoski, sepkoski$order == i & !is.na(pbdb_genus))
     if(length(unique(this.order$pbdb_order)) == 1) {
          sepkoski$pbdb_order[sepkoski$order == i & is.na(sepkoski$pbdb_order)] <- this.order$pbdb_order[1]
          #print(i)
     }
}

unused.classes <- unique(subset(sepkoski, is.na(pbdb_genus) & !is.element(class, pbdb_class))$class)
for(i in unused.classes) {
     this.class <- subset(sepkoski, class == i & !is.na(pbdb_genus))
     if(length(unique(this.class$pbdb_class)) == 1) {
          sepkoski$pbdb_class[sepkoski$class == i & is.na(sepkoski$pbdb_class)] <- this.class$pbdb_class[1]
          #print(i)
     }
}

## THERE ARE SOME GENERA WHOSE PBDB and SEPKOSKI CLASSES DO NOT MATCH
# Take care of those here
bad.class <- subset(sepkoski, class != pbdb_class)
params.new <- paste(c(
     "pres=regular", # only body fossils
     "taxon_status=all",
     "rank=genus",
     "show=class,attr,app",
     "limit=250",
     "vocab=pbdb"
), collapse="&")

#i <- 6358 # Gosseletia
#i <- 1 # Syphax
#for(i in 1:nrow(bad.class)) {
     # for this we have to use the name autofill option to get all the valid names and their id
#     url1 <- URLencode(paste0(api.base.auto, "name=", bad.class$GENUS[i], "&limit=100"))
#     temp.taxa <- fromJSON(url1)$records
     
#     overwrite.pbdb.to.na <- FALSE
     
#     if(!is.null(nrow(temp.taxa))) {
#          taxon.no <- subset(temp.taxa, rnk == 5)
#          if(nrow(taxon.no) > 0) {
#               url2 <- URLencode(paste0(api.base, 
#                                        params.new, "&taxon_id=",
#                                        paste(taxon.no$oid, collapse=","))
#               )
#               temp.genera <- fromJSON(url2)$records
#               temp.genera <- subset(temp.genera, taxon_name == bad.class$GENUS[i] & class == bad.class$CLASS[i])
#               if(nrow(temp.genera == 1)) {
#                    sepkoski$pbdb_phylum[i] <- temp.genera$phylum[1]
#                    sepkoski$pbdb_class[i] <- temp.genera$class[1]
#                    sepkoski$pbdb_order[i] <- temp.genera$order[1]
#                    sepkoski$pbdb_family[i] <- temp.genera$family[1]
#                    sepkoski$pbdb_genus[i] <- temp.genera$genus_name[1]
#                    sepkoski$pbdb_accepted_no[i] <- temp.genera$accepted_no[1]
#               } else if(nrow(temp.genera) > 1) {
#                    print(i)
#               } else {
#                    overwrite.pbdb.to.na <- TRUE
#               }
#          } else {
#               overwrite.pbdb.to.na <- TRUE
#          }
#     } 
#     
#     if(overwrite.pbdb.to.na == TRUE) {
#          sepkoski$pbdb_phylum[i] <- NA
#          sepkoski$pbdb_class[i] <- NA
#          sepkoski$pbdb_order[i] <- NA
#          sepkoski$pbdb_family[i] <- NA
#          sepkoski$pbdb_genus[i] <- NA
#          sepkoski$pbdb_accepted_no[i] <- NA  
#     }
#     if(i %% 200 == 0) {
#          print0("Progress: ", i) # just to keep track
#     }
#}

write.csv(sepkoski, file="sepkoski.csv", na="", row.names = FALSE)
write.csv(sep.pbdb, file="pbdb_genenera_in_sepkoski.csv", na="", row.names = FALSE) # just in case you want it
