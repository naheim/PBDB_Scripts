### SET WORKING DIRECTORY
my.wd <- "~/Documents/PBDB_Scripts/Sepkoski/"
setwd(my.wd)

### Load Libraries
library(fossilbrush)
library(jsonlite)

### General Set Up
api.base <- "https://paleobiodb.org/data1.2/taxa/list.json?"

params <- paste(c(
     "pres=regular", # only body fossils
     "taxon_status=all",
     "rank=genus",
     "show=class,attr,app",
     "limit=250",
     "vocab=pbdb"
), collapse="&")


###### ADD & CLEAN UP SEPKOSKI DATASET
data("sepkoski")
sepkoski <- subset(sepkoski, PHYLUM != "Protista")
sepkoski <- cbind('index' = 1:nrow(sepkoski), sepkoski)
mult.gen <- table(sepkoski$GENUS)
repeats <- names(mult.gen[mult.gen > 1])
for(i in repeats) {
     temp <- subset(sepkoski, GENUS == i)
     temp <- temp[order(temp$RANGE_BASE, decreasing=TRUE),]
     
     if(diff(temp$RANGE_BASE) <= 0 & diff(temp$RANGE_TOP) >= 0) { # if ranges are fully overlapping
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
          paste(sepkoski$GENUS[iterate[i]:(iterate[i+1]-1)], collapse=","))
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

# ADD PBDB HIGHER TAXONOMY TO SEPKOSKI
sepkoski$pbdb_phylum <- NA
sepkoski$pbdb_class <- NA
sepkoski$pbdb_order <- NA
sepkoski$pbdb_family <- NA
sepkoski$pbdb_genus <- NA
sepkoski$pbdb_accepted_no <- NA

sep.anim.phy <- c("Annelida","Arthropoda","Brachiopoda","Bryozoa",
     "Chordata","Cnidaria","Echinodermata","Hemichordata","Mollusca","Porifera")
for(i in 1:nrow(sepkoski)) {
     if(is.na(sepkoski$PHYLUM[i]) | sepkoski$PHYLUM[i] == "Protista") {
          temp.pbdb <- subset(sep.pbdb, accepted_rank == "genus" & !is.element(phylum, sep.anim.phy) & (accepted_name == sepkoski$GENUS[i] | genus == sepkoski$GENUS[i] | taxon_name == sepkoski$GENUS[i]))
     } else {
          temp.pbdb <- subset(sep.pbdb, accepted_rank == "genus" & phylum == sepkoski$PHYLUM[i] & (accepted_name == sepkoski$GENUS[i] | genus == sepkoski$GENUS[i] | taxon_name == sepkoski$GENUS[i]))
     }
     if(nrow(temp.pbdb) == 1) {
          sepkoski$pbdb_phylum[i] <- temp.pbdb$phylum[1]
          sepkoski$pbdb_class[i] <- temp.pbdb$class[1]
          sepkoski$pbdb_order[i] <- temp.pbdb$order[1]
          sepkoski$pbdb_family[i] <- temp.pbdb$family[1]
          sepkoski$pbdb_genus[i] <- temp.pbdb$accepted_name[1]
          sepkoski$pbdb_accepted_no[i] <- temp.pbdb$accepted_no[1]
     } else if (nrow(temp.pbdb) > 1) {
          temp.pbdb <- subset(temp.pbdb, taxon_name == sepkoski$GENUS[i])
          if(nrow(temp.pbdb) > 1) {print(i)}
          sepkoski$pbdb_phylum[i] <- temp.pbdb$phylum[1]
          sepkoski$pbdb_class[i] <- temp.pbdb$class[1]
          sepkoski$pbdb_order[i] <- temp.pbdb$order[1]
          sepkoski$pbdb_family[i] <- temp.pbdb$family[1]
          sepkoski$pbdb_genus[i] <- temp.pbdb$accepted_name[1]
          sepkoski$pbdb_accepted_no[i] <- temp.pbdb$accepted_no[1]
     } 
     if(i %% 5000 == 0) {print(i)} # just to keep track of progress
}

unused.orders <- unique(subset(sepkoski, is.na(pbdb_genus) & !is.element(ORDER, pbdb_order))$ORDER)
for(i in unused.orders) {
     this.order <- subset(sepkoski, ORDER == i & !is.na(pbdb_genus))
     if(length(unique(this.order$pbdb_order)) == 1) {
          sepkoski$pbdb_order[sepkoski$ORDER == i & is.na(sepkoski$pbdb_order)] <- this.order$pbdb_order[1]
          #print(i)
     }
}

unused.classes <- unique(subset(sepkoski, is.na(pbdb_genus) & !is.element(CLASS, pbdb_class))$CLASS)
for(i in unused.classes) {
     this.class <- subset(sepkoski, CLASS == i & !is.na(pbdb_genus))
     if(length(unique(this.class$pbdb_class)) == 1) {
          sepkoski$pbdb_class[sepkoski$CLASS == i & is.na(sepkoski$pbdb_class)] <- this.class$pbdb_class[1]
          #print(i)
     }
}

write.csv(sepkoski, file="sepkoski.csv", na="", row.names = FALSE)
write.csv(sep.pbdb, file="pbdb_genenera_in_sepkoski.csv") # just in case you want it

