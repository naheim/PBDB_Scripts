### SET WORKING DIRECTORY
my.wd <- "~/Documents/PBDB_Scripts/Marine_Genera/"
setwd(my.wd)

### Load Libraries
library(fossilbrush)

### General Set Up
api.base <- "https://paleobiodb.org/data1.2/occs/list.csv?"

### EXTERNAL DATA FILES FOR SEPKOSKI & MARINE AND TERRESTRAIL TAXA
pbdb.occs <- read.csv(file=gzfile("../PBDB_Occurrences/pbdb_occs.csv.gz"))
sepkoski <- read.csv(file="../Sepkoski/sepkoski.csv") # this will overwrite the sepkoski data fram from fossil brush

########
##
## MAKE A DATA FRAME OF MARINE GENERA
##
#######
# get marine occurrences
marine.occs <- subset(pbdb.occs, is.element(marine, c("marine","likely_marine")))

# select occurrence columns to include in genus data frame
genus.columns <- c("phylum","class","order","family","genus_name","class_genus")

# create a genus-level data frame for marine taxa
marine.genera <- unique(marine.occs[, match(genus.columns, colnames(marine.occs))])
marine.genera <- marine.genera[order(marine.genera$phylum, marine.genera$class, marine.genera$order, marine.genera$family, marine.genera$genus_name),]

# limit sepkoski to just those genera in PBDB
sepkoski <- subset(sepkoski, is.element(pbdb_phylum, unique(marine.genera$phylum)))
sepkoski <- sepkoski[order(sepkoski$pbdb_phylum, sepkoski$pbdb_class, sepkoski$pbdb_order, sepkoski$pbdb_genus),]
sepkoski$class_genus <- paste(sepkoski$pbdb_class, sepkoski$pbdb_genus, sep="_")
sepkoski$range_duration <- sepkoski$max_ma - sepkoski$min_ma

# make some important calculations
pbdb.n.occ <- table(marine.occs$class_genus)
pbdb.fad <- tapply(marine.occs$max_ma, marine.occs$class_genus, max)
pbdb.lad <- tapply(marine.occs$min_ma, marine.occs$class_genus, min)
marine.genera$n.occs <- pbdb.n.occ[match(marine.genera$class_genus, names(pbdb.n.occ))]
marine.genera$pbdb.fad <- pbdb.fad[match(marine.genera$class_genus, names(pbdb.fad))]
marine.genera$pbdb.lad <- pbdb.lad[match(marine.genera$class_genus, names(pbdb.lad))]

write.csv(marine.genera, file=gzfile("marine_genera.csv.gz"), na="", row.names=FALSE)
