### SET WORKING DIRECTORY
my.wd <- "~/Documents/PBDB_Scripts/PBDB_Occurrences/"
setwd(my.wd)

### SET TAXON SCOPE
#which.phyla <- "all.phyla" # all phyla
#which.phyla <- "major.phyla" # phyla with >= 2000 occurrences
which.phyla <- "canonical.phyla" # mollusks, arthropods, brachiopods, bryozoa, echinodermata, cnidaria, porifera, chordates, hemichordates (subclass Graptolithina only)
#which.phyla <- "canonical.bilaterians" # mollusks, arthropods, brachiopods, echinodermata, and chordates

### Load Libraries
library(fossilbrush)

### General Set Up
api.base <- "https://paleobiodb.org/data1.2/occs/list.csv?"

### EXTERNAL DATA FILES FOR SEPKOSKI & MARINE AND TERRESTRAIL TAXA
taxon.realms <- read.csv(file="Marine_Nonmarine_Taxa.csv")
sepkoski <- read.csv(file="../Sepkoski/sepkoski.csv") # this will overwrite the sepkoski data fram from fossil brush
timescale <- read.csv(file="../Timescale/timescale_2020.csv")

# FIX TYPOS IN GTS_2020
GTS_2020$LAD[GTS_2020$Interval == "Changhsingian"] <- 251.902 # was 251.901
GTS_2020$LAD[GTS_2020$Interval == "Stage 10"] <- 486.85 # was 485.85
GTS_2020$Interval[GTS_2020$Interval == "Palaeozoic"] <- "Paleozoic" # using American spelling

### MARINE AND TERRESTRIAL ENVIRONMENTS

# Marine environments -- defined in taxa
# oligotrophic, mesotrophic, eutrophic, and hypersaline were excluded because of ambiguity
mar.taxa <- c(
     "marine",
     "lagoonal",	
     "coastal",
     "inner shelf",
     "outer shelf",	
     "oceanic",
     "brackish"
)

# Terrestrial Environments -- defined in taxa
ter.taxa <- c(
     "freshwater",
     "lacustrine"
)

# Marine environments -- defined in collections
mar.env <- c(
     "marine indet.",
     "offshore shelf",                      
     "shallow subtidal indet.",
     "deep subtidal shelf",
     "offshore",
     "sand shoal",                          
     "peritidal",
     "lagoonal",                          
     "slope",
     "offshore ramp",
     "deep subtidal ramp",
     "transition zone/lower shoreface",
     "delta front",
     "shoreface",                        
     "interdistributary bay",
     "carbonate indet.",              
     "reef, buildup or bioherm",
     "basinal (carbonate)",           
     "prodelta",
     "submarine fan",
     "delta plain",
     "foreshore",
     "open shallow subtidal",
     "deep subtidal indet.",
     "marginal marine indet.",
     "coastal indet.",
     "lagoonal/restricted shallow subtidal",
     "fluvial-lacustrine indet.",
     "paralic indet.",
     "deep-water indet.",
     "basinal (siliciclastic)",
     "deltaic indet.",
     "offshore indet.",                    
     "basin reef",
     "platform/shelf-margin reef",
     "perireef or subreef",
     "estuary/bay",
     "basinal (siliceous)",
     "intrashelf/intraplatform reef",
     "slope/ramp reef"
)

# Terrestrial Environments -- defined in collections
ter.env <- c(
     "pond",
     "lacustrine indet.",                   
     "terrestrial indet.",
     "\"floodplain\"",
     "fluvial indet.",
     "mire/swamp",
     "lacustrine - large",
     "fluvial-deltaic indet.",
     "alluvial fan",
     "lacustrine deltaic indet.",
     "wet floodplain",
     "lacustrine - small",
     "channel lag",
     "crevasse splay",
     "fine channel fill",
     "\"channel\"",                  
     "dry floodplain",
     "sinkhole",
     "cave",                           
     "crater lake",
     "glacial",
     "dune",
     "fissure fill",
     "coarse channel fill",
     "spring",
     "lacustrine delta plain",
     "levee",
     "karst indet.",
     "lacustrine prodelta",
     "loess",
     "eolian indet.",
     "lacustrine delta front",
     "interdune",
     "tar"
)


######
#
# Get Occurrences
#
######
phyla.url <- "https://paleobiodb.org/data1.2/taxa/list.csv?base_name=Animalia&rank=phylum&taxon_status=accepted&pres=regular&order=n_occs"

if(which.phyla == "all.phyla") {
     phyla.call <- read.csv(file=URLencode(phyla.url))
     my.phyla <- unique(phyla.call$taxon_name[phyla.call$n_occs > 0 & phyla.call$taxon_name != "Mollusca"])
} else if(which.phyla == "major.phyla") {
     phyla.call <- read.csv(file=URLencode(phyla.url))
     my.phyla <- unique(phyla.call$taxon_name[phyla.call$n_occs >= 2000 & phyla.call$taxon_name != "Mollusca"])
} else if(which.phyla == "canonical.phyla") {
     my.phyla <- c("Arthropoda", "Brachiopoda", "Bryozoa", "Chordata", "Cnidaria", "Echinodermata", "Graptolithina", "Porifera")
} else if(which.phyla == "canonical.bilaterians") {
     my.phyla <- c("Arthropoda", "Brachiopoda", "Chordata", "Echinodermata")
}
my.phyla <- c(my.phyla, c("Bivalvia","Gastropoda","Mollusca,^Gastropoda,^Bivalvia")) # mollusks have been split to prevent timing out

params <- paste(c(
     "pres=regular", # only body fossils
     "taxon_status=accepted",
     "idreso=lump_genus", # lump mult. species of the same genus in each collection into a single occurrence
     "show=attr,class,coords,paleoloc,abund,env,lith,ecospace,ent,entname"
), collapse="&")

# Because of the size of the request, it often times out
# to solve this I am requesting each phylum separately and by geological Era
pbdb.occs <- list()
k <- 1
for(i in my.phyla) {
     for(j in unique(timescale$Era)) {
          final.url <- paste0(api.base, paste0("base_name=",i,"&interval=",j,"&"), params)
          pbdb.call <- read.csv(file=URLencode(final.url))
          pbdb.occs[[k]] <- pbdb.call
          k <- k + 1
          print(paste(i, j, sep=" - ")) # this is to keep track of loop progress
     }
}

pbdb.occs <- do.call(rbind, pbdb.occs) # combine individual data frames into one
pbdb.occs <- subset(pbdb.occs, occurrence_no != "THIS REQUEST RETURNED NO RECORDS") # drop rows created by requests with no occurrences (e.g., Cenozoic Hyolitha)
save(pbdb.occs, file="raw_pbdb.RData") # just so I don't have to keep redownloading duing debugging

##### Take Care of the Subgenus Problem
# Even though subgenera should be lumped into genera, there are still some subgenera

pbdb.occs$genus_name <- clean_name(pbdb.occs$accepted_name)
pbdb.occs$genus_no <- pbdb.occs$accepted_no

subgen <- unique(subset(pbdb.occs, accepted_rank == "subgenus")$genus_name)

subgenera <- read.csv(file=URLencode(paste0("https://paleobiodb.org/data1.2/taxa/list.csv?taxon_name=",paste0(subgen, collapse=","),"&taxon_status=accepted&show=classext")))
for(i in 1:nrow(subgenera)) {
     pbdb.occs$genus_no[pbdb.occs$accepted_no == subgen[i]] <- subgenera$genus_no[subgenera$orig_no == subgen[i]]
     pbdb.occs$genus_name[pbdb.occs$accepted_no == subgen[i]] <- subgenera$genus[subgenera$orig_no == subgen[i]]
}

pbdb.occs$class_genus <- paste(pbdb.occs$class, pbdb.occs$genus_name, sep="_")

##### UPDATE HIGHER TAXA ONE-BY-ONE
# families
pbdb.occs$family[pbdb.occs$family == "Chilidiopsidae" & pbdb.occs$genus_name == "Hipparionix"] <- "Orthotetidae"
pbdb.occs$family[pbdb.occs$family == "Strophodontidae" & pbdb.occs$genus_name == "Strophomena"] <- "Strophomenidae"
pbdb.occs$family[pbdb.occs$family == "Hiatellidae" & pbdb.occs$genus_name == "Glycymeris"] <- "Glycymerididae"

# orders
pbdb.occs$order[pbdb.occs$order == "Adapedonta" & pbdb.occs$genus_name == "Glycymeris"] <- "Arcida"
pbdb.occs$order[pbdb.occs$order == "Porcellanidea" & pbdb.occs$genus_name == "Porcellana"] <- "Porcellanidae"
pbdb.occs$order[pbdb.occs$order == "Cidaroida" & pbdb.occs$genus_name == "Mesodiadema"] <- "Diadematoida"

###### FIX KNOWN NO_ORDER_SPECIFIED & NO_FAMILY_SPECIFIED
pbdb.occs$order[pbdb.occs$order == "NO_ORDER_SPECIFIED" & pbdb.occs$family == "Lepisosteidae"] <- "Lepisosteiformes"
pbdb.occs$order[pbdb.occs$order == "NO_ORDER_SPECIFIED" & pbdb.occs$family == "Sciaenidae"] <- "Lutjaniformes"
pbdb.occs$order[pbdb.occs$order == "NO_ORDER_SPECIFIED" & pbdb.occs$genus_name == "Frechastraea"] <- "Stauriida"
pbdb.occs$order[pbdb.occs$order == "NO_ORDER_SPECIFIED" & pbdb.occs$genus_name == "Bakevellia"] <- "Ostreida"
pbdb.occs$order[pbdb.occs$order == "NO_ORDER_SPECIFIED" & pbdb.occs$genus_name == "Nanogyra"] <- "Ostreida"
pbdb.occs$order[pbdb.occs$order == "NO_ORDER_SPECIFIED" & pbdb.occs$genus_name == "Didymoceras"] <- "Ammonitida"
pbdb.occs$order[pbdb.occs$order == "NO_ORDER_SPECIFIED" & pbdb.occs$genus_name == "Mesodiadema"] <- "Diadematoida"
pbdb.occs$order[pbdb.occs$order == "NO_ORDER_SPECIFIED" & pbdb.occs$genus_name == "Itieria"] <- "Heterostropha"
pbdb.occs$order[pbdb.occs$order == "NO_ORDER_SPECIFIED" & pbdb.occs$genus_name == "Pterynotus"] <- "Neogastropoda"
pbdb.occs$order[pbdb.occs$order == "NO_ORDER_SPECIFIED" & pbdb.occs$genus_name == "Forolinia"] <- "Labechiida"

pbdb.occs$family[pbdb.occs$family == "NO_FAMILY_SPECIFIED" & pbdb.occs$genus_name == "Atrypoidea"] <- "Septatrypidae"
pbdb.occs$family[pbdb.occs$family == "NO_FAMILY_SPECIFIED" & pbdb.occs$genus_name == "Frechastraea"] <- "Phillipsastreidae"
pbdb.occs$family[pbdb.occs$family == "NO_FAMILY_SPECIFIED" & pbdb.occs$genus_name == "Thamnasteria"] <- "Thamnasteriidae"
pbdb.occs$family[pbdb.occs$family == "NO_FAMILY_SPECIFIED" & pbdb.occs$genus_name == "Bakevellia"] <- "Bakevelliidae"
pbdb.occs$family[pbdb.occs$family == "NO_FAMILY_SPECIFIED" & pbdb.occs$genus_name == "Nanogyra"] <- "Gryphaeidae"
pbdb.occs$family[pbdb.occs$family == "NO_FAMILY_SPECIFIED" & pbdb.occs$genus_name == "Didymoceras"] <- "Nostoceratidae"
pbdb.occs$family[pbdb.occs$family == "NO_FAMILY_SPECIFIED" & pbdb.occs$genus_name == "Mesodiadema"] <- "Diadematidae"
pbdb.occs$family[pbdb.occs$family == "NO_FAMILY_SPECIFIED" & pbdb.occs$genus_name == "Itieria"] <- "Nerineidae"
pbdb.occs$family[pbdb.occs$family == "NO_FAMILY_SPECIFIED" & pbdb.occs$genus_name == "Pterynotus"] <- "Muricidae"
pbdb.occs$family[pbdb.occs$family == "NO_FAMILY_SPECIFIED" & pbdb.occs$genus_name == "Tiberia"] <- "Pyramidellidae"
pbdb.occs$family[pbdb.occs$family == "NO_FAMILY_SPECIFIED" & pbdb.occs$genus_name == "Rossella"] <- "Rossellidae"
pbdb.occs$family[pbdb.occs$family == "NO_FAMILY_SPECIFIED" & pbdb.occs$genus_name == "Atrypoidea"] <- "Atrypida"
pbdb.occs$family[pbdb.occs$family == "NO_FAMILY_SPECIFIED" & pbdb.occs$genus_name == "Forolinia"] <- "Rosenellidae"

##### GENERA TO DROP
genera.to.drop <- c(
     "Komiella" # This genus is assigned to two different classes of brachiopods
)
pbdb.occs <- subset(pbdb.occs, !is.element(genus_name, genera.to.drop))

genera.to.drop <- c(
     "Cephalopoda_Billingsites", # entered twice with same type species, but very different ages
     "Cephalopoda_Onychoceras", # entered twice, but very different ages
     "Cephalopoda_Treptoceras", # entered twice, but very different ages
     "Gastropoda_Domiporta", # entered twice, but very different ages
     "Hexactinellida_Testiispongia" # entered twice, but not sure what's happening
)
pbdb.occs <- subset(pbdb.occs, !is.element(class_genus, genera.to.drop))

##### Clean Taxonomy using list produced by fossilbrush
syns <- read.csv("synonym_fixes.csv")
name.cols <- c("t1","t2")
syns <- subset(syns, is.element(use_this, name.cols))
for(i in 1:nrow(syns)) {
     this.syn <- syns[i,]
     this.rank <- this.syn$level
     keep <- this.syn[match(this.syn$use_this, names(this.syn))]
     remove <- syns[i,match(name.cols[name.cols != names(keep)], colnames(syns))]
     rank.col <- match(this.rank, colnames(pbdb.occs))
     full.rank.column <- pbdb.occs[, rank.col]
     full.rank.column[full.rank.column == remove] <- as.character(keep)
     pbdb.occs[, rank.col] <- full.rank.column
}

#####
#
# Determine Marine Affinity
#
#####
# initialize marine column to NA for all taxa. Values are marine, likely_marine, assumed_marine, non_marine, likely_non_marine
pbdb.occs$marine <- NA 

# set marine Phyla 
mar.phyla <- subset(taxon.realms, realm == "marine" & taxon_rank == "phylum")$taxon_name
pbdb.occs$marine[is.element(pbdb.occs$phylum, mar.phyla)] <- "marine" 

# set Tetrapods to FALSE by default
pbdb.occs$marine[is.element(pbdb.occs$class, c("Reptilia","Aves","Mammalia","Amphibia"))] <- "non_marine" 

# set Marine Higher Taxa
marine.classes <- subset(taxon.realms, realm == "marine" & taxon_rank == "class")$taxon_name
marine.orders <- subset(taxon.realms, realm == "marine" & taxon_rank == "order")$taxon_name
marine.families <- subset(taxon.realms, realm == "marine" & taxon_rank == "family")$taxon_name
marine.genera <- subset(taxon.realms, realm == "marine" & taxon_rank == "genus")$taxon_name

pbdb.occs$marine[is.element(pbdb.occs$class, marine.classes)] <- "marine" # classes
pbdb.occs$marine[is.element(pbdb.occs$order, marine.orders)] <- "marine" # orders
pbdb.occs$marine[is.element(pbdb.occs$family, marine.families)] <- "marine" # families
pbdb.occs$marine[is.element(pbdb.occs$genus_name, marine.genera)] <- "marine" # genera

# set Terrestrial Higher Taxa
terrestrial.classes <- subset(taxon.realms, realm == "non_marine" & taxon_rank == "class")$taxon_name
terrestrial.orders <- subset(taxon.realms, realm == "non_marine" & taxon_rank == "order")$taxon_name
terrestrial.families <- subset(taxon.realms, realm == "non_marine" & taxon_rank == "family")$taxon_name
terrestrial.genera <- subset(taxon.realms, realm == "non_marine" & taxon_rank == "genus")$taxon_name

pbdb.occs$marine[is.element(pbdb.occs$class, terrestrial.classes)] <- "non_marine" # classes
pbdb.occs$marine[is.element(pbdb.occs$order, terrestrial.orders)] <- "non_marine" # orders
pbdb.occs$marine[is.element(pbdb.occs$family, terrestrial.families)] <- "non_marine" # families
pbdb.occs$marine[is.element(pbdb.occs$genus_name, terrestrial.genera)] <- "non_marine" # genera

# set Sepkoski Genera to Marine
for(i in 1:nrow(sepkoski)) {
     pbdb.occs$marine[pbdb.occs$genus_name == sepkoski$pbdb_genus[i] &
          pbdb.occs$phylum == sepkoski$pbdb_phylum[i]] <- "marine"   
}

###############
# Assess by collection env type
unassigned <- subset(pbdb.occs, is.na(marine))
unassigned.genera <- unique(unassigned$class_gen)
assigned <- subset(pbdb.occs, !is.na(marine)) # checking and validating
assigned.genera <- unique(assigned$class_gen) # checking and validating

## Fill in some missing taxonomy
missed <- unique(unassigned$class_genus[is.element(unassigned$class_genus, assigned$class_genus)])
for(i in missed) {
     temp.assign <- subset(assigned, class_genus == i)
     if(length(unique(temp.assign$accepted_no))==1) {
          pbdb.occs$accepted_no[pbdb.occs$class_genus == i & is.na(pbdb.occs$marine)] <- temp.assign$accepted_no[1]
          pbdb.occs$class[pbdb.occs$class_genus == i & is.na(pbdb.occs$marine)] <- temp.assign$class[1]
          pbdb.occs$order[pbdb.occs$class_genus == i & is.na(pbdb.occs$marine)] <- temp.assign$order[1]
          pbdb.occs$family[pbdb.occs$class_genus == i & is.na(pbdb.occs$marine)] <- temp.assign$family[1]
          pbdb.occs$marine[pbdb.occs$class_genus == i & is.na(pbdb.occs$marine)] <- temp.assign$marine[1]
     } else {
          print(i)
     }
}
unassigned <- subset(pbdb.occs, is.na(marine))
unassigned.genera <- unique(unassigned$class_genus)

## NOW use proportions of terrestrial and marine evns.
for(i in unassigned.genera) {
     n.mar <- nrow(subset(unassigned, class_genus == i & is.element(environment, mar.env)))
     n.ter <- nrow(subset(unassigned, class_genus == i & is.element(environment, ter.env)))
     
     if(n.mar > 0 & n.mar == n.ter) {
          pbdb.occs$marine[pbdb.occs$class_genus == i] <- "assumed_marine"
     } else if(n.mar > n.ter) {
          pbdb.occs$marine[pbdb.occs$class_genus == i] <- "likely_marine"
     } else if(n.mar < n.ter) {
          pbdb.occs$marine[pbdb.occs$class_genus == i] <- "likely_non_marine"
     }
}
unassigned <- subset(pbdb.occs, is.na(marine))
unassigned.genera <- unique(unassigned$class_genus)

# LASTLY use taxon_environment
pbdb.occs$marine[is.na(pbdb.occs$marine) & grepl("marine", pbdb.occs$taxon_environment)] <- "likely_marine"
pbdb.occs$marine[is.na(pbdb.occs$marine) & pbdb.occs$taxon_environment != "" & !grepl("marine", pbdb.occs$taxon_environment)] <- "likely_non_marine"
unassigned <- subset(pbdb.occs, is.na(marine))
unassigned.genera <- unique(unassigned$class_genus)
pbdb.occs$marine[is.na(pbdb.occs$marine)] <- "unknown"

###### UPDATE GEOLOGICAL TIMESCALE USING FOSSILBRUSH
pbdb.occs <- chrono_scale(pbdb.occs, srt = "early_interval", end = "late_interval", max_ma = "max_ma", min_ma = "min_ma")


##### DROP OCCURRENCES WHERE LAD >= FAD
pbdb.occs <- subset(pbdb.occs, newFAD > newLAD)

###### DROP OCCURRENCES WHOSE age range is > than the longest Phanerozoic Stage + 1 myr, the Norian
base.phan <- max(timescale$age_bottom[timescale$Period=="Cambrian"]) # keep precambrian occurrences
max.dur <- timescale$age_bottom[timescale$Age == "Norian"] - timescale$age_top[timescale$Age == "Norian"] + 1

pbdb.occs <- subset(pbdb.occs, newFAD > base.phan | newFAD - newLAD <= max.dur )

##### GitHub won't let us upload the uncompressed occurrence dataset.

write.csv(pbdb.occs, file=gzfile("pbdb_occs_mar.csv.gz"), row.names=FALSE, na="")

