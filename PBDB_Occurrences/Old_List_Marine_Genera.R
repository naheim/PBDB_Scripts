# Set Up
api.base <- "https://paleobiodb.org/data1.2/taxa/list.csv?"

# Only include phyla with at least this number of occurrences
occ.threshold <- 2000

### MARINE AND TERRESTRAIL TAXA
mar.phyla <- c("Brachiopoda","Cnidaria","Echinodermata","Bryozoa","Porifera","Hemichordata","Annelida","Hyolitha")

ter.classes <- c("Insecta","Ornithischia","Saurischia","Myriapoda","Arachnida","Amphibia","Chilopoda")


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
     "marine", # Any marine environment
     "carbonate", # Carbonate marine
     "silicic", # Siliciclastic marine
     "marginal", # Marginal marine
     "reef", # Any reef
     "stshallow", # Shallow subtidal
     "stdeep", # Deep subtidal
     "offshore", # Offshore
     "slope", # Slope
     "basin", # Basin
     "marindet", # Marine indet.
     "unknown" # Environment not recorded
)

# Terrestrial Environments -- defined in collections
ter.env <- c(
     "terrestrial", # Any terrestrial environment
     "lacustrine", # Lacustrine
     "fluvial", # Fluvial
     "karst", # Karst
     "glacial", # Glacial
     "terrother", # Other terrestrial
     "terrindet" # Terrestrial indet.
)




### GET PHYLA 

# Get all animal phyla in the PBDB
phyla.params <- params <- paste(c(
     "base_name=Metazoa",
     "pres=regular", # only body fossils
     "taxon_status=accepted",
     "rank=phylum",
     "show=size",
     "order=n_occs"
), collapse="&")

phyla.url <- paste0(api.base, phyla.params)

all.phyla <- read.csv(file=URLencode(phyla.url))

# Major phyla, including Problematica
major.phyla <- subset(all.phyla, n_occs >= occ.threshold)$taxon_name


### GET GENERA 

# Get Genera from Major Phyla
genera.params <- params <- paste(c(
     paste0("base_name=",paste0(major.phyla, collapse=",")),
     "pres=regular", # only body fossils
     "taxon_status=accepted",
     "rank=genus",
     "show=classext,attr,app,acconly,parent,ecospace,etbasis,size"
), collapse="&")

genera.url <- paste0(api.base, genera.params)

all.genera <- read.csv(file=URLencode(genera.url))






