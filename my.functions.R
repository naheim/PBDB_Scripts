# This functions takes a PBDB Download of Occurrences or Taxa (at the genus level) and Returns it with cleaned taxonomy
# The function also creates a new column called genus_name that does not have any subgenera
clean.pbdb.taxa <- function(pbdb.data) {
     require(fossilbrush)
     
     # Change some column names if accepted_name and accepted_no are not present
     if(!is.element("accpeted_name", colnames(pbdb.data))) {
          colnames(pbdb.data)[colnames(pbdb.data) == "genus"] <- "accepted_name"
          colnames(pbdb.data)[colnames(pbdb.data) == "taxon_no"] <- "accepted_no"
          colnames(pbdb.data)[colnames(pbdb.data) == "taxon_rank"] <- "accepted_rank"
     }
     
     ##### Take Care of the Subgenus Problem
     # Even though subgenera should be lumped into genera, there are still some subgenera
     pbdb.data$genus_name <- clean_name(pbdb.data$accepted_name)
     pbdb.data$genus_no <- pbdb.data$accepted_no
     
     subgen <- unique(subset(pbdb.data, accepted_rank == "subgenus")$genus_name)
     if(length(subgen) > 0) {
          subgenera <- read.csv(file=URLencode(paste0("https://paleobiodb.org/data1.2/taxa/list.csv?taxon_name=",paste0(subgen, collapse=","),"&taxon_status=accepted&show=classext")))
          for(i in 1:nrow(subgenera)) {
               pbdb.data$genus_no[pbdb.data$accepted_no == subgen[i]] <- subgenera$genus_no[subgenera$orig_no == subgen[i]]
               pbdb.data$genus_name[pbdb.data$accepted_no == subgen[i]] <- subgenera$genus[subgenera$orig_no == subgen[i]]
          }
     }
     
     ##### UPDATE HIGHER TAXA TO FIX (mostly missing family and order)
     fix.by.fam <- read.csv(file="../Taxon_Cleaning_Files/taxonomy_fixes/families-fix_by_family.csv")
     fix.by.gen <- read.csv(file="../Taxon_Cleaning_Files/taxonomy_fixes/genera-fix_by_genus.csv")
     for(i in 1:nrow(fix.by.fam)) {
          if(!is.na(fix.by.fam$family_class[i])) {
               pbdb.data$class[pbdb.data$family == fix.by.fam$family_to_fix[i]] <- fix.by.fam$family_class[i]
          }
          if(!is.na(fix.by.fam$family_order[i])) {
               pbdb.data$order[pbdb.data$family == fix.by.fam$family_to_fix[i]] <- fix.by.fam$family_order[i]
          }
     }
     
     for(i in 1:nrow(fix.by.gen)) {
          if(!is.na(fix.by.gen$genus_class[i])) {
               pbdb.data$class[pbdb.data$genus_name == fix.by.gen$genus_to_fix[i]] <- fix.by.gen$genus_class[i]
          }
          if(!is.na(fix.by.gen$genus_order[i])) {
               pbdb.data$order[pbdb.data$genus_name == fix.by.gen$genus_to_fix[i]] <- fix.by.gen$genus_order[i]
          }
          if(!is.na(fix.by.gen$genus_family[i])) {
               pbdb.data$family[pbdb.data$genus_name == fix.by.gen$genus_to_fix[i]] <- fix.by.gen$genus_family[i]
          }
     }
     
     ##### Clean Taxonomy using list produced by fossilbrush
     syns <- read.csv("../Taxon_Cleaning_Files/synonym_fixes.csv")
     name.cols <- c("t1","t2")
     syns <- subset(syns, is.element(use_this, name.cols))
     for(i in 1:nrow(syns)) {
          this.syn <- syns[i,]
          this.rank <- this.syn$level
          keep <- this.syn[match(this.syn$use_this, names(this.syn))]
          remove <- syns[i,match(name.cols[name.cols != names(keep)], colnames(syns))]
          rank.col <- match(this.rank, colnames(pbdb.data))
          full.rank.column <- pbdb.data[, rank.col]
          full.rank.column[full.rank.column == remove] <- as.character(keep)
          pbdb.data[, rank.col] <- full.rank.column
     }
     
    return(pbdb.data) 
}

