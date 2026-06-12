### SET WORKING DIRECTORY
my.wd <- "~/Library/CloudStorage/Dropbox/PBDB_Scripts/PBDB_Occurrences/"
setwd(my.wd)

# LOAD LIBRARIES
library(worrms)
library(dplyr)

get.taxon.list <- function(taxa, rank) {
     api.base <- "https://paleobiodb.org/data1.2/taxa/list.csv?"
     
     params.1 <- paste(c(
          "pres=regular", # only body fossils
          "taxon_status=accepted",
          "show=class"
     ), collapse="&")
     
     params.2 <- paste(c(
          paste0("taxon_name=",paste0(taxa, collapse=",")),
          paste0("rank=",rank)
     ), collapse="&")
     
     pbdb.url <- paste0(api.base, params.1, "&", params.2); #print(pbdb.url)
     pbdb <- read.csv(file=URLencode(pbdb.url))
     return(pbdb)
}

mar.phyla <- c("Brachiopoda","Cnidaria","Echinodermata","Bryozoa","Porifera","Hemichordata",
     "Hyolitha","Problematica","Scalidophora","Priapulida","Chaetognatha","Proarticulata","Ctenophora",
     "Sipuncula","Entoprocta")
mp <- get.taxon.list(mar.phyla, "phylum")

mar.class <- c('Marrellomorpha','Marrelomorpha','Remipedia','Cephalocarida','Archaeocyatha',
               'Granuloreticulosea','Rhizopodea','Bacillariophyceae','Cephalopoda','Polyplacophora',
               'Aplacophora','Helcionelloida','Monoplacophora','Rostroconchia','Thaliacea',
               'Ascidiacea','Trilobita','Merostomata','Cirripedia','Pycnogonida','Merostomoidea','Scaphopoda',
               'Tentaculita','Tergomya','Xiphosura','Aculifera','Artiopoda','Leptocardii','Megacheira','Myxini',
               'Paragastropoda','Paratrilobita','Stenothecoida','Thylacocephala','Thecostraca','Nektaspidida',
               'Banffozoa')
mc <- get.taxon.list(mar.class, "class")

mar.order <- c('Marrellida','Euphausiacea','Amphionidacea','Cumacea','Lophogastrida','Calpionellida',
               'Scaphopoda','Archaeogastropoda','Bellerophontida','Cardiida','Carditida','Trigoniida',
               'Pterioida','Pleurotomariida','Solemyoida','Nuculanida','Mytiloida','Hippuritida','Euomphalina',
               'Arcoida','Ostreoida','Pectinoida','Limoida','Praecardioida',
               "Pectinida","Mytilida","Arcida","Archaeostraca",     
               "Cyrtodontida","Myalinida","Colpomyida",         
               "Ostreida","Pterioida","Limida","Scalpellomorpha",
               "Calanticomorpha","Verrucomorpha","Stomatopoda","Pollicipomorpha",
               "Balanomorpha","Xiphosurida","Archaeolepadomorpha","Aeschronectida",
               "Leptostraca","Eolepadomorpha","Lithoglyptida","Dendrogastrida",
               "Palaeostomatopoda","Hoplostraca")
mo <- get.taxon.list(mar.order, "order")

mar.family <- c('Otinidae','Buccinidae','Marginellidae','Littorinidae','Pyramidellidae','Volutidae',
                'Veneridae','Lucinidae','Turridae','Trochidae','Muricidae','Fasciolariidae','Cypraeidae',
                'Aporrhaidae',"Discinocarididae","Ischyrodontidae","Peltocarididae",
                "Ctenaenigmatidae","Zapfellidae","Peltogastridae","Kasibelinuridae","Matheriidae","Yunannidae",
                "Jerometichenoriidae","Tylocarididae","Tealliocarididae","Pygocephalidae",
                "Rostellariidae","Volutidae","Terebridae","Columbellidae","Clathurellidae","Borsoniidae","Conidae","Nassariidae",
                "Drilliidae","Melongenidae","Cochlespiridae","Raphitomidae","Strombidae","Colombellinidae",
                "Eocypraeidae","Clavatulidae","Pediculariidae","Harpidae","Mangeliidae","Pseudomelatomidae","Eratoidae",
                "Seraphsidae","Pholidotomidae","Coralliophilidae","Pyrenidae","Tudiclidae","Cystiscidae","Strepsiduridae",
                "Triviidae","Siphonaliidae","Horaiclavidae","Velutinidae","Mitromorphidae","Struthiolariidae","Ovulidae","Thersiteidae",
                "Magilidae","Buccinulidae","Strictispiridae","Strepturidae","Hippochrenidae","Colubrariidae",
                "Nuculidae","Ctenodontidae","Pholadomyidae","Praenuculidae","Nucularcidae","Paracyclidae","Solemyidae",
                "Chaenomyidae","Arcticidae","Mactridae","Kelliellidae","Veneridae","Verticordiidae","Ungulinidae","Mactromyidae",
                "Gastrochaenidae","Laternulidae","Hiatellidae","Glossidae","Cuspidariidae","Clavagellidae","Mesodesmatidae","Chamidae",
                "Galeommatidae","Anatinellidae","Trapezidae","Petricolina","Nucinellidae","Lasaeidae","Pollicidae","Spheniopsidae",
                "Pandoridae","Lyonsiidae","Manzanellidae","Isocyprinidae","Cardiliidae","Turtoniina","Penicillidae","Vesicomyidae",
                "Ucumariidae","Euloxidae","Saxicavellidae","Hemidonacidae","Margaritariidae")
mf <- get.taxon.list(mar.family, "family")

mar.genus <- c('Cobracephalus',"Nothozoe","Willwerathia","Dioxycaris","Maldybulakia","Micrichnus",
               "Patesia","Rhamphoverritor","Corobalanus","Chitobalanus","Ciurcalimulus","Sosiocaris","Liocaris",
               "Fulgerca","Leiorhinus","Sphenolium","Aksumya","Barcoona")
mg <- get.taxon.list(mar.genus, "genus")

mar.mammal.order <- c('Cetacea','Sirenia');
mmo <- get.taxon.list(mar.mammal.order, "order")

mar.mammal.family <- c('Otariidae','Phocidae','Odobenidae','Desmatophocidae')          
mmf <- get.taxon.list(mar.mammal.family, "family")

mar.mammal.genus <- c('Enhydra',"Enaliarctos","Pinnarctidion","Potamotherium","Kolponomos","Pteronarctos","Pacificotaria","Palaeotaria","Puijila","Semantor")
mmg <- get.taxon.list(mar.mammal.genus, "genus")

mar.reptile.order <- c('Sauropterygia','Ichthyosauria','Placodontia',"Plesiosauria",'Thalattosauria',"Eosauropterygia");
mro <- get.taxon.list(mar.reptile.order, "order")

mar.reptile.family <- c('Cheloniidae','Dermochelyidae','Protostegidae','Toxochelyidae',
                        'Thalassemyidae','Mosasauridae',"Metriorhynchidae","Teleosauridae","Machimosauridae",
                        "Euclastidae","Dermochelyoidae","Desmatochelyidae")
mrf <- get.taxon.list(mar.reptile.family, "family")

mar.reptile.genus <- c("Eoneustes","Deslongchampsina","Peipehsuchus","Pelagosaurus","Mystriosaurus","Plagiophthalmosuchus","Charitomenosuchus","Zoneait","Neosteneosaurus","Seldsienean","Teleidosaurus","Magyarosuchus","Clovesuurdameredeor","Andrianavoay","Opisuchus","Turnersuchus")
mrg <- get.taxon.list(mar.reptile.genus, "genus")

mar.bird.order <- c('Sphenisciformes', 'Procellariiformes','Pelecaniformes')
mbo <- get.taxon.list(mar.bird.order, "order")

mar.bird.family <- c('Hesperornithidae','Stercorariidae','Laridae','Sternidae','Alcidae','Rynchopidae','Scolopacidae')
mbf <- get.taxon.list(mar.bird.family, "family")

mar.fish.class <- c('Conodonta','Chondrichthyes','Agnatha','Pteraspidomorphi','Placodermi',
                    'Pituriaspididae','Anaspida')
mfc <- get.taxon.list(mar.fish.class, "class")

mar.fish.order <- c('Pachycormiformes','Conodontophorida','Pleuronectiformes',
                    "Lamniformes","Hybodontiformes","Odontaspidida","Hexanchiformes",    
                    "Sclerorhynchiformes","Myliobatiformes","Squatiniformes","Carcharhiniformes",
                    "Squaliformes","Orectolobiformes","Rhinopristiformes","Protacrodontiformes",
                    "Rajiformes","Xenacanthida","Synechodontiformes",
                    "Heterodontiformes","Pristiophoriformes","Echinorhiniformes","Batoidea",
                    "Torpediniformes","Anachronistiformes","Protospinaciformes","Selachii","Plagiostomi")
mfo <- get.taxon.list(mar.fish.order, "order")

mar.fish.family <- c("Listracanthidae","Agaleidae","Ostenoselachidae", 
                     "Belemnobatidae","Galeidae","Archaeobatidae","Cestraciontidae",  
                     "Pliotremidae","Lugalepididae","Thrinacodontidae")
mff <- get.taxon.list(mar.fish.family, "family")

mar.fish.genus <- c("Physonemus","Labascicorona","Minuticorona","Fragilicorona",
                    "Glabrisubcorona","Odontorhytis","Youssoubatis","Carcharopsis",
                    "Hopleacanthus","Parvidiabolus","Complanicorona","Gondwanodus",
                    "Surcaudalus","Grozonodon","Hypsobatis","Acronemus",
                    "Amelacanthus","Meishanselache","Vallisodus","Hueneichthys",
                    "Reifia","Serratocorona","Ornathiliabrilance","Carinasubcorona",
                    "Parvicorona","Annulicorona","Sacrisubcorona","Lobaticorona",
                    "Labrilancea","Eunemacanthus","Jurobatos","Duffinselache",
                    "Favusodus","Microtoxodus","Rhomaleodus","Raineria",
                    "Coniunctio","Ornatilabrilancea","Suaviloquentia","Undulaticorona",
                    "Parviscapha","Rugosicorona","Oniscina","Changxingselache",
                    "Nanocetorhinus","Artiodus","Enantiobatis","Cheirostephanus","Rosaodus","Tuberospina","Onchopristus","Eorapax")
mfg <- get.taxon.list(mar.fish.genus, "genus")

mar.amphibian.family <- c('Trematosauridae')
maf <- get.taxon.list(mar.amphibian.family, "family")

marine.taxa <- rbind(mp,mc,mo,mf,mg,
                     mmo,mmf,mmg,
                     mro,mrf,mrg,
                     mbo,mbf,
                     mfc,mfo,mff,mfg,
                     maf)
marine.taxa$realm <- "marine"

ter.class <- c('Insecta','Arachnida','Entognatha','Myriapoda','Saurischia','Ornithischia','Chilopoda','Collembola','Glossata','Pauropoda','Symphyla');
tc <- get.taxon.list(ter.class, "class")

ter.order <- c('Bathynellacea','Unionoida','Avetheropoda','Pterosauria','Gymnophiona','Xenacanthida',
               'Trituberculata','Saurischia','Ornithischia','Temnospondyli','Diplura','Anura',
               'Spongillida');
to <- get.taxon.list(ter.order, "order")

ter.family <- c('Anhingidae',"Euraxemydidae","Pelomedusidae","Podocnemidoidae","Podocnemididae",
                "Peiropemydidae","Araripemydidae","Sahonachelyidae","Bothremydidae","Chelidae","Panchelidae")
tf <- get.taxon.list(ter.family, "family")

ter.genus <- c('Leverhulmia','Onycholepisma',
               "Apodichelys","Itapecuruemys","Palaeaspis","Teneremys","Portezueloemys",
               "Brasilemys","Nostimochelone","Francemys","Caririemys","Stenotrema","Euconulus","Triodopsis",
               "Anguispira","Neohelix","Trochozonites","Mennoia","Neubertella","Discus","Pseudohercynella","Vasculum","Siphonacmea","Williamia","Anisomyon",
               "Siphonaria","Acroreia","Ptychogyra","Pleurolimnaea","Patellopsis","Bernicia")
tg <- get.taxon.list(ter.genus, "genus")

ter.bivalve.family <- c('Sphaeriidae','Corbiculidae','Dreissenidae','Hyriidae','Etheriidae','Mycetopodidae');
tbf <- get.taxon.list(ter.bivalve.family, "family")

ter.gastropod.family <- c('Aciculidae','Viviparidae','Amnicolidae','Bithyniidae','Cochliopidae','Valvatidae','Pomatiidae','Lithoglyphidae',
                          'Cochlicopidae','Pristilomatidae','Parmacellidae','Helicidae','Planorbidae','Thiaridae','Physidae','Ampullariidae','Moitessieriidae',
                          'Helicinidae','Neocyclotidae','Veronicellidae','Succineidae','Subulinidae','Streptaxidae','Orthalicidae','Oleacinidae',
                          'Agriolimacidae','Pleurodontidae','Polygyridae','Achatinidae','Pupillidae','Camaenidae','Lymnaeidae','Hydrocenidae','Rhytididae',
                          'Charopidae','Glacidorbidae','Chilina','Cyclophoridae','Helicodiscidae','Diplommatinidae','Milacidae','Pleuroceridae','Pomatiopsidae','Pyramidulidae',
                          'Valloniidae','Ellobiidae','Zonitidae','Ferussaciidae','Discidae','Strophocheilidae','Clausiliidae','Helminthoglyptidae',
                          'Paludomidae','Plectopylidae','Dorcasiidae','Cerastidae','Streptaxidae',
                          'Melanopsidae','Acroloxidae','Lauriidae','Gastrodontidae','Hygromiidae','Pachychilidae','Semisulcospiridae',
                          'Helicostoidae','Acochlidiidae','Chilinidae','Latiidae',"Ferussinidae","Maizaniidae","Alycaeidae","Craspedopomatidae",
                          "Pupinidae","Megalomastomatidae","Gastrocoptidae","Enidae",
                          "Orculidae","Argnidae","Partulidae","Pleurodiscidae",
                          "Chondrinidae","Azecidae","Truncatellinidae","Vertiginidae",
                          "Strobilopsidae","Acavidae","Megomphicidae","Ariolimacidae",
                          "Trissexodontidae","Helicodontidae","Geomitridae",
                          "Elonidae","Thysanophoridae","Xanthonychidae","Sphincterochilidae",
                          "Testacellidae","Filholiidae","Bulimulidae","Odontostomidae",
                          "Endodontidae","Oreohelicidae","Punctidae","Vitrinidae",
                          "Limacidae","Oxychilidae")
tgf <- get.taxon.list(ter.gastropod.family, "family")

terrestrial.taxa <- rbind(tc,to,tf,tg,
                     tbf,tgf)
terrestrial.taxa$realm <- "non_marine"

big.taxon.table <- rbind(marine.taxa, terrestrial.taxa)

ter.genus.extra <- c('Cremnoconchus','Clea','Rivomarginella','Urnatella')
ter.gastropod.family.extra <- c('Haplotrematidae','Euconulidae','Megaspiridae',
     'Bothriembryontidae','Sculptariidae','Micractaeonidae','Helicarionidae',
     'Staffordiidae','Tantulidae','Scolodontidae')

temp <- (do.call(rbind, wm_records_taxamatch(c(ter.gastropod.family.extra, ter.genus.extra), marine_only = FALSE)))
not.in.pbdb.yet <- cbind(temp$rank, temp$valid_name, temp$phylum, temp$class, temp$order, temp$family, temp$genus)
colnames(not.in.pbdb.yet) <- c("taxon_rank","taxon_name","phylum","class","order","family","genus")
not.in.pbdb.yet <- as.data.frame(not.in.pbdb.yet) 
not.in.pbdb.yet$realm <- "non_marine"
not.in.pbdb.yet$is_ectinct <- "extant"
not.in.pbdb.yet$taxon_rank <- tolower(not.in.pbdb.yet$taxon_rank)
not.in.pbdb.yet <- rbind(c("genus","Camptophyllia","Arthropoda","incertae sedis",NA,NA,"Camptophyllia","non_marine","extinct"), not.in.pbdb.yet)

big.taxon.table <- bind_rows(big.taxon.table, not.in.pbdb.yet)

write.csv(big.taxon.table, file="~/Library/CloudStorage/Dropbox/Useful Scripts/PBDB Occurrences/Marine_Nonmarine_Taxa.csv", na="")
dim(big.taxon.table)
