# Introduction
This repository contains a series of R scripts that are used sequentially to download and process marine occurrences and genera from the Paleobiology Database (PBDB). These scripts make use of the `fossilbrush` package as well as the PBDB API <https://paleobiodb.org/data1.2/>.

# Scripts

1. Timescale -- this generates two versions of the geological timescale, 2020 and 2013. 
2. Sepkoski -- this cleans up the Spekoski dataset (from fossilbrush) and adds PBDB higher taxonomy.
3. PBDB Occurrences -- this downloads and processes animal occurrences from the PBDB
4. Marine Genera -- this created a genus level dataset from Sepkoski and PBDB Occurrences

# Links to Data Files & R Code To Download 

## The Geological Timescale

There are two timescales. The native PBDB timescale, which is based on the International Comission on Stratigraphy 2024 timescale. The other is from the ``fossilbrush`` R package and is based on the Gradstein 2020 timescale (i.e., the current ICS Timescale).


* [timescale_2024.csv](Timescale/timescale_2024.csv)
* [timescale_2020.csv](Timescale/timescale_2020.csv)

```{r}
timescale.2024 <- read.csv(file="https://raw.githubusercontent.com/naheim/PBDB_Scripts/master/Timescale/timescale_2024.csv")

timescale.2020 <- read.csv(file="https://raw.githubusercontent.com/naheim/PBDB_Scripts/master/Timescale/timescale_2020.csv")
```

## Sepkoski's Compendium of Fossil Marine Genera

Note, I have dropped "Protista" from Sepkoski's compendium so it only includes animals.

* [sepkoski.csv](Sepkoski/sepkoski.csv)

```{r}
sepkoski <- read.csv(file="https://raw.githubusercontent.com/naheim/PBDB_Scripts/master/Sepkoski/sepkoski.csv")
```

## List of "cleaned" Paleobiology Database Occurrences

* [pbdb\_occs.csv.gz](PBDB\_Occurrences/pbdb\_occs.csv.gz)

```{r}
library(readr) # need this package to easily read zipped file

pbdb.occs <- as.data.frame(read_csv("https://raw.githubusercontent.com/naheim/PBDB_Scripts/master/PBDB_Occurrences/pbdb_occs.csv.gz"))
```


## List of "cleaned" Paleobiology Database Marine Genera

* [marine_genera.csv](Marine\_Genera/marine\_genera.csv)

```{r}
library(readr) # need this package to easily read zipped file

marine.genera <- as.data.frame(read_csv("https://raw.githubusercontent.com/naheim/PBDB_Scripts/master/Marine_Genera/marine_genera.csv.gz"))
```
