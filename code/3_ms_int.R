# this file calculates the integration factor for policy files
# Laura Gabriele Elsler, WMU

# clear workspace
rm(list = ls())
graphics.off()

# packages
library(textreadr)
library(pdftools)
library(pdfsearch)
library(tidyverse)

# directory 
dirct <- "~/Dropbox/WMU/analysis/data"

# function
readCleanCount<-function(pdf){    # uses pdftools to convert pdfs to plain-text, replaces line breaks with spaces and then counts the words, ignoring non-word symbols
  txt<-pdf_text(pdf)
  txt<-paste(gsub(txt,pattern="\r\n",replace=" "),collapse=" ")
  count<-sapply(gregexpr("[[:alpha:]]+", txt), function(x) sum(x > 0))
  return(count)
}

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# data
df_sff <- read.csv(file.path(dirct, 'analysis/ms_sff_all_step2.csv'), as.is=T) %>% select(-c(X))
df_cff <- read.csv(file.path(dirct, 'analysis/ms_cff_all_step2.csv'), as.is=T) %>% select(-c(X))


###################################################################################################################
#######################################   INTEGRATION ANALYSIS   ##################################################
###################################################################################################################

# split datasets which contain word with and w/o policies 
df_c_pol <- df_cff[grepl("United Nations Convention Framework for Climate Change|UNFCCC|Paris Agreement|Kyoto Protocol|London Protocol", df_cff$keyword),]
df_cff <- df_cff[!grepl("United Nations Convention Framework for Climate Change|UNFCCC|Paris Agreement|Kyoto Protocol|London Protocol", df_cff$keyword),]
df_s_pol <- df_sff[grepl("Convention on Biological Diversity|CBD|Aichi Biodiversity Targets|IPBES|intergovernmental science-policy platform on biodiversity and ecosystem services|Post-2020 Biodiversity Targets|CITES|Convention on International Trade in Endangered Species of Wild Fauna and Flora|CMS|Convention on the Conservation of Migratory Species of Wild Animals|UNEP|United Nations Environment Program|IWC|International Whaling Commission|ICRW|International Convention for the Regulation of Whaling|UNFSA|United Nations Fish Stock Agreement|Regional Fisheries Management Organizations|RFMO|FAO|Food and Agriculture Organization of the United Nations", df_sff$keyword),]
df_sff <- df_sff[!grepl("Convention on Biological Diversity|CBD|Aichi Biodiversity Targets|IPBES|intergovernmental science-policy platform on biodiversity and ecosystem services|Post-2020 Biodiversity Targets|CITES|Convention on International Trade in Endangered Species of Wild Fauna and Flora|CMS|Convention on the Conservation of Migratory Species of Wild Animals|UNEP|United Nations Environment Program|IWC|International Whaling Commission|ICRW|International Convention for the Regulation of Whaling|UNFSA|United Nations Fish Stock Agreement|Regional Fisheries Management Organizations|RFMO|FAO|Food and Agriculture Organization of the United Nations", df_sff$keyword),]


### matches
# spp-climate 
df_sff <- df_sff %>% mutate(kw_type = ifelse(line_text %in% c(grep("climate|climate change|climate mitigat|mitigating climate change| carbon|CO2|carbon pump|carbon flow|carbon flux|carbon seques|carbon cycl|carbon captur|carbon export|carbon stor|carbon uptake|carbon fix|carbon burial|carbon transformation|carbon transfer|carbon immobilization|carbon deposit|carbon stock|carbon sink|United Nations Convention Framework for Climate Change|UNFCCC|Paris Agreement|Kyoto Protocol|London Protocol", line_text,ignore.case=TRUE,value=TRUE)), "match",NA)) 
# climate-spp
df_cff <- df_cff %>% mutate(kw_type = ifelse(line_text %in% c(grep("species|marine species|organism|fauna|living resource|animal|creature|wildlife|biodiversity|food web|target species|fishing|fish stock|overfishing|anchovies|coalfish| cod |crab|cuttlefish|flatfish|haddock|halibut|herring|lobster|mackerel|mussel|octopus|plaice|rock lobster|salmon|sardines|scallop|seabass|shark|shrimp| sole |swordfish|tuna|zooplankton|macroalgae|seaweed|kelp|whale|dolphin|marine mammal|Convention on Biological Diversity|CBD|Aichi Biodiversity Targets|IPBES|intergovernmental science-policy platform on biodiversity and ecosystem services|Post-2020 Biodiversity Targets|CITES|Convention on International Trade in Endangered Species of Wild Fauna and Flora|CMS|Convention on the Conservation of Migratory Species of Wild Animals|UNEP|United Nations Environment Program|IWC|International Whaling Commission|ICRW|International Convention for the Regulation of Whaling|UNFSA|United Nations Fish Stock Agreement|Regional Fisheries Management Organizations|RFMO|FAO|Food and Agriculture Organization of the United Nations", line_text,ignore.case=TRUE,value=TRUE)), "match",NA)) 
# policy 
df_c_pol <- df_c_pol %>% mutate(kw_type = ifelse(line_text %in% c(grep("Convention on Biological Diversity|CBD|Aichi Biodiversity Targets|IPBES|intergovernmental science-policy platform on biodiversity and ecosystem services|Post-2020 Biodiversity Targets|CITES|Convention on International Trade in Endangered Species of Wild Fauna and Flora|CMS|Convention on the Conservation of Migratory Species of Wild Animals|UNEP|United Nations Environment Program|IWC|International Whaling Commission|ICRW|International Convention for the Regulation of Whaling|UNFSA|United Nations Fish Stock Agreement|Regional Fisheries Management Organizations|RFMO|FAO|Food and Agriculture Organization of the United Nations", line_text,ignore.case=TRUE,value=TRUE)), "match",NA)) 
df_s_pol <- df_s_pol %>% mutate(kw_type = ifelse(line_text %in% c(grep("United Nations Convention Framework for Climate Change|UNFCCC|Paris Agreement|Kyoto Protocol|London Protocol", line_text,ignore.case=TRUE,value=TRUE)), "match",NA)) 

# keep only cases with matches
match1 <- completeFun(df_sff, "kw_type")
match2 <- completeFun(df_cff, "kw_type")
match3 <- completeFun(df_s_pol, "kw_type")
match4 <- completeFun(df_c_pol, "kw_type")

############################################   CONDITIONS   #######################################################
# preliminary merge
match <- match1 %>% full_join(match2) 

# select  
match <- match %>% mutate(sequestration = ifelse(line_text %in% c(grep("sequest", line_text,ignore.case=TRUE,value=TRUE)), "sequestration",NA)) 

# remove all that do not match
match <- completeFun(match, "sequestration")

# merge all data
match <- match %>% full_join(match3) %>% full_join(match4) 

# remove conditional columns
match <- match %>% select(-sequestration)


###################################################################################################################
############################################### KEYWORD ANALYSIS   ################################################
###################################################################################################################

## keywords 
kw = c('biological pump', 'biological carbon pump','whale pump','mesopelagic migrant pump','seasonal lipid pump',
       'blue carbon','ocean carbon', 'biomass carbon', 'fish carbon', 'whale carbon',
       'remineralization', 'respiration', 'fecal pellets', 'marine snow')

###################################   BBNJ   ####################################################
dirct_bbnj <- "~/Dropbox/WMU/analysis/data/original/BBNJ"
setwd("~/Dropbox/WMU/analysis/data/original/BBNJ")

## keywords
bbnj <- keyword_directory(dirct_bbnj, 
                            keyword = kw,
                            surround_lines = 0, full_names = TRUE)
bbnj <- bbnj %>% mutate(kw_type='kw')

# print head & remove unnecessary column
bbnj$token_text <- NULL
bbnj$line_text <- (unlist(bbnj$line_text, use.names = FALSE)) # change class from list to vector

### total words
#finds files in current directory
pdfs <- list.files(pattern = "pdf",  full.names = TRUE)

# applies word count function and creates 'count' dataframe
count = cbind(pdfs,sapply(pdfs, readCleanCount))
count <- as.data.frame(apply(count, 2, unlist))
count <- count %>% rename(pdf_name = pdfs, total_words = V2)
count$total_words <- as.numeric(count$total_words)
count$pdf_name2 <- substring(count[,1],3)
count$pdf_name <- NULL
count <- count %>% rename(pdf_name = pdf_name2) 

# leftjoin count to result (dataframe with extracted keywords and sentences)
bbnj <- bbnj %>% left_join(count) %>% mutate(policy='bbnj')

# remove temp datasets
rm(count)



###################################   CBD   ####################################################
dirct_cbd <- "~/Dropbox/WMU/analysis/data/original/CBD"
setwd("~/Dropbox/WMU/analysis/data/original/CBD")

## keywords
cbd <- keyword_directory(dirct_cbd, 
                         keyword = kw,
                         surround_lines = 0, full_names = TRUE)
cbd <- cbd %>% mutate(kw_type='kw')

# print head & remove unnecessary column
cbd$token_text <- NULL
cbd$line_text <- (unlist(cbd$line_text, use.names = FALSE)) # change class from list to vector

### total CBD word count per PDF
#finds files in current directory
pdfs <- list.files(pattern = "pdf",  full.names = TRUE)

# applies word count function and creates 'count' dataframe
count = cbind(pdfs,sapply(pdfs, readCleanCount))
count <- as.data.frame(apply(count, 2, unlist))
count <- count %>% rename(pdf_name = pdfs, total_words = V2)
count$total_words <- as.numeric(count$total_words)
count$pdf_name2 <- substring(count[,1],3)
count$pdf_name <- NULL
count <- count %>% rename(pdf_name = pdf_name2) 

# leftjoin count to result (dataframe with extracted keywords and sentences)
cbd <- cbd %>% left_join(count) %>% mutate(policy='cbd')

# remove temp datasets
rm(count)  

###################################   CITES   ####################################################
dirct_cites <- "~/Dropbox/WMU/analysis/data/original/CITES"
setwd("~/Dropbox/WMU/analysis/data/original/CITES")

## keywords
cites <- keyword_directory(dirct_cites, 
                           keyword = kw,
                           surround_lines = 0, full_names = TRUE)
cites <- cites %>% mutate(kw_type='kw')

# print head & remove unnecessary column
cites$token_text <- NULL
cites$line_text <- (unlist(cites$line_text, use.names = FALSE)) # change class from list to vector

### total CITES word count per PDF
#finds files in current directory
pdfs <- list.files(pattern = "pdf",  full.names = TRUE)

# applies word count function and creates 'count' dataframe
count = cbind(pdfs,sapply(pdfs, readCleanCount))
count <- as.data.frame(apply(count, 2, unlist))
count <- count %>% rename(pdf_name = pdfs, total_words = V2)
count$total_words <- as.numeric(count$total_words)
count$pdf_name2 <- substring(count[,1],3)
count$pdf_name <- NULL
count <- count %>% rename(pdf_name = pdf_name2) 

# leftjoin count to result (dataframe with extracted keywords and sentences)
cites <- cites %>% left_join(count) %>% mutate(policy='cites')

# remove temp datasets
rm(count)  

###################################   CMS   ####################################################
dirct_cms <- "~/Dropbox/WMU/analysis/data/original/CMS"
setwd("~/Dropbox/WMU/analysis/data/original/CMS")

## keywords
cms <- keyword_directory(dirct_cms, 
                         keyword = kw,
                         surround_lines = 0, full_names = TRUE)
cms <- cms %>% mutate(kw_type='kw')

# print head & remove unnecessary column
cms$token_text <- NULL
cms$line_text <- (unlist(cms$line_text, use.names = FALSE)) # change class from list to vector

### total CMS word count per PDF
#finds files in current directory
pdfs <- list.files(pattern = "pdf",  full.names = TRUE)

# applies word count function and creates 'count' dataframe
count = cbind(pdfs,sapply(pdfs, readCleanCount))
count <- as.data.frame(apply(count, 2, unlist))
count <- count %>% rename(pdf_name = pdfs, total_words = V2)
count$total_words <- as.numeric(count$total_words)
count$pdf_name2 <- substring(count[,1],3)
count$pdf_name <- NULL
count <- count %>% rename(pdf_name = pdf_name2) 

# leftjoin count to result (dataframe with extracted keywords and sentences)
cms <- cms %>% left_join(count) %>% mutate(policy='cms')

# remove temp datasets
rm(count)  


###################################   ICRW   ####################################################
dirct_icrw <- "~/Dropbox/WMU/analysis/data/original/ICRW"
setwd("~/Dropbox/WMU/analysis/data/original/ICRW")

## keywords
icrw <- keyword_directory(dirct_icrw, 
                          keyword = kw,
                          surround_lines = 0, full_names = TRUE)
icrw <- icrw %>% mutate(kw_type='kw')

# print head & remove unnecessary column
icrw$token_text <- NULL
icrw$line_text <- (unlist(icrw$line_text, use.names = FALSE)) # change class from list to vector

### total_words  
#finds files in current directory
pdfs <- list.files(pattern = "pdf",  full.names = TRUE)

# applies word count function and creates 'count' dataframe
count = cbind(pdfs,sapply(pdfs, readCleanCount))
count <- as.data.frame(apply(count, 2, unlist))
count <- count %>% rename(pdf_name = pdfs, total_words = V2)
count$total_words <- as.numeric(count$total_words)
count$pdf_name2 <- substring(count[,1],3)
count$pdf_name <- NULL
count <- count %>% rename(pdf_name = pdf_name2) 

# leftjoin count to result (dataframe with extracted keywords and sentences)
icrw <- icrw %>% left_join(count) %>% mutate(policy='icrw')

# remove temp datasets
rm(count) 


###################################   LC   ####################################################
dirct_lc <- "~/Dropbox/WMU/analysis/data/original/LC"
setwd("~/Dropbox/WMU/analysis/data/original/LC")

## keywords
lc <- keyword_directory(dirct_lc, 
                        keyword = kw,
                        surround_lines = 0, full_names = TRUE)
lc <- lc %>% mutate(kw_type='kw')

# print head & remove unnecessary column
lc$token_text <- NULL
lc$line_text <- (unlist(lc$line_text, use.names = FALSE)) # change class from list to vector

### total lc word count per PDF
#finds files in current directory
pdfs <- list.files(pattern = "pdf",  full.names = TRUE)

# applies word count function and creates 'count' dataframe
count = cbind(pdfs,sapply(pdfs, readCleanCount))
count <- as.data.frame(apply(count, 2, unlist))
count <- count %>% rename(pdf_name = pdfs, total_words = V2)
count$total_words <- as.numeric(count$total_words)
count$pdf_name2 <- substring(count[,1],3)
count$pdf_name <- NULL
count <- count %>% rename(pdf_name = pdf_name2) 

# leftjoin count to result (dataframe with extracted keywords and sentences)
lc <- lc %>% left_join(count) %>% mutate(policy='lc')

# remove temp datasets
rm(count)  

###################################   PARTXI   ####################################################
dirct_partxi <- "~/Dropbox/WMU/analysis/data/original/PARTXI"
setwd("~/Dropbox/WMU/analysis/data/original/PARTXI")

## keywords
partxi <- keyword_directory(dirct_partxi, 
                            keyword = kw,
                            surround_lines = 0, full_names = TRUE)
partxi <- partxi %>% mutate(kw_type='kw')

# print head & remove unnecessary column
partxi$token_text <- NULL
partxi$line_text <- (unlist(partxi$line_text, use.names = FALSE)) # change class from list to vector

### total partxi word count per PDF
#finds files in current directory
pdfs <- list.files(pattern = "pdf",  full.names = TRUE)

# applies word count function and creates 'count' dataframe
count = cbind(pdfs,sapply(pdfs, readCleanCount))
count <- as.data.frame(apply(count, 2, unlist))
count <- count %>% rename(pdf_name = pdfs, total_words = V2)
count$total_words <- as.numeric(count$total_words)
count$pdf_name2 <- substring(count[,1],3)
count$pdf_name <- NULL
count <- count %>% rename(pdf_name = pdf_name2) 

# leftjoin count to result (dataframe with extracted keywords and sentences)
partxi <- partxi %>% left_join(count) %>% mutate(policy='partxi')

# remove temp datasets
rm(count)  

###################################   UNCLOS   ####################################################
dirct_unclos <- "~/Dropbox/WMU/analysis/data/original/UNCLOS"
setwd("~/Dropbox/WMU/analysis/data/original/UNCLOS")

## keywords
unclos <- keyword_directory(dirct_unclos, 
                            keyword = kw,
                            surround_lines = 0, full_names = TRUE)
unclos <- unclos %>% mutate(kw_type='kw')

# print head & remove unnecessary column
unclos$token_text <- NULL
unclos$line_text <- (unlist(unclos$line_text, use.names = FALSE)) # change class from list to vector

### total unclos word count per PDF
#finds files in current directory
pdfs <- list.files(pattern = "pdf",  full.names = TRUE)

# applies word count function and creates 'count' dataframe
count = cbind(pdfs,sapply(pdfs, readCleanCount))
count <- as.data.frame(apply(count, 2, unlist))
count <- count %>% rename(pdf_name = pdfs, total_words = V2)
count$total_words <- as.numeric(count$total_words)
count$pdf_name2 <- substring(count[,1],3)
count$pdf_name <- NULL
count <- count %>% rename(pdf_name = pdf_name2) 

# leftjoin count to result (dataframe with extracted keywords and sentences)
unclos <- unclos %>% left_join(count) %>% mutate(policy='unclos')

# remove temp datasets
rm(count)  


###################################   UNFCCC   ####################################################
dirct_unfccc <- "~/Dropbox/WMU/analysis/data/original/UNFCCC"
setwd("~/Dropbox/WMU/analysis/data/original/UNFCCC")

## keywords
unfccc <- keyword_directory(dirct_unfccc, 
                            keyword = kw,
                            surround_lines = 0, full_names = TRUE)
unfccc <- unfccc %>% mutate(kw_type='kw')

# print head & remove unnecessary column
unfccc$token_text <- NULL
unfccc$line_text <- (unlist(unfccc$line_text, use.names = FALSE)) # change class from list to vector

### total unfccc word count per PDF
#finds files in current directory
pdfs <- list.files(pattern = "pdf",  full.names = TRUE)

# applies word count function and creates 'count' dataframe
count = cbind(pdfs,sapply(pdfs, readCleanCount))
count <- as.data.frame(apply(count, 2, unlist))
count <- count %>% rename(pdf_name = pdfs, total_words = V2)
count$total_words <- as.numeric(count$total_words)
count$pdf_name2 <- substring(count[,1],3)
count$pdf_name <- NULL
count <- count %>% rename(pdf_name = pdf_name2) 

# leftjoin count to result (dataframe with extracted keywords and sentences)
unfccc <- unfccc %>% left_join(count) %>% mutate(policy='unfccc')

# remove temp datasets
rm(count)  

###################################   UNFSA   ####################################################
dirct_unfsa <- "~/Dropbox/WMU/analysis/data/original/UNFSA/ALL"
setwd("~/Dropbox/WMU/analysis/data/original/UNFSA/ALL")

## keywords
unfsa <- keyword_directory(dirct_unfsa, 
                           keyword = kw,
                           surround_lines = 0, full_names = TRUE)
unfsa <- unfsa %>% mutate(kw_type='kw')

# print head & remove unnecessary column
unfsa$token_text <- NULL
unfsa$line_text <- (unlist(unfsa$line_text, use.names = FALSE)) # change class from list to vector

### total unfsa word count per PDF
#finds files in current directory
pdfs <- list.files(pattern = "pdf",  full.names = TRUE)

# applies word count function and creates 'count' dataframe
count = cbind(pdfs,sapply(pdfs, readCleanCount))
count <- as.data.frame(apply(count, 2, unlist))
count <- count %>% rename(pdf_name = pdfs, total_words = V2)
count$total_words <- as.numeric(count$total_words)
count$pdf_name2 <- substring(count[,1],3)
count$pdf_name <- NULL
count <- count %>% rename(pdf_name = pdf_name2) 

# leftjoin count to result (dataframe with extracted keywords and sentences)
unfsa <- unfsa %>% left_join(count) %>% mutate(policy='unfsa')

# remove temp datasets
rm(count)  

############################################   MERGE   #######################################################

# merge original keyword search
result <- bbnj %>% full_join(cbd) %>% full_join(cites) %>% full_join(cms) %>% full_join(icrw) %>% full_join(lc) %>% full_join(partxi) %>% full_join(unclos) %>% full_join(unfccc) %>% full_join(unfsa)

############################################   CONDITIONS   #######################################################
# conditioning keywords aqua/terra
result <- result %>% mutate(marine = ifelse(line_text %in% c(grep("marine|ocean| sea|blue |pelagic|benthic|aqua|fish", line_text,ignore.case=TRUE,value=TRUE)), "marine", 
                                            ifelse(policy %in% c(grep("bbnj|icrw|lc|partxi|unclos|unfsa", policy,ignore.case=TRUE,value=TRUE)),"marine",NA))) 
result <- result %>% mutate(land = ifelse(line_text %in% c(grep(" land|terrestrial|urban|rural|forest|crop|farm|soil|desert|arid", line_text,ignore.case=TRUE,value=TRUE)), "land",NA)) 

# remove rows that do are only 'land' classified
result$keep <- ifelse(grepl("land",result$land) & is.na(result$marine),NA,'keep')
result <- completeFun(result, "keep")

# remove conditional columns
result <- result  %>% select(-marine,-land,-keep)

#### integrate results
result <- result %>% full_join(match)
result$ID <- NULL

# write and read data created above
# write.csv(result,'~/Dropbox/WMU/analysis/data/analysis/ms_iff_all_step1.csv')



