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

## keywords 
kw = c('species','marine species','organism','fauna','living resource','animal','creature','wildlife',
       'target species','fishing', 'fish stock', 'overfishing',
       'anchovies', 'coalfish',' cod ', 'crab', 'cuttlefish', 'flatfish','haddock','halibut','herring','homarus','lobster','mackerel','mussel','octopus','plaice','rock lobster','salmon','salmonidae','sardines','scallop','seabass','shark','shrimp',' sole ','swordfish','tuna','zooplankton','macroalgae','seaweed','kelp','whale','dolphin','marine mammal',
       'Convention on Biological Diversity', 'CBD', 'Aichi Biodiversity Targets', 'Post-2020 Biodiversity Targets','CITES','Convention on International Trade in Endangered Species of Wild Fauna and Flora', 'CMS','Convention on the Conservation of Migratory Species of Wild Animals','UNEP','United Nations Environment Program','IWC','International Whaling Commission', 'ICRW','International Convention for the Regulation of Whaling', 'UNFSA','United Nations Fish Stock Agreement','FAO','Food and Agriculture Organization of the United Nations', 'Regional Fisheries Management Organizations', 'RFMO') # conditional keywords


###################################################################################################################
################################################   CREATE DATA   ##################################################
###################################################################################################################



result <- read.csv(file.path(dirct, 'analysis/ms_sff_all_step2.csv'), as.is=T) %>%
  select(-X)


###################################################################################################################
#################################################   ANALYSIS   ####################################################
###################################################################################################################

### count keywords  
# counts the number of all keyword occurrence per PDF 
x = result %>%
  group_by(pdf_name, keyword) %>%
  tally() %>%
  rename(individual_count_keywords = n) # individual keywords
var <- aggregate( individual_count_keywords ~ pdf_name, x, sum ) %>%
  rename(count_keywords = individual_count_keywords) %>% left_join(x)

### count categories
# counts the number of occurrences of categories per PDF 
y <- within(var, {count = ave(keyword, pdf_name, FUN = function(var) length(unique(var)))}) %>%
  rename(count_categories = count)
y$count_categories <- as.numeric(y$count_categories)

# clean and join
y <- y %>% distinct() 
var <- var %>% left_join(y)  

### leftjoin list to results (dataframe with extracted keywords and sentences)
result = result %>% 
  left_join(var) 

# order columns in result
col_order <- c('policy','pdf_name','keyword','kw_type','page_num','line_num','line_text','individual_count_keywords','count_keywords','count_categories','total_words')
result <- result[, col_order]

### calculate focus factor
result <- transform(result, sff = 100000* (count_keywords/total_words) * (1+ (count_categories/max(result$count_categories)))) 

### save files
# save result file
result1 <- result[,-c(8:12)] 
# write.csv(result1,'~/Dropbox/WMU/analysis/data/analysis/ms_sff_all_text.csv')

# save numeric file
result2 <- result[,-c(5:7)] 
# write.csv(result2,'~/Dropbox/WMU/analysis/data/analysis/ms_sff_all.csv')
