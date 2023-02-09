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


###################################################################################################################
################################################   READ DATA   ####################################################
###################################################################################################################

## keywords 
kw = c('biological pump', 'biological carbon pump','whale pump','mesopelagic migrant pump','seasonal lipid pump',
       'blue carbon','ocean carbon', 'biomass carbon', 'fish carbon', 'whale carbon',
       'remineralization', 'respiration', 'fecal pellets', 'marine snow')


result <- read.csv(file.path(dirct, 'analysis/ms_iff_all_step1.csv'), as.is=T) %>% select(-X)

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
y <- y %>% select(-4) %>% distinct() 
var <- var %>% left_join(y) # rm individual_count_keywords dont need them

### leftjoin list to results (dataframe with extracted keywords and sentences)
result = result %>% 
  left_join(var) %>% distinct()

# order columns in result
col_order <- c('policy','pdf_name','keyword','kw_type','page_num','line_num','line_text','individual_count_keywords','count_keywords','count_categories','total_words')
result <- result[, col_order]

### calculate focus factor
result <- transform(result, iff = 100000* (count_keywords/total_words) * (1+ (count_categories/max(result$count_categories)))) 

### save files
# save result file
result1 <- result[,-c(8:12)] 
# write.csv(result1,'~/Dropbox/WMU/analysis/data/analysis/ms_iff_all_text.csv')

# save numeric file
result2 <- result[,-c(5:7)] 
# write.csv(result2,'~/Dropbox/WMU/analysis/data/analysis/ms_iff_all.csv')
