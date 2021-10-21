# this file calculates the climate factor for policy files
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

# functions
readCleanCount<-function(pdf){    # uses pdftools to convert pdfs to plain-text, replaces line breaks with spaces and then counts the words, ignoring non-word symbols
  txt<-pdf_text(pdf)
  txt<-paste(gsub(txt,pattern="\r\n",replace=" "),collapse=" ")
  count<-sapply(gregexpr("[[:alpha:]]+", txt), function(x) sum(x > 0))
  return(count)
}


## keywords 
kw = c('climate','climate change','climate mitigat','mitigating climate change',
       ' carbon','CO2','carbon pump','carbon flow', 'carbon flux','carbon seques','carbon cycl','carbon captur','carbon export','carbon stor','carbon uptake','carbon fix','carbon burial','carbon transformation','carbon transfer','carbon immobilization','carbon deposit','carbon stock','carbon sink',
       'United Nations Convention Framework for Climate Change','UNFCCC','Paris Agreement', 'Kyoto Protocol', 'London Protocol') 

###################################################################################################################
################################################   READ DATA   ####################################################
###################################################################################################################

result <- read.csv(file.path(dirct, 'analysis/ms_cff_all_step2.csv'), as.is=T) %>%
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
result <- transform(result, cff = 100000* (count_keywords/total_words) * (1+ (count_categories/max(result$count_categories)))) 

### save files
# text file
result1 <- result[,-c(8:12)] 
# write.csv(result1,'~/Dropbox/WMU/analysis/data/analysis/ms_cff_all_text.csv')

# numeric file
result2 <- result[,-c(5:7)] 
# write.csv(result2,'~/Dropbox/WMU/analysis/data/analysis/ms_cff_all.csv')

