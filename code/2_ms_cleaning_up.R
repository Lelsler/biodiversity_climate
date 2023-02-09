# this file plots depth of species for different policies
# Maartje Oostidjk, Laura Elsler, WMU

# clear workspace
rm(list = ls())
graphics.off()

# libraries
require(tidyverse)
require(dplyr)

# directory
dirct <- "~/Dropbox/WMU/analysis/data/analysis"
setwd("~/Dropbox/WMU/analysis/data/analysis")

# function
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# read text files
cff <- read.csv(file.path(dirct, 'ms_cff_all_step1.csv'), as.is=T) %>%
  select(-X)%>%
  select(policy, pdf_name, keyword, kw_type, page_num, line_num, line_text, total_words)
sff <- read.csv(file.path(dirct, 'ms_sff_all_step1.csv'), as.is=T) %>%
  select(-X)%>%
  select(policy, pdf_name, keyword, kw_type, page_num, line_num, line_text, total_words)
  

#not all new columns were duplicates as the column spreads also if climate and carbon are mentioned in the same
#paragraph so therefore probably filter per problematic keyword and solve subsequentially

###################################################################################################################
###################################################   CLIMATE   ###################################################
###################################################################################################################

# climate
df = dplyr::filter(cff, grepl('climate',keyword))

dups <- df %>% #spread keyword column for duplicates
  group_by(pdf_name,page_num,line_num,line_text,kw_type,policy,total_words) %>%
  mutate(kw_id = paste0('keyword', row_number())) %>%
  spread(kw_id, keyword)

non_dups_climate = dups%>%
  filter(is.na(keyword2) & is.na(keyword3))%>%
  select(-keyword2, -keyword3)%>%
  rename(keyword =keyword1)#safe to assume climate is THE keyword

one_dups_climate = dups%>%
  filter(!is.na(keyword2) & is.na(keyword3)) 

unique(one_dups_climate$keyword2) #either climate change or climate mitigation
  
one_dups_climate= one_dups_climate%>%
  select(-keyword1, -keyword3)%>%
  rename(keyword = keyword2) #safe to assume the 2 word combination is the set of keywords

two_dups_climate = dups%>%
  filter(!is.na(keyword2) & !is.na(keyword3))%>%
  select(-keyword1, -keyword2)%>%
  rename(keyword = keyword3)#only mitigating climate change here

all_climate = bind_rows(non_dups_climate, one_dups_climate, two_dups_climate)
rm(df, dups, non_dups_climate, one_dups_climate, two_dups_climate)


# carbon
df2 = dplyr::filter(cff, grepl('carbon',keyword))

dups2 <- df2 %>% #spread keyword column for duplicates
  group_by(pdf_name,page_num,line_num,line_text,kw_type,policy,total_words) %>%
  mutate(kw_id = paste0('keyword', row_number())) %>%
  spread(kw_id, keyword)

non_dups_carbon = dups2%>%
  filter(is.na(keyword2) & is.na(keyword3) & is.na(keyword4))%>%
  select(-keyword2, -keyword3, -keyword4)%>%
  rename(keyword =keyword1)#safe to assume carbon is THE keyword

one_dups_carbon= dups2%>%
  filter(!is.na(keyword2) & is.na(keyword3) & is.na(keyword4))%>% 
  select(-keyword1, -keyword3, -keyword4)%>%
  rename(keyword = keyword2) #safe to assume the 2 word combination is the set of keywords

two_dups_carbon = dups2%>%
  filter(!is.na(keyword2) & !is.na(keyword3) & is.na(keyword4))%>%
  select(-keyword1, -keyword4)%>% #%>%#these are not actual duplicates so should be gathered again.
  gather(keywordnr, keyword3, -policy,-pdf_name,-page_num, -kw_type, -line_num, -line_text, -total_words)%>%
  select(-keywordnr)%>%
  rename(keyword = keyword3) #these last few are not actual doubles so gather again

three_dups_carbon = dups2%>%
  filter(!is.na(keyword2) & !is.na(keyword3) & !is.na(keyword4))%>%
  select(-keyword1)%>% #%>%#these are not actual duplicates so should be gathered again.
  gather(keywordnr,keyword3, -policy,-pdf_name,-page_num, -kw_type, -line_num, -line_text, -total_words)%>%
  select(-keywordnr)%>%
  rename(keyword = keyword3) #these last few are not actual doubles so gather again

all_carbon = bind_rows(non_dups_carbon, one_dups_carbon, two_dups_carbon)
rm(df2, dups2, non_dups_carbon, one_dups_carbon, two_dups_carbon)

#all single kw
non_cff = dplyr::filter(cff, !grepl('climate|carbon',keyword))

#this document is all the stuff for the cff
all_cff = bind_rows(non_cff, all_climate, all_carbon)
rm(non_cff, all_climate, all_carbon)

###############################################   SELF MENTIONS   ##################################################
sub1 <- all_cff %>% filter(policy=='unfccc') %>% mutate(self = ifelse(keyword %in% c(grep('United Nations Convention Framework for Climate Change|UNFCCC|Paris Agreement|Kyoto Protocol',
                                                                       keyword,ignore.case=TRUE,value=TRUE)),'self',NA))
sub2 <- all_cff %>% filter(policy=='lc') %>% mutate(self = ifelse(keyword %in% c(grep('London Protocol',
                                                                                          keyword,ignore.case=TRUE,value=TRUE)),'self',NA))


all_cff <- full_join(sub2, sub1) %>% full_join(., all_cff) 
all_cff <- all_cff %>% filter(is.na(self)) %>% select(-self) # remove if column contains self mentions (other than NA)

#################################################   CONDITIONS   ##################################################

# conditioning keywords
all_cff <- all_cff %>% mutate(marine = ifelse(line_text %in% c(grep("marine|ocean| sea|blue |pelagic|benthic|aqua|fish", line_text,ignore.case=TRUE,value=TRUE)), "marine", 
                                            ifelse(policy %in% c(grep("bbnj|fao|icrw|lc|partxi|unclos|unfsa", policy,ignore.case=TRUE,value=TRUE)),"marine",NA))) 
all_cff <- all_cff %>% mutate(land = ifelse(line_text %in% c(grep(" land|terrestrial|urban|rural|forest|crop|farm|soil|desert|arid", line_text,ignore.case=TRUE,value=TRUE)), "land",NA)) 

# remove rows that do are only 'land' classified
all_cff$keep <- ifelse(grepl("land",all_cff$land) & is.na(all_cff$marine),NA,'keep')
all_cff <- completeFun(all_cff, "keep")
all_cff <- all_cff %>%
  select(-marine,-land,-keep)
  
#####################################################   SAVE   ####################################################
# write.csv(all_cff, "ms_cff_all_step2.csv")


###################################################################################################################
####################################################   SPECIES   ##################################################
###################################################################################################################

# species
df3 = dplyr::filter(sff, grepl('species',keyword))

dups3 <- df3 %>% #spread keyword column for duplicates
  group_by(pdf_name,page_num,line_num,line_text,kw_type,policy) %>%
  mutate(kw_id = paste0('keyword', row_number())) %>%
  spread(kw_id, keyword)

non_dups_species = dups3%>%
  filter(is.na(keyword2) & is.na(keyword3))%>%
  select(-keyword2, -keyword3)%>%
  rename(keyword =keyword1)#safe to assume species is THE keyword

one_dups_species= dups3%>%
  filter(!is.na(keyword2) & is.na(keyword3))%>% 
  select(-keyword1, -keyword3)%>%
  rename(keyword = keyword2) #safe to assume the 2 word combination is the set of keywords

two_dups_species = dups3%>%
  filter(!is.na(keyword2) & !is.na(keyword3))%>%
  select(-keyword1)%>% #%>%#these are not actual duplicates so should be gathered again.
  gather(keyword2, keyword3, -policy,-pdf_name,-page_num, -kw_type, -line_num, -line_text, -total_words)%>%
  select(-keyword2)%>%
  rename(keyword = keyword3) #these last few are not actual doubles so gather again

all_spp = bind_rows(non_dups_species, one_dups_species, two_dups_species)
rm(df3, dups3, non_dups_species, one_dups_species, two_dups_species)



# biodiversity
df4 = dplyr::filter(sff, grepl('biodiversity',keyword))

dups4 <- df4 %>% #spread keyword column for duplicates
  group_by(pdf_name,page_num,line_num,line_text,kw_type,policy) %>%
  mutate(kw_id = paste0('keyword', row_number())) %>%
  spread(kw_id, keyword)

non_dups_bio = dups4%>%
  filter(is.na(keyword2)) %>%
  select(-keyword2)%>%
  rename(keyword =keyword1)#safe to assume biodiversity is THE keyword

one_dups_bio= dups4%>%
  filter(!is.na(keyword2))%>% 
  select(-keyword1)%>%
  rename(keyword = keyword2) #safe to assume the 2 word combination is the set of keywords

all_bio = bind_rows(non_dups_bio, one_dups_bio)
rm(df4, dups4, non_dups_bio, one_dups_bio)

non_sff = dplyr::filter(sff, !grepl('species|biodiversity',keyword))

#this document is all the stuff for the cff
all_sff = bind_rows(non_sff, all_spp, all_bio)
rm(non_sff, all_spp, all_bio)


###############################################   SELF MENTIONS   ##################################################
sub1 <- all_sff %>% filter(policy=='cbd') %>% mutate(self = ifelse(keyword %in% c(grep('Convention on Biological Diversity|CBD|Aichi Biodiversity Targets|IPBES|intergovernmental science-policy platform on biodiversity and ecosystem services|Post-2020 Biodiversity Targets',
                                                                                          keyword,ignore.case=TRUE,value=TRUE)),'self',NA))
sub2 <- all_sff %>% filter(policy=='cites') %>% mutate(self = ifelse(keyword %in% c(grep('CITES|Convention on International Trade in Endangered Species of Wild Fauna and Flora',
                                                                                      keyword,ignore.case=TRUE,value=TRUE)),'self',NA))
sub3 <- all_sff %>% filter(policy=='cms') %>% mutate(self = ifelse(keyword %in% c(grep('CMS|Convention on the Conservation of Migratory Species of Wild Animals|UNEP|United Nations Environment Program',
                                                                                      keyword,ignore.case=TRUE,value=TRUE)),'self',NA))
sub4 <- all_sff %>% filter(policy=='icrw') %>% mutate(self = ifelse(keyword %in% c(grep('IWC|International Whaling Commission|ICRW|International Convention for the Regulation of Whaling',
                                                                                      keyword,ignore.case=TRUE,value=TRUE)),'self',NA))
sub5 <- all_sff %>% filter(policy=='unfsa') %>% mutate(self = ifelse(keyword %in% c(grep('UNFSA|United Nations Fish Stock Agreement|Regional Fisheries Management Organizations|RFMO',
                                                                                      keyword,ignore.case=TRUE,value=TRUE)),'self',NA))
sub6 <- all_sff %>% filter(policy=='bbnj') %>% mutate(self = ifelse(keyword %in% c(grep('BBNJ agreement',
                                                                                       keyword,ignore.case=TRUE,value=TRUE)),'self',NA))

sel_sff = filter(all_sff, grepl('lc|partxi|unclos|unfccc',policy))
sel_sff <- sel_sff %>% mutate(self = NA)

all_sff <- full_join(sub1, sub2) %>% 
  full_join(., sub3) %>% 
  full_join(., sub4) %>% 
  full_join(., sub5) %>% 
  full_join(., sub6) %>%   
  full_join(., sel_sff) 

all_sff <- all_sff %>% filter(is.na(self)) %>% select(-self) # remove if column contains self mentions (other than NA)


#################################################   CONDITIONS   ##################################################

# conditioning keywords
all_sff <- all_sff %>% mutate(marine = ifelse(line_text %in% c(grep("marine|ocean| sea|blue |pelagic|benthic|aqua|fish", line_text,ignore.case=TRUE,value=TRUE)), "marine", 
                                            ifelse(policy %in% c(grep("bbnj|fao|icrw|lc|partxi|unclos|unfsa", policy,ignore.case=TRUE,value=TRUE)),"marine",NA))) 
all_sff <- all_sff %>% mutate(land = ifelse(line_text %in% c(grep(" land|terrestrial|urban|rural|forest|crop|farm|soil|desert|arid", line_text,ignore.case=TRUE,value=TRUE)), "land",NA)) 

# remove rows that do are only 'land' classified
all_sff$keep <- ifelse(grepl("land",all_sff$land) & is.na(all_sff$marine),NA,'keep')
all_sff <- completeFun(all_sff, "keep")
all_sff <- all_sff %>%
  select(-marine,-land,-keep)

#####################################################   SAVE   ####################################################
# write.csv(all_sff, "ms_sff_all_step2.csv")

