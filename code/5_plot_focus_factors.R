# this file creates plots the focus factors of policy files
# Laura Gabriele Elsler, Maartje Oostdijk, WMU

### set up
# clear workspace
rm(list = ls())
graphics.off()

# directory
dirct <- "~/Dropbox/WMU/analysis/data/analysis"
setwd("~/Dropbox/WMU/analysis")

# libraries
library(tidyverse)
require(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(data.table)


# read files
df_cff <- read.csv(file.path(dirct, 'ms_cff_all.csv'), as.is=T) %>% select(-X) %>% mutate(cat='climate')
df_sff <- read.csv(file.path(dirct, 'ms_sff_all.csv'), as.is=T) %>% select(-X) %>% mutate(cat='species')
df_iff <- read.csv(file.path(dirct, 'ms_iff_all.csv'), as.is=T) %>% select(-X) %>% mutate(cat='integration')

# capitalize policies
df_cff[,1] = toupper(df_cff[,1])
df_sff[,1] = toupper(df_sff[,1])
df_iff[,1] = toupper(df_iff[,1])

# read text files
t_cff <- read.csv(file.path(dirct, 'ms_cff_all_text.csv'), as.is=T) 
t_sff <- read.csv(file.path(dirct, 'ms_sff_all_text.csv'), as.is=T) 
t_iff <- read.csv(file.path(dirct, 'ms_iff_all_text.csv'), as.is=T) 

# change names
names(df_cff)[9] <- "ff"
names(df_sff)[9] <- "ff"
names(df_iff)[9] <- "ff"

# clean global data
df_cff$policy[which(df_cff$policy == "LC")] <- "LC/LP"
df_cff$policy[which(df_cff$policy == "FAO")] <- "UNFSA"
df_sff$policy[which(df_sff$policy == "LC")] <- "LC/LP"
df_sff$policy[which(df_sff$policy == "FAO")] <- "UNFSA"
df_iff$policy[which(df_iff$policy == "LC")] <- "LC/LP"
df_iff$policy[which(df_iff$policy == "FAO")] <- "UNFSA"

# merge files
ff <- rbind(df_sff,df_cff,df_iff) # no removed values
df_ff <- rbind(df_sff,df_cff,df_iff)  %>% distinct() 
t_all <- rbind(t_sff,t_cff,t_iff)
rm(t_sff,t_cff,t_iff)

###################################################################################################################
#############################################   KW FREQUENCY   ####################################################
###################################################################################################################

##################################################   PREP DATA   ##################################################

# create dataset for policy, number of PDF documents
pdf <- data.frame (policy  = c('BBNJ','CBD','CITES','CMS','ICRW','LC/LP','PARTXI','UNCLOS','UNFCCC','UNFSA'),
                   pdfs = c(43,513,101,252,57,93,88,66,207,1293))

# read keyword sets file
keywords = read.csv(file.path(dirct, 'keyword_list_types.csv')) 

# pull in data, match pdf and keyword sets file 
ff_freq = df_ff  %>% left_join(pdf) %>% 
  left_join(keywords,by = c("keyword", "cat")) 

# match unmatched keywords to OBIS keyword set
unknown =ff_freq%>%
  filter(is.na(kw_set))
ff_freq$kw_set[is.na(ff_freq$kw_set)] = "OBIS species"
ff_freq= na.omit(ff_freq)


#################################################   STATS FOR TEXT   ##############################################

### PDFS MENTIONING KEYWORDS PER CATEGORY ###
unique(df_sff$pdf_name)
unique(df_cff$pdf_name)
unique(df_iff$pdf_name)

### KW PER PDF IN CATEGORIES ###
a <-ff %>%
  group_by(cat)%>%
  tally()%>%
  mutate(per_pdf=(n/sum(pdf$pdfs)))

### KW PER PDF IN POLICIES ###
b <-ff%>%
  group_by(policy)%>%
  tally()%>%
  left_join(pdf)%>%
  mutate(per_pdf=(n/pdfs))

### KW PER PDF IN POLICIES AND SET ###
c <-ff%>%
  group_by(policy,cat)%>%
  tally()%>%
  left_join(pdf)%>%
  mutate(per_pdf=(n/pdfs))

### KW FREQ PER PDF ###
d <- ff_freq%>%
  group_by(policy,pdf_name,cat)%>%
  summarise(total_kw = sum(individual_count_keywords)) # step 1: filter by CBD and climate and order max-min; step 2: filter by bbnj and order max-min

# identify the keywords mentioned in the top ranking pdf's
view(ff_freq) 
# cbd: MUS-nbsap-v2-en-2020.pdf
# bbnj textual proposals: textual_proposals_compilation_-_15_april_2020.pdf
# bbnj draft: a_conf.232_2020_3_E.pdf

# show rows of the pdf with highest kw frequency
e <- ff_freq[grepl("MUS-nbsap-v2-en-2020.pdf", ff_freq$pdf_name),]

#################################################   PREP PLOT DATA   ###########################################

# kw counted per set and policy
ff_sums=ff_freq %>% 
  group_by(policy, kw_set, cat, pdfs) %>%
  summarise(total_keywords = sum(individual_count_keywords)) %>%
  mutate(kw_per_pdf=total_keywords/pdfs) %>% 
  mutate(display= ifelse(kw_per_pdf > 1, round(kw_per_pdf,0),
                         ifelse(kw_per_pdf<1 & kw_per_pdf>0,round(kw_per_pdf,1)))) %>%
  mutate(plot=kw_per_pdf+2) # increases bar size when log transforming the values for plotting


ff_sums <- ff_sums %>% mutate(col = ifelse(cat == 'species', 'A. Biodiversity', 
                                           ifelse(cat == 'climate', 'B. Climate',  
                                                  ifelse(cat == 'integration', 'C. Ocean carbon', NA)))) 
ff_sums$cat <- NULL
ff_sums <- ff_sums %>% rename(cat=col)
ff_sums$display <- ifelse(ff_sums$display>0.9, round(ff_sums$display,0), ff_sums$display) 

#################################################   PLOT KW FREQUENCY   ###########################################

bar <- ggplot(ff_sums, aes(x=kw_set, y=plot, fill=policy, label=display)) +
  geom_bar(stat = "identity") + theme_classic()+
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.text.x = element_text(angle = 90)) +
  ylab('Keywords per policy document') + xlab('Keyword set') + labs(fill = 'Agreement')+
  facet_wrap(~cat,scales = "free")+scale_y_log10()+theme(text = element_text(size = 16))

ggplotly(bar, width=1000, height=800) 
# ggsave(filename="figures/keywordfig.png", width = 300, height = 200, units = "mm", dpi=500)

rm(a,b,c,d,e,ff_freq,ff_sums,bar,unknown,pdf,keywords)





###################################################################################################################
##############################################   FOCUS FACTORS   ##################################################
###################################################################################################################

##################################################   PREP DATA   ##################################################


# calculate average value of cff/sff/iff
mean_cff <- aggregate(df_cff$ff, list(df_cff$policy), mean, na.rm = TRUE)
mean_sff <- aggregate(df_sff$ff, list(df_sff$policy), mean, na.rm = TRUE)
mean_iff <- aggregate(df_iff$ff, list(df_iff$policy), mean, na.rm = TRUE) 
miss <- data.frame(Group.1=c('CITES'), x=c(0))
mean_iff <- rbind(mean_iff, miss)



##############################################   DEV
# why are there NAs in ff value? 
x1<- df_cff%>% filter(is.na(ff)) # remove if column is NA
x2 <- df_iff%>% filter(is.na(ff)) # remove if column is NA
x3 <- df_sff%>% filter(is.na(ff)) # remove if column is NA
##############################################   DEV



#################################################   STATS FOR TEXT   ################################################

# SFF
no <- df_sff %>% select(policy,pdf_name,keyword,individual_count_keywords,ff) %>%  filter(!is.na(ff)) 

# to find policies with high sff 
sffrank <- no %>% 
  group_by(policy) %>%
  summarize(mean = mean(ff)) 
view(sffrank) # result,  used

# find PDF with max value sff 
view(no) # sort by ff
max(no$ff)


# CFF
no <- df_cff %>% select(policy,pdf_name,keyword,individual_count_keywords,ff) %>%  filter(!is.na(ff)) 

# to find policies with high sff 
sffrank <- no %>% 
  group_by(policy) %>%
  summarize(mean = mean(ff)) 
view(sffrank) # result,  used

# find PDF with max value sff 
view(no) # sort by ff
max(no$ff)

# IFF
no <- df_iff %>% select(policy,pdf_name,keyword,individual_count_keywords,ff) %>%  filter(!is.na(ff)) 

# to find policies with high sff 
sffrank <- no %>% 
  group_by(policy) %>%
  summarize(mean = mean(ff)) 
view(sffrank) # result,  used

# find PDF with max value sff 
view(no) # sort by ff
max(no$ff)


# total number of keywords per policy 
no <- df_iff %>% select(policy,pdf_name,keyword,individual_count_keywords) %>% 
  distinct()# to find pdfs with high counts run only this part
no_kw <- no %>% 
  group_by(policy) %>% 
  summarize(sum = sum(individual_count_keywords)) 

# unique keywords per policy 
uni_kw <- aggregate( keyword~policy,df_iff, function(x) length(unique(x)))
uni_kw_example <- df_iff %>% filter(policy == 'CBD') # just to have a look
unique(uni_kw_example$keyword)

# pdfs with high no of hits
no_pdf <- no %>% 
  group_by(pdf_name) %>% 
  summarize(sum = sum(individual_count_keywords)) 

# pull out text examples from here
df_iff_text <- read.csv(file.path(dirct, 'ms_iff_all_text.csv'), as.is=T) 

# keep bbnj rows 
d <- df_ff[grepl("BBNJ", df_ff$policy),]
e <- d %>% 
  group_by(pdf_name) %>% 
  summarize(sum = sum(individual_count_keywords)) 

###################################################   PLOT FF   ##################################################

### bar plots FF

# CFF
bar1 <- ggplot(mean_cff, aes(Group.1, x, fill = Group.1,label=(round(x, digits = 0)))) + 
  geom_bar(stat="identity", position = "dodge", show.legend=FALSE) + theme_classic() +
  theme(axis.text.x = element_text(size=14, angle=45, vjust=0.5),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14, angle=0),
        axis.title.y = element_text(face="bold", color="black", 
                                    size=14)) +
  ylab('Climate focus factor') + xlab('Policy')+
  geom_text(size = 3,position = position_stack(vjust = 0.5))
ggplotly(bar1, width=600, height=400) 
# ggsave('~/Dropbox/WMU/analysis/figures/ms_cff_bar.png',bar1, width = 150, height = 120, units = "mm", dpi=500)

# SFF
bar2 <- ggplot(mean_sff, aes(Group.1, x, fill = Group.1,label=(round(x, digits = 0)))) + 
  geom_bar(stat="identity", position = "dodge", show.legend=FALSE) + theme_classic() +
  theme(axis.text.x = element_text(size=14, angle=45, vjust=0.5),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14, angle=0),
        axis.title.y = element_text(face="bold", color="black", 
                                    size=14)) +
  ylab('Biodiversity focus factor') + xlab('Policy')+
  geom_text(size = 3,position = position_stack(vjust = 0.5))
ggplotly(bar2, width=600, height=400) 
# ggsave('~/Dropbox/WMU/analysis/figures/ms_sff_bar.png',bar2, width = 150, height = 120, units = "mm", dpi=500)

# IFF
bar3 <- ggplot(mean_iff, aes(Group.1, x, fill = Group.1,label=(round(x, digits = 0)))) + 
  geom_bar(stat="identity", position = "dodge", show.legend=FALSE) + theme_classic() +
  theme(axis.text.x = element_text(size=14, angle=45, vjust=0.5),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14, angle=0),
        axis.title.y = element_text(face="bold", color="black", 
                                    size=14)) +
  ylab('Ocean carbon focus factor') + xlab('Policy')+
  geom_text(size = 3,position = position_stack(vjust = 0.5))
ggplotly(bar3, width=600, height=400) 
# ggsave('~/Dropbox/WMU/analysis/figures/ms_iff_bar.png',bar3, width = 150, height = 120, units = "mm", dpi=500)


################################################   DEV

df_iff_nf <- read.csv(file.path(dirct, 'ms_iff_all_no_filter.csv'), as.is=T) 
df_iff_nf[,2] = toupper(df_iff_nf[,2])
mean_iff_nf <- aggregate(df_iff_nf$iff, list(df_iff_nf$policy), mean, na.rm = TRUE) 

# IFF no filter
bar4 <- ggplot(mean_iff_nf, aes(Group.1, x, fill = Group.1,label=(round(x, digits = 0)))) + 
  geom_bar(stat="identity", position = "dodge", show.legend=FALSE) + theme_classic() +
  theme(axis.text.x = element_text(size=14, angle=45, vjust=0.5),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=14, angle=0),
        axis.title.y = element_text(face="bold", color="black", 
                                    size=14)) +
  ylab('Integration focus factor/no filters') + xlab('Policy')+
  geom_text(size = 3,position = position_stack(vjust = 0.5))
ggplotly(bar4, width=600, height=400) 
# ggsave('~/Dropbox/WMU/analysis/figures/ms_iff_no_filter_bar.png', bar4, width = 150, height = 120, units = "mm", dpi=500)

###################################################   DEV







