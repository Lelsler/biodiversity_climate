# this file creates plots and statistics of keyword frequency and focus factors 
# Laura Gabriele Elsler, Maartje Oostdijk, WMU

### set up
# clear workspace
rm(list = ls())
graphics.off()

# directory
dirct <- "~/Dropbox/current_projects/ocean_carbon/analysis/data/analysis"
setwd("~/Dropbox/current_projects/ocean_carbon/analysis")

# libraries
library(tidyverse)
require(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(data.table)
library(grid)
library(gridExtra)

###################################################################################################################
##########################################   READ AND CLEAN   #####################################################
###################################################################################################################

##################################################   KW AND FF   ##################################################

# read files
df_cff <- read.csv(file.path(dirct, 'ms_cff_all.csv'), as.is=T) %>% select(-X) %>% mutate(cat='climate')
df_sff <- read.csv(file.path(dirct, 'ms_sff_all.csv'), as.is=T) %>% select(-X) %>% mutate(cat='species')
df_iff <- read.csv(file.path(dirct, 'ms_iff_all.csv'), as.is=T) %>% select(-X) %>% mutate(cat='integration')

# capitalize policies
df_cff[,1] = toupper(df_cff[,1])
df_sff[,1] = toupper(df_sff[,1])
df_iff[,1] = toupper(df_iff[,1])

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
df_ff <- rbind(df_sff,df_cff,df_iff) %>% distinct() # double (or multi-) entries are a result of multiple kw in one pdf

# a file with all focus factors in individual columns
df_cff_pdf <- subset(df_cff, select = -c(keyword,kw_type,individual_count_keywords,count_keywords,count_categories,total_words,cat))
df_sff_pdf <- subset(df_sff, select = -c(keyword,kw_type,individual_count_keywords,count_keywords,count_categories,total_words,cat))
df_iff_pdf <- subset(df_iff, select = -c(keyword,kw_type,individual_count_keywords,count_keywords,count_categories,total_words,cat))
df_ff_ind <- df_cff_pdf %>% full_join(df_sff_pdf) %>% full_join(df_iff_pdf) %>% distinct() 
rm(df_cff_pdf,df_sff_pdf,df_iff_pdf)

##################################################   TEXT FILES   #################################################

# read text files
t_cff <- read.csv(file.path(dirct, 'ms_cff_all_text.csv'), as.is=T) 
t_sff <- read.csv(file.path(dirct, 'ms_sff_all_text.csv'), as.is=T) 
t_iff <- read.csv(file.path(dirct, 'ms_iff_all_text.csv'), as.is=T) 
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

# remove df not further needed
rm(unknown,keywords)

#############################################   STATISTICS     ####################################################

### No of PDFS MENTIONING KW/CATEGORY
unique(df_sff$pdf_name)
unique(df_cff$pdf_name)
unique(df_iff$pdf_name)

### KW/PDF IN CATEGORIES
a <-ff %>%
  group_by(cat)%>%
  tally()%>%
  mutate(per_pdf=(n/sum(pdf$pdfs)))

### KW/PDF IN POLICIES
b <-ff%>%
  group_by(policy)%>%
  tally()%>%
  left_join(pdf)%>%
  mutate(per_pdf=(n/pdfs))

### KW/PDF PDF IN POLICIES AND CATEGORY
c <-ff%>%
  group_by(policy,cat)%>%
  tally()%>%
  left_join(pdf)%>%
  mutate(per_pdf=(n/pdfs))

### KW FREQ/PDF
d <- ff_freq%>%
  group_by(policy,pdf_name,cat)%>%
  summarise(total_kw = sum(individual_count_keywords)) # step 1: filter by CBD and climate and order max-min; step 2: filter by bbnj and order max-min

# identify frequency and keywords mentioned in the top ranking pdf's
# view(d)   OR  view(ff_freq) 
# cbd: MUS-nbsap-v2-en-2020.pdf
# bbnj textual proposals: textual_proposals_compilation_-_15_april_2020.pdf
# bbnj draft: a_conf.232_2020_3_E.pdf

# show rows of the pdf with highest kw frequency
e <- ff_freq[grepl("MUS-nbsap-v2-en-2020.pdf", ff_freq$pdf_name),]
f <- ff_freq[grepl("textual_proposals_compilation_-_15_april_2020.pdf", ff_freq$pdf_name),]
g <- ff_freq[grepl("a_conf.232_2020_3_E.pdf", ff_freq$pdf_name),]

#################################################   PREP PLOT DATA   ###########################################

# kw counted per set and policy
ff_sums=ff_freq %>% 
  group_by(policy, kw_set, cat, pdfs) %>%
  summarise(total_keywords = sum(individual_count_keywords)) %>%
  mutate(kw_per_pdf=total_keywords/pdfs) %>% 
  mutate(alt= ifelse(kw_per_pdf > 1, round(kw_per_pdf,0),
                         ifelse(kw_per_pdf<1 & kw_per_pdf>0,round(kw_per_pdf,1)))) 
# correct category names
ff_sums <- ff_sums %>% mutate(col = ifelse(cat == 'species', 'A. Biodiversity', 
                                           ifelse(cat == 'climate', 'B. Climate',  
                                                  ifelse(cat == 'integration', 'C. Ocean carbon', NA)))) 
ff_sums$cat <- NULL
ff_sums <- ff_sums %>% rename(cat=col)

# round displayed numbers and remove those <2
ff_sums$display <- ifelse(ff_sums$alt>0.9, round(ff_sums$alt,0), ff_sums$alt) 
ff_sums$display <- ifelse(ff_sums$alt<10, '', ff_sums$alt)


#################################################   PLOT KW FREQUENCY   ###########################################

bar <- ggplot(ff_sums, aes(x=kw_set, y=kw_per_pdf, fill=policy, label=display)) + 
  geom_bar(stat = "identity") + theme_classic()+
  geom_text(size = 5, position = position_stack(vjust = 0.5)) +
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.text.x = element_text(angle = 90)) +
  ylab('Keywords per policy document') + xlab('') + labs(fill = 'Agreement')+
  facet_wrap(~cat,scales = "free_x")+theme(text = element_text(size = 20)) +
  scale_fill_manual(values = c("#9D4146", "#D77658", "#C48407", "#6F9F00", "#2CA37E", "#00A2A2", "#5F96C2", "#888CC7", "#B37EBE", "#FFB0C2"))

ggplotly(bar, width=400, height=250) 
# ggsave(filename="figures/Fig1_keywordfig.jpg", width = 400, height = 200, units = "mm", dpi=300)

# remove df's
rm(a,b,c,d,e,f,g,ff_freq,bar,pdf)


###################################################################################################################
##############################################   FOCUS FACTORS   ##################################################
###################################################################################################################

##################################################   PREP DATA   ##################################################


# calculate average value of cff/sff/iff
mean_cff <- aggregate(df_cff$ff, list(df_cff$policy), mean, na.rm = TRUE) %>% rename(policy=Group.1, ff=x)
mean_sff <- aggregate(df_sff$ff, list(df_sff$policy), mean, na.rm = TRUE) %>% rename(policy=Group.1, ff=x)
mean_iff <- aggregate(df_iff$ff, list(df_iff$policy), mean, na.rm = TRUE) %>% rename(policy=Group.1, ff=x)
miss <- data.frame(policy=c('CITES'), ff=c(0))
mean_iff <- rbind(mean_iff, miss)

# calculate mean and sd of cff/sff/iff
mean_cff <- df_cff %>% group_by(policy) %>% summarise(
    csd = sd(ff, na.rm = TRUE), cff = mean(ff))
mean_sff <- df_sff %>% group_by(policy) %>% summarise(
  ssd = sd(ff, na.rm = TRUE), sff = mean(ff))
mean_iff <- df_iff %>% group_by(policy) %>% summarise(
  isd = sd(ff, na.rm = TRUE), iff = mean(ff))
mean_ff <- mean_cff %>% full_join(mean_sff) %>% full_join(mean_iff)
# write.csv(mean_ff,'./data/analysis/mean_ff.csv')

# write csv with agreements and focus factors
mcff <- mean_cff %>% rename(cff=ff)
msff <- mean_sff %>% rename(sff=ff)
miff <- mean_iff %>% rename(iff=ff)
mean_ff <- mcff %>% full_join(msff) %>% full_join(miff)

#################################################   STATS FOR TEXT   ################################################

# report maxima
#PDF with highest FF
max(df_ff$ff) # highest FF in PDF
a <- df_ff[grepl("NEAFC_Rec9_Amend-Ann-1b-Ann-5_Scheme.pdf", ff_freq$pdf_name),]

#policy with highest FF
a <- rbind(mean_cff,mean_sff,mean_iff) 
# view(a) # highest FF in policy, sort FF

# mean, sd, and coefficient of variation of all FF
stat_ff <- mean_ff %>% summarize(
  count = n(),
  meanc = mean(cff, na.rm = TRUE), 
  sdc = sd(cff, na.rm = TRUE),
  means = mean(sff, na.rm = TRUE), 
  sds = sd(sff, na.rm = TRUE),
  meani = mean(iff, na.rm = TRUE), 
  sdi = sd(iff, na.rm = TRUE))  %>% 
  mutate(cvc = sdc/meanc)%>% 
  mutate(cvs = sds/means)%>% 
  mutate(cvi = sdi/meani)

#################################################   SFF
# SFF
sffrank_pdf <- df_sff %>% select(policy,pdf_name,keyword,individual_count_keywords,ff) %>% distinct() %>%  filter(!is.na(ff))

# to find policies with high sff 
sffrank <- sffrank_pdf %>% 
  group_by(policy) %>%
  summarize(mean = mean(ff)) 
# view(sffrank) # sort by ff

# find PDF with max value sff 
# view(sffrank_pdf) # filter by policy, sort ff
max(sffrank_pdf$ff)


#################################################   CFF
# CFF
cffrank_pdf <- df_cff %>% select(policy,pdf_name,keyword,individual_count_keywords,ff) %>%  filter(!is.na(ff)) %>% distinct()

# to find policies with high sff 
cffrank <- cffrank_pdf %>% 
  group_by(policy) %>%
  summarize(mean = mean(ff)) 
# view(cffrank) # result,  used

# find PDF with max value sff 
# view(cffrank_pdf) # sort by ff
max(cffrank_pdf$ff)


#################################################   IFF
# IFF
iffrank_pdf <- df_iff %>% select(policy,pdf_name,keyword,individual_count_keywords,ff) %>%  filter(!is.na(ff)) 

# to find policies with high iff 
iffrank <- iffrank_pdf %>% 
  group_by(policy) %>%
  summarize(mean = mean(ff)) 
# view(sffrank) # result,  used

# find PDF with max value sff 
# view(iffrank_pdf) # sort by ff
max(iffrank_pdf$ff)

# total number of keywords per policy 
iffrank_pdf <- df_iff %>% select(policy,pdf_name,keyword,individual_count_keywords) %>% 
  distinct()# to find pdfs with high counts run only this part
iff_no_kw <- iffrank_pdf %>% 
  group_by(policy) %>% 
  summarize(sum = sum(individual_count_keywords)) 

# unique keywords per policy 
uni_kw <- aggregate( keyword~policy,df_iff, function(x) length(unique(x)))
uni_kw_example <- df_iff %>% filter(policy == 'CBD') # just to have a look
unique(uni_kw_example$keyword)

# pdfs with high # of hits
no_pdf <- iffrank_pdf %>% 
  group_by(pdf_name) %>% 
  summarize(sum = sum(individual_count_keywords)) 

# pull out text examples from here
df_iff_text <- read.csv(file.path(dirct, 'ms_iff_all_text.csv'), as.is=T) 

# keep bbnj rows 
d <- df_ff[grepl("BBNJ", df_ff$policy),]
e <- d %>% 
  group_by(pdf_name) %>% 
  summarize(sum = sum(individual_count_keywords)) 


########################################### TABLE S4 PDFS   ##################################################

# show rows of the pdf with highest kw frequency
e <- df_ff[grepl("bbnj_igc_iii_side_events_schedule_final_eng.pdf", df_ff$pdf_name),]
f <- df_ff[grepl("A_CONF.232_2019_1_E.pdf", df_ff$pdf_name),]
g <- df_ff[grepl("MUS-nbsap-v2-en-2020.pdf", df_ff$pdf_name),]
h <- df_ff[grepl("E-Res-09-25-R18.pdf", df_ff$pdf_name),]
i <- df_ff[grepl("cms_sharks-mos3_outcome3.1_amendment-annex1_e.pdf", df_ff$pdf_name),]
j <- df_ff[grepl("cms_cop12_res.12.21_climate-change_e.pdf", df_ff$pdf_name),]
k <- df_ff[grepl("cms_cop12_res.12.17_whales-south-atlantic_e.pdf", df_ff$pdf_name),]
l <- df_ff[grepl("LC-SG 44-INF.5 - Results of Canada's 2019 disposal site monitoring program (Canada).pdf", df_ff$pdf_name),]
n <- df_ff[grepl("LP 1-INF.2 - Information on the Composition of CO2 streams from Capture Plants (United States, Norway and...).pdf", df_ff$pdf_name),]
o <- df_ff[grepl("LP 1-6-1 - Proposal to amend Annex 1 of the 1996 Protocol to the Convention on the Prevention of Mar... (Spain).pdf", df_ff$pdf_name),]
p <- df_ff[grepl("N0254760.pdf", df_ff$pdf_name),]
q <- df_ff[grepl("a_res_71_312_E.pdf", df_ff$pdf_name),]
r <- df_ff[grepl("NEAFC_Rec9_Amend-Ann-1b-Ann-5_Scheme.pdf", df_ff$pdf_name),]
s <- df_ff[grepl("CAMLR_r30-xxviii_5.pdf", df_ff$pdf_name),]
t <- df_ff[grepl("FAO_I4356EN.pdf", df_ff$pdf_name),]


###############################################   PLOT CFF and BFF   ##################################################

g.mid<-ggplot(mean_ff,aes(x=1,y=policy))+geom_text(aes(label=policy))+
  ggtitle("")+
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(1,-1,1,-1), "mm"))

g1 <- ggplot(data = mean_ff, aes(x = policy, y = sff, fill=policy))+
  geom_bar(stat = "identity") + ggtitle("Biodiversity focus factor")+
  geom_errorbar(aes(ymin = sff, ymax = sff+ssd), width = 0.2)+
  theme_classic()+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = 'none',
        title = element_text(face="bold", color="black", size=20),
        axis.line.y = element_blank(), axis.line.x = element_line(),
        plot.margin = unit(c(1,-1,1,0), "mm"), axis.text.x = element_text(size = 15))+
  scale_fill_manual(values = c("#9D4146", "#D77658", "#C48407", "#6F9F00", "#2CA37E", "#00A2A2", "#5F96C2", "#888CC7", "#B37EBE", "#FFB0C2"))+
  scale_y_reverse() + coord_flip() 

g2 <- ggplot(data = mean_ff, aes(policy, cff, fill=policy, dlabel=(round(cff, digits = 0)))) +xlab(NULL)+
  geom_bar(stat = "identity") + ggtitle("Climate focus factor") + 
  theme_classic()+
  geom_errorbar(aes(ymin = cff, ymax = cff+csd), width = 0.2)+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = 'none',
        title = element_text(face="bold", color="black", size=20),
        plot.margin = unit(c(1,0,1,-1), "mm"), axis.text.x = element_text(size = 15)) +
  scale_fill_manual(values = c("#9D4146", "#D77658", "#C48407", "#6F9F00", "#2CA37E", "#00A2A2", "#5F96C2", "#888CC7", "#B37EBE", "#FFB0C2"))+
  coord_flip()

gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

bar1 <- grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4.2/9,0.6/9,4.2/9))
# ggsave('~/Dropbox/current_projects/ocean_carbon/analysis/figures/Fig2_bcff_bar.jpg',bar1, width = 300, height = 150, units = "mm", dpi=300)


##################################################   PLOT OFF   #####################################################

bar2 <- ggplot(mean_iff, aes(policy, iff, fill = policy,label=(round(iff, digits = 0))))+ 
  geom_bar(stat="identity", position = "dodge", show.legend=FALSE)+ 
  # geom_text(size = 3,position = position_stack(vjust=0.5, hjust=0.1))+
  geom_errorbar(aes(ymin = iff, ymax = iff+isd), width = 0.2)+
  theme_classic()+
  theme(axis.text.x = element_text(size=14, angle=45, vjust=0.5),
        axis.title.x = element_blank(),
        legend.position = 'none',
        axis.text.y = element_text(size=14, angle=0),
        axis.title.y = element_text(face="bold", color="black", 
                                    size=14)) +
  ylab('Ocean carbon focus factor') + xlab('Agreement')+
  scale_fill_manual(values = c("#9D4146", "#D77658", "#C48407", "#6F9F00", "#2CA37E", "#00A2A2", "#5F96C2", "#888CC7", "#B37EBE", "#FFB0C2"))
ggplotly(bar2, width=600, height=400) 
# ggsave('~/Dropbox/current_projects/ocean_carbon/analysis/figures/Fig3_off_bar.jpg',bar2, width = 150, height = 120, units = "mm", dpi=300)






