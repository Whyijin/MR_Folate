# MR
#####Load useful package###########
# data.table - for reading in / writing out files
#install.packages("data.table")
library("data.table")
# install.packages("openxlsx")
library(openxlsx)
#readxl
#install.packages('readxl')
library("readxl")
#tibble
#install.packages("tibble")
library("tibble")
#dplyr
# install.packages('dplyr')
library('dplyr')
#tidyr
#install.packages("tidyr")
library("tidyr")
# read in vcf files
#install.packages("vcfR")
library(vcfR)
# library(remotes)
# install_github("MRCIEU/TwoSampleMR")
library(TwoSampleMR)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(doBy)
library(gtsummary)
library(haven)
library(tableone)
library(stats)
#set working directory
setwd("/Users/xiangyijin/Desktop/Research/Wiemels/MR/Data/")
outpath <- "/Users/xiangyijin/Desktop/Research/Wiemels/MR/Data/MR_output"
############################################
############################################



#######################
## By ethnicity groups#
#######################

#######################
####### CCRLP lat######
#######################

#Seperate out exposure and outcome data form MR
exposure_ccrlp_lat_data <- output_match_ccrlp_lat_final %>% 
  select(exposure,SNP,effect_allele.exposure,
         other_allele.exposure,eaf.exposure,beta.exposure,
         se.exposure,pval.exposure,gene.exposure,samplesize.exposure)%>%
  mutate(id.exposure = exposure)%>% 
  unique()

outcome_ccrlp_lat_data <- output_match_ccrlp_lat_final %>% 
  select(SNP,other_allele.outcome,effect_allele.outcome,
         eaf.outcome,
         logOR,
         SE,P)%>%
  rename(beta.outcome = logOR,
         se.outcome = SE,
         pval.outcome = P)%>%
  mutate(outcome = "ALL",
         id.outcome = "Y")%>%
  group_by(SNP) %>%
  filter(row_number()==1)


# Harmonize the data 
dat_ccrlp_lat <- harmonise_data(
  exposure_dat =  exposure_ccrlp_lat_data, 
  outcome_dat = outcome_ccrlp_lat_data)
mr_pleiotropy_test(dat_ccrlp_lat)
mr_heterogeneity(dat_ccrlp_lat)

# MR Study
res_ccrlp_lat <- mr(dat_ccrlp_lat,
              method_list= c("mr_ivw_fe",
                             "mr_wald_ratio",
                             "mr_egger_regression",
                             "mr_weighted_median"))
res_ccrlp_lat_loo <- mr_leaveoneout(dat_ccrlp_lat)

res_lat_sen <- mr(dat_ccrlp_lat%>% filter(!(SNP %in% c("rs1801133")& id.exposure == "Homocysteine")),
                  method_list= c("mr_ivw_fe",
                                 "mr_wald_ratio",
                                 "mr_egger_regression",
                                 "mr_weighted_median"))

res_ccrlp_lat_single <- mr_singlesnp(dat_ccrlp_lat,
                               all_method=c("mr_ivw_fe",
                                            "mr_ivw_mre",
                                            "mr_wald_ratio",
                                            "mr_egger_regression",
                                            "mr_weighted_median"))
mr_result_ccrlp_lat <- res_ccrlp_lat_single %>% 
  mutate(exposure = paste0(exposure,"-","CCRLP_LAT"),
         cases = 1878,
         control = 2023)%>%
  group_by(exposure) %>%
  mutate(count_snp = sum(grepl("^rs", SNP))) %>% 
  left_join(mr_heterogeneity(dat_ccrlp_lat) %>%
              filter(method =="Inverse variance weighted")%>%
              select(id.exposure, Q_pval))


res_ccrlp_lat%>% ggplot(aes(x=exposure,y=b,color = exposure,shape=method))+#Add dot plot and error bars
  geom_errorbar(aes(ymin = b-1.96*se, ymax = b+1.96*se),position=position_dodge(width = 0.5), width = 0.25) +
  geom_point(size = 2.5,position=position_dodge(width = 0.5))+ 
  ylab("Beta")+
  scale_color_npg()+
  guides(color = FALSE)+
  labs(shape = "Method")+
  geom_hline(yintercept=0, linetype = "longdash") + 
  theme(axis.ticks.y=element_blank(),
        legend.position="right",
        legend.text=element_text(angle=0,size=12,family="Times New Roman"),
        strip.text.y=element_text(angle=0, face="bold",size=12,family="Times New Roman"),
        panel.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA),
        axis.text=element_text(colour="black",size=12,family="Times New Roman"),
        axis.title.x=element_blank(),
        # strip.background=element_blank(),
        # plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(linetype="dotted",color="gray"),
        title=element_text(face="bold",size=14,family="Times New Roman"))
\
#######################
####### CCRLP NLW######
#######################


#Seperate out exposure and outcome data form MR
exposure_ccrlp_nlw_data <- output_match_ccrlp_nlw_final %>% select(exposure,SNP,effect_allele.exposure,
                                                 other_allele.exposure,eaf.exposure,beta.exposure,
                                                 se.exposure,pval.exposure,gene.exposure,samplesize.exposure)%>%
  mutate(id.exposure = exposure)%>% 
  unique()

outcome_ccrlp_nlw_data <- output_match_ccrlp_nlw_final %>% 
  select(SNP,other_allele.outcome,effect_allele.outcome,
         eaf.outcome,
         logOR,
         SE,P)%>%
  rename(beta.outcome = logOR,
         se.outcome = SE,
         pval.outcome = P)%>%
  mutate(outcome = "ALL",
         id.outcome = "Y")%>%
  group_by(SNP) %>%
  filter(row_number()==1)

# Harmonize the data 
dat_ccrlp_nlw<- harmonise_data(
  exposure_dat =  exposure_ccrlp_nlw_data, 
  outcome_dat = outcome_ccrlp_nlw_data)
mr_pleiotropy_test(dat_ccrlp_nlw)
mr_heterogeneity(dat_ccrlp_nlw)

# MR Study
res_ccrlp_nlw <- mr(dat_ccrlp_nlw,
              method_list= c("mr_ivw_fe",
                             "mr_wald_ratio",
                             "mr_egger_regression",
                             "mr_weighted_median"))
res_ccrlp_nlw_loo <- mr_leaveoneout(dat_ccrlp_nlw)

# res_nlw_sen <- mr(dat_nlw%>% filter(!SNP %in% c("rs1801222","rs1801133")),
#               method_list= c("mr_ivw",
#                              # "mr_ivw_mre",
#                              "mr_wald_ratio","mr_egger_regression",
#                              "mr_weighted_median","mr_weighted_mode"))
res_ccrlp_nlw_single <- mr_singlesnp(dat_ccrlp_nlw,
                                     all_method=c("mr_ivw_fe",
                                                  "mr_ivw_mre",
                                                  "mr_wald_ratio",
                                                  "mr_egger_regression",
                                                  "mr_weighted_median"))

mr_result_ccrlp_nlw <- res_ccrlp_nlw_single %>% 
  mutate(exposure = paste0(exposure,"-","CCRLP_NLW"),
         cases = 1162,
         control = 1229,)%>%
  group_by(exposure) %>%
  mutate(count_snp = sum(grepl("^rs", SNP)))  %>% 
  left_join(mr_heterogeneity(dat_ccrlp_nlw) %>%
              filter(method =="Inverse variance weighted")%>%
              select(id.exposure, Q_pval))

res_ccrlp_nlw%>% ggplot(aes(x=exposure,y=b,color = exposure,shape=method))+#Add dot plot and error bars
  geom_errorbar(aes(ymin = b-1.96*se, ymax = b+1.96*se),position=position_dodge(width = 0.5), width = 0.25) +
  geom_point(size = 2.5,position=position_dodge(width = 0.5))+ 
  ylab("Beta")+
  scale_color_npg()+
  guides(color = FALSE)+
  labs(shape = "Method")+
  geom_hline(yintercept=0, linetype = "longdash") + 
  theme(axis.ticks.y=element_blank(),
        legend.position="right",
        legend.text=element_text(angle=0,size=12,family="Times New Roman"),
        strip.text.y=element_text(angle=0, face="bold",size=12,family="Times New Roman"),
        panel.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA),
        axis.text=element_text(colour="black",size=12,family="Times New Roman"),
        axis.title.x=element_blank(),
        # strip.background=element_blank(),
        # plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(linetype="dotted",color="gray"),
        title=element_text(face="bold",size=14,family="Times New Roman"))


#######################
######## CCLS lat #####
#######################

#Seperate out exposure and outcome data form MR
ccls_lat_exposure_data <- ccls_output_lat_match %>% 
  select(exposure,SNP,effect_allele.exposure,
         other_allele.exposure,eaf.exposure,beta.exposure,
         se.exposure,pval.exposure,gene.exposure,samplesize.exposure)%>%
  mutate(id.exposure = exposure)

ccls_lat_outcome_data <- ccls_output_lat_match %>% 
  mutate(effect_allele.outcome=A1,
         other_allele.outcome=ifelse(A1==ALT1,REF,ALT1))%>%
  select(SNP,effect_allele.outcome,other_allele.outcome,A1_FREQ,BETA,
         SE,P)%>%
  rename(eaf.outcome = A1_FREQ,
         beta.outcome = BETA,
         se.outcome = SE,
         pval.outcome = P)%>%
  mutate(outcome = "ALL",
         id.outcome = "Y")%>%
  group_by(SNP) %>%
  filter(row_number()==1)

# Harmonize the data 
ccls_lat_dat <- harmonise_data(
  exposure_dat =  ccls_lat_exposure_data, 
  outcome_dat = ccls_lat_outcome_data
)
mr_pleiotropy_test(ccls_lat_dat)
mr_heterogeneity(ccls_lat_dat)

# MR Study
res_ccls_lat <- mr(ccls_lat_dat,
              method_list= c("mr_ivw_fe",
                             "mr_ivw_mre",
                             "mr_wald_ratio",
                             "mr_egger_regression",
                             "mr_weighted_median"))
res_ccls_lat_loo <- mr_leaveoneout(ccls_lat_dat)

# ccls_lat_res_sen <- mr(ccls_lat_dat%>% filter(!SNP %in% c("rs1801222","rs1801133")),
#                    method_list= c("mr_ivw",
#                                   # "mr_ivw_mre", 
#                                   "mr_wald_ratio","mr_egger_regression",
#                                   "mr_weighted_median","mr_weighted_mode"))

res_ccls_lat_single <- mr_singlesnp(ccls_lat_dat,
                                     all_method=c("mr_ivw_fe",
                                                  "mr_ivw_mre",
                                                  "mr_wald_ratio",
                                                  "mr_egger_regression",
                                                  "mr_weighted_median"))
mr_result_ccls_lat <- res_ccls_lat_single %>% 
  mutate(exposure = paste0(exposure,"-","CCLS_LAT"),
         cases = 750,
         control = 504)%>%
  group_by(exposure) %>%
  mutate(count_snp = sum(grepl("^rs", SNP))) %>% 
  left_join(mr_heterogeneity(ccls_lat_dat) %>%
              filter(method =="Inverse variance weighted")%>%
              select(id.exposure, Q_pval))

ccls_lat_dat_rc <- ccls_lat_dat %>% mutate(wald_ratio = beta.outcome/beta.exposure)

#######################
######## CCLS nlw #####
#######################

# Seperate out exposure and outcome data form MR

ccls_eur_exposure_data <- ccls_output_eur_match %>% 
  dplyr::select(exposure,SNP,effect_allele.exposure,
         other_allele.exposure,eaf.exposure,beta.exposure,
         se.exposure,pval.exposure,gene.exposure,samplesize.exposure)%>%
  mutate(id.exposure = exposure)

ccls_eur_outcome_data <- ccls_output_eur_match %>% 
  mutate(effect_allele.outcome=A1,
         other_allele.outcome=ifelse(A1==ALT1,REF,ALT1))%>%
  dplyr::select(SNP,effect_allele.outcome,other_allele.outcome,
         A1_FREQ,BETA,SE,P)%>%
  rename(eaf.outcome = A1_FREQ,
         beta.outcome = BETA,
         se.outcome = SE,
         pval.outcome = P)%>%
  mutate(outcome = "ALL",
         id.outcome = "Y")%>%
  group_by(SNP) %>%
  filter(row_number()==1)

# Harmonize the data 
ccls_eur_dat <- harmonise_data(
  exposure_dat =  ccls_eur_exposure_data, 
  outcome_dat = ccls_eur_outcome_data
)
mr_pleiotropy_test(ccls_eur_dat)
mr_heterogeneity(ccls_eur_dat)
res_ccls_eur <- mr(ccls_eur_dat,
                   method_list= c("mr_ivw_fe",
                                  "mr_ivw_mre",
                                  "mr_wald_ratio",
                                  "mr_egger_regression",
                                  "mr_weighted_median"))
res_ccls_nlw_loo <- mr_leaveoneout(ccls_eur_dat)

# ccls_eur_res_sen <- mr(ccls_eur_dat %>% filter(!SNP %in% c("rs1801222","rs1801133")),
#                    method_list= c("mr_ivw",
#                                   # "mr_ivw_mre",
#                                   "mr_wald_ratio","mr_egger_regression",
#                                   "mr_weighted_median","mr_weighted_mode"))
# # write.csv(ccls_lat_res,paste(outpath,"mr_ccls_lat_outcome_1023.csv",sep="/"))

res_ccls_nlw_single <- mr_singlesnp(ccls_eur_dat,
                                    all_method=c("mr_ivw_fe",
                                                 "mr_ivw_mre",
                                                 "mr_wald_ratio",
                                                 "mr_egger_regression",
                                                 "mr_weighted_median"))

mr_result_ccls_nlw <- res_ccls_nlw_single %>% 
  mutate(exposure = paste0(exposure,"-","CCLS_NLW"),
         cases = 472,
         control = 340)%>%
  group_by(exposure) %>%
  mutate(count_snp = sum(grepl("^rs", SNP))) %>% 
  left_join(mr_heterogeneity(ccls_eur_dat) %>%
              filter(method =="Inverse variance weighted")%>%
              select(id.exposure, Q_pval))


ccls_eur_dat_rc <- ccls_eur_dat %>% mutate(wald_ratio = beta.outcome/beta.exposure)


############################
### Meta analysed latino ###
############################


# Seperate out exposure and outcome data form MR
lat_meta_exposure_data <- lat_meta_output_match %>% 
  select(exposure,SNP,effect_allele.exposure,
         other_allele.exposure,eaf.exposure,beta.exposure,
         se.exposure,pval.exposure,gene.exposure,samplesize.exposure)%>%
  mutate(id.exposure = exposure)

lat_meta_outcome_data <- lat_meta_output_match %>%
  select(SNP,effect_allele.outcome,other_allele.outcome,
         eaf.outcome,Effect_lat,StdErr_lat,p_lat)%>%
  rename(beta.outcome = Effect_lat,
         se.outcome = StdErr_lat,
         pval.outcome = p_lat)%>%
  mutate(outcome = "ALL",
         id.outcome = "Y")%>%
  group_by(SNP) %>%
  filter(row_number()==1)

# Harmonize the data 
lat_meta_dat <- harmonise_data(
  exposure_dat =  lat_meta_exposure_data, 
  outcome_dat = lat_meta_outcome_data
)
mr_heterogeneity(lat_meta_dat)

# MR Study
res_lat_meta <- mr(lat_meta_dat,
                   method_list= c("mr_ivw_fe",
                                  "mr_ivw_mre",
                                  "mr_wald_ratio",
                                  "mr_egger_regression",
                                  "mr_weighted_median"))
res_lat_meta_loo <- mr_leaveoneout(lat_meta_dat)
# lat_res <- mr(res_lat_meta,
#                    method_list= c("mr_ivw",
#                                   # "mr_ivw_mre",
#                                   "mr_wald_ratio","mr_egger_regression",
#                                   "mr_weighted_median","mr_weighted_mode"))
res_lat_meta_single <- mr_singlesnp(lat_meta_dat,
                                     all_method=c("mr_ivw_fe",
                                                  "mr_ivw_mre",
                                                  "mr_wald_ratio",
                                                  "mr_egger_regression",
                                                  "mr_weighted_median"))
mr_result_lat_meta <- res_lat_meta_single %>% 
  mutate(exposure = paste0(exposure,"-","LAT_META"),
         cases = 2628,
         control = 2527)%>%
  group_by(exposure) %>%
  mutate(count_snp = sum(grepl("^rs", SNP))) %>% 
  left_join(mr_heterogeneity(lat_meta_dat) %>%
              filter(method =="Inverse variance weighted")%>%
              select(id.exposure, Q_pval))

res_lat_meta %>% ggplot(aes(x=exposure,y=b,color = exposure,shape=method))+#Add dot plot and error bars
  geom_errorbar(aes(ymin = b-1.96*se, ymax = b+1.96*se),position=position_dodge(width = 0.5), width = 0.25) +
  geom_point(size = 2.5,position=position_dodge(width = 0.5))+ 
  ylab("Beta")+
  scale_color_npg()+
  guides(color = FALSE)+
  labs(shape = "Method")+
  geom_hline(yintercept=0, linetype = "longdash") + 
  theme(axis.ticks.y=element_blank(),
        legend.position="right",
        legend.text=element_text(angle=0,size=12,family="Times New Roman"),
        strip.text.y=element_text(angle=0, face="bold",size=12,family="Times New Roman"),
        panel.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA),
        axis.text=element_text(colour="black",size=12,family="Times New Roman"),
        axis.title.x=element_blank(),
        # strip.background=element_blank(),
        # plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(linetype="dotted",color="gray"),
        title=element_text(face="bold",size=14,family="Times New Roman"))

#########################
### Meta analysed NLW ###
#########################

# Seperate out exposure and outcome data form MR
nlw_meta_exposure_data <- nlw_meta_output_match %>% 
  select(exposure,SNP,effect_allele.exposure,
         other_allele.exposure,eaf.exposure,beta.exposure,
         se.exposure,pval.exposure,gene.exposure,samplesize.exposure)%>%
  mutate(id.exposure = exposure)

nlw_meta_outcome_data <- nlw_meta_output_match %>%
  select(SNP,effect_allele.outcome,other_allele.outcome,
         eaf.outcome,Effect_nlw,StdErr_nlw,p_nlw)%>%
  rename(beta.outcome = Effect_nlw,
         se.outcome = StdErr_nlw,
         pval.outcome = p_nlw)%>%
  mutate(outcome = "ALL",
         id.outcome = "Y")%>%
  group_by(SNP) %>%
  filter(row_number()==1)

# Harmonize the data 
nlw_meta_dat <- harmonise_data(
  exposure_dat =  nlw_meta_exposure_data, 
  outcome_dat = nlw_meta_outcome_data
)
mr_heterogeneity(nlw_meta_dat)
res_nlw_meta <- mr(nlw_meta_dat,
                   method_list= c("mr_ivw_fe","mr_ivw_mre",
                                  "mr_wald_ratio","mr_egger_regression",
                                  "mr_weighted_median"))
res_nlw_meta_single <- mr_singlesnp(nlw_meta_dat,
                                    all_method=c("mr_ivw_fe",
                                                 "mr_ivw_mre",
                                                 "mr_wald_ratio",
                                                 "mr_egger_regression",
                                                 "mr_weighted_median"))
mr_result_nlw_meta <- res_nlw_meta_single %>% 
  mutate(exposure = paste0(exposure,"-","NLW_META"),
         cases = 1634,
         control = 1569)%>%
  group_by(exposure) %>%
  mutate(count_snp = sum(grepl("^rs", SNP))) %>% 
  left_join(mr_heterogeneity(nlw_meta_dat) %>%
              filter(method =="Inverse variance weighted")%>%
              select(id.exposure, Q_pval))

res_nlw_meta%>% ggplot(aes(x=exposure,y=b,color = exposure,shape=method))+#Add dot plot and error bars
  geom_errorbar(aes(ymin = b-1.96*se, ymax = b+1.96*se),position=position_dodge(width = 0.5), width = 0.25) +
  geom_point(size = 2.5,position=position_dodge(width = 0.5))+ 
  ylab("Beta")+
  scale_color_npg()+
  guides(color = FALSE)+
  labs(shape = "Method")+
  geom_hline(yintercept=0, linetype = "longdash") + 
  theme(axis.ticks.y=element_blank(),
        legend.position="right",
        legend.text=element_text(angle=0,size=12,family="Times New Roman"),
        strip.text.y=element_text(angle=0, face="bold",size=12,family="Times New Roman"),
        panel.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA),
        axis.text=element_text(colour="black",size=12,family="Times New Roman"),
        axis.title.x=element_blank(),
        # strip.background=element_blank(),
        # plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(linetype="dotted",color="gray"),
        title=element_text(face="bold",size=14,family="Times New Roman"))



#########################
### Meta analysed ALL ###
#########################

# Seperate out exposure and outcome data form MR
all_exposure_data <- all_output_match %>% 
  select(exposure,SNP,effect_allele.exposure,
         other_allele.exposure,eaf.exposure,beta.exposure,
         se.exposure,pval.exposure,gene.exposure,samplesize.exposure)%>%
  mutate(id.exposure = exposure)

all_outcome_data <- all_output_match %>%
  select(SNP,effect_allele.outcome,other_allele.outcome,
         eaf.outcome,Effect_all,StdErr_all,p_all)%>%
  rename(beta.outcome = Effect_all,
         se.outcome = StdErr_all,
         pval.outcome = p_all)%>%
  mutate(outcome = "ALL",
         id.outcome = "Y")%>%
  group_by(SNP) %>%
  filter(row_number()==1)

# Harmonize the data 
all_dat <- harmonise_data(
  exposure_dat =  all_exposure_data, 
  outcome_dat = all_outcome_data
)
mr_heterogeneity(all_dat)

res_all_meta <- mr(all_dat,
              method_list= c("mr_ivw_fe","mr_ivw_mre",
                             "mr_wald_ratio","mr_egger_regression",
                             "mr_weighted_median"))
res_all_meta_single <- mr_singlesnp(all_dat,
                                    all_method=c("mr_ivw_fe",
                                                 "mr_ivw_mre",
                                                 "mr_wald_ratio",
                                                 "mr_egger_regression",
                                                 "mr_weighted_median"))
mr_result_all_meta <- res_all_meta_single %>% 
  mutate(exposure = paste0(exposure,"-","ALL_META"),
         cases = 4262,
         control = 3138)%>%
  group_by(exposure) %>%
  mutate(count_snp = sum(grepl("^rs", SNP))) %>% 
  left_join(mr_heterogeneity(all_dat) %>%
              filter(method =="Inverse variance weighted")%>%
              select(id.exposure, Q_pval))

res_all_meta%>% ggplot(aes(x=exposure,y=b,color = exposure,shape=method))+#Add dot plot and error bars
  geom_errorbar(aes(ymin = b-1.96*se, ymax = b+1.96*se),position=position_dodge(width = 0.5), width = 0.25) +
  geom_point(size = 2.5,position=position_dodge(width = 0.5))+ 
  ylab("Beta")+
  scale_color_npg()+
  guides(color = FALSE)+
  labs(shape = "Method")+
  geom_hline(yintercept=0, linetype = "longdash") + 
  theme(axis.ticks.y=element_blank(),
        legend.position="right",
        legend.text=element_text(angle=0,size=12,family="Times New Roman"),
        strip.text.y=element_text(angle=0, face="bold",size=12,family="Times New Roman"),
        panel.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA),
        axis.text=element_text(colour="black",size=12,family="Times New Roman"),
        axis.title.x=element_blank(),
        # strip.background=element_blank(),
        # plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(linetype="dotted",color="gray"),
        title=element_text(face="bold",size=14,family="Times New Roman"))

mr_result_summary <- rbind(mr_result_ccrlp_lat,
                           mr_result_ccrlp_nlw,
                           mr_result_ccls_lat,
                           mr_result_ccls_nlw,
                           mr_result_lat_meta,
                           mr_result_nlw_meta,
                           mr_result_all_meta) %>%
  filter(!is.na(b)) 

