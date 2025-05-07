# Stepwise analysis

############
# CCLS LAT #
############
# Filtered out all those individuals without FFQ or Down sydrome
ccls_lat_vb_all_filt <- ccls_lat_vb_all %>% filter(!is.na(ffq_dfe_tot_new)) %>% filter(ch_downs == 0)

# Table 1 
# Continuous maternal/paternal variables
maternal_paternal_continuous <- c(
  "fa_age_birth", "mo_age_birth", 
  "mo_gravid_int","parity","prevloss",
  "mo_height", "mo_weight", "mo_weightgain", 
  "ffq_dfe_food_new", "ffq_dfe_fort_new", "ffq_dfe_nat_new", 
  "ffq_dfe_tot_new", "ffq_h_eat"
  # "mo_alc_freq_bef", "mo_alc_freq_preg"
)

# Categorical maternal/paternal variables
maternal_paternal_categorical <- c(
  "plural","hh_educ_int","fa_educ_int", "income.f",
  "fa_ethnicity_bc", "fa_hispanic_bc", "fa_hispanic_int",
  "fa_race_bc", "fa_race_int", "mo_alc_ever_bef", "mo_alc_ever_preg", 
  "mo_educ_int", "mo_ethnicity_bc", "mo_hispanic_bc",
  "mo_hispanic_int", "mo_race_bc", "mo_race_int", 
  "mo_alc_freq_bef", "mo_alc_freq_preg", 
  "mo_smokpreg",
  "ffq_dfe_sup_new","sup_folate_yn","sup_folaye_400","mo_alc_freq_preg_cat"
)
# Continuous child-specific variables
child_continuous <- c(
  "br_feed_dur_mo", "br_feed_dur_wks", "ch_ageref", "ch_birthlength_int",
  "ch_birthwt_bc","ch_birthorder_int", "ch_birthwt_bc_full", "ch_birthwt_int", 
  "ch_gestage_int", "ch_siblings_int","PC1","PC2","PC3","PC4","PC5"
)

# Categorical child-specific variables
child_categorical <- c(
  "br_feed", "ch_age_category", "ch_delivery_int", "ch_birthyear",
  "ch_downs", "ch_ethnicity_final", "ch_hispanic_final", "ch_race_final", 
  "ch_sex","leuk_dx_infant"
)
# Create Table 1 for maternal and paternal variables
table1_maternal_paternal <- CreateTableOne(
  vars = unique(c(maternal_paternal_continuous, maternal_paternal_categorical)), 
  strata = "caco_char",
  data = ccls_lat_vb_all_filt,
  factorVars = maternal_paternal_categorical
)
table1_maternal_paternal_final <- print(table1_maternal_paternal, quote = F, noSpaces = TRUE, printToggle = TRUE, showAllLevels = TRUE,nonnormal = maternal_paternal_continuous)
write.csv(table1_maternal_paternal_final, "/Users/xiangyijin/Desktop/Research/Wiemels/MR/Writeup/Output/table1_ccls_lat_parental_final_1219.csv")
table1_maternal_paternal_final <- print(table1_maternal_paternal, quote = F, noSpaces = TRUE, printToggle = TRUE, showAllLevels = TRUE)
write.csv(table1_maternal_paternal_final, "/Users/xiangyijin/Desktop/Research/Wiemels/MR/Writeup/Output/table1_ccls_lat_parental_test_final_1219.csv")

# Create Table 1 for child-specific variables
table1_child <- CreateTableOne(
  vars = c(child_continuous, child_categorical), 
  strata = "caco_char",
  data = ccls_lat_vb_all_filt,
  factorVars = child_categorical
)
table1_child_final <- print(table1_child, quote = F, noSpaces = TRUE,nonnormal = child_continuous,printToggle = TRUE, showAllLevels = TRUE)
table1_child_final <- print(table1_child, quote = F, noSpaces = TRUE,printToggle = TRUE, showAllLevels = TRUE)
table(ccls_lat_vb_all_filt$parity,ccls_lat_vb_all_filt$ch_birthorder_int)

########################################################################
# Create Table 1 for maternal and paternal variables by missing status
table1_maternal_paternal <- CreateTableOne(
  vars = unique(c(maternal_paternal_continuous, maternal_paternal_categorical)), 
  strata = "ffq_dfe_na",
  data = ccls_lat_vb_all,
  factorVars = maternal_paternal_categorical
)
table1_maternal_paternal_final <- print(table1_maternal_paternal, quote = F, noSpaces = TRUE, printToggle = TRUE, showAllLevels = TRUE,nonnormal = maternal_paternal_continuous)
table1_maternal_paternal_final <- print(table1_maternal_paternal, quote = F, noSpaces = TRUE, printToggle = TRUE, showAllLevels = TRUE)

# Create Table 1 for child-specific variables
table1_child <- CreateTableOne(
  vars = c(child_continuous, child_categorical), 
  strata = "ffq_dfe_na",
  data = ccls_lat_vb_all,
  factorVars = child_categorical
)
table1_child_final <- print(table1_child, quote = F, noSpaces = TRUE,nonnormal = child_continuous,printToggle = TRUE, showAllLevels = TRUE)
table1_child_final <- print(table1_child, quote = F, noSpaces = TRUE,printToggle = TRUE, showAllLevels = TRUE)
########################################################################

###############Modeling################

m1 <- glm(caco_n ~ ffq_dfe_tot_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# m1 <- glm(caco_n ~ ffq_dfe_tot_new_100 + ch_birthyear + ch_birthorder_int  + mo_educ_int.f + income.f + mo_age_birth , data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)

m1 <- glm(caco_n ~ ffq_dfe_food_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# m1 <- glm(caco_n ~ ffq_dfe_tot_new_100 + ch_birthyear + ch_birthorder_int  + mo_educ_int.f + income.f + mo_age_birth , data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)

m1 <- glm(caco_n ~ factor(ffq_dfe_sup_new) + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# m1 <- glm(caco_n ~ factor(ffq_dfe_sup_new)+ ch_birthyear + ch_birthorder_int  + mo_educ_int.f + income + mo_age_birth, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)
m1 <- glm(caco_n ~ sup_folaye_400 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# m1 <- glm(caco_n ~ factor(ffq_dfe_sup_new)+ ch_birthyear + ch_birthorder_int  + mo_educ_int.f + income + mo_age_birth, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)

m1 <- glm(caco_n ~ ffq_h_eat_10 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)


m1 <- glm(caco_n ~ prs_folate_005_neg + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
# m1 <- glm(caco_n ~ prs_folate_005 + ch_birthyear + ch_birthorder_int + mo_educ_int.f + income + mo_age_birth, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
tbl_regression(m1, exponentiate = TRUE)


m1 <- glm(caco_n ~ prs_homo_01 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, 
          data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)

# m1 <- glm(caco_n ~ prs_homo_01  + ch_birthorder_int + mo_educ_int.f + income.f + mo_age_birth, 
#           data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# summary(m1)
# tbl_regression(m1, exponentiate = TRUE)

m1 <- glm(caco_n ~ prs_b12_02 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, 
          data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)

# m1 <- glm(caco_n ~ prs_b12_02 + ch_birthyear  + ch_birthorder_int + mo_educ_int.f + income + mo_age_birth, 
#           data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# summary(m1)
# tbl_regression(m1, exponentiate = TRUE)


m1 <- glm(caco_n ~ as.factor(rs1801133)+ ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, 
          data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)

m1 <- glm(caco_n ~ rs1801133+ ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, 
          data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)
###########################

# m2 <- glm(caco_n ~ prs_folate_005+ffq_dfe_tot_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
m2 <- glm(caco_n ~ prs_folate_005_neg*ffq_dfe_tot_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# m2 <- glm(caco_n ~ prs_folate_005*ffq_dfe_tot_new_100  + ch_birthorder_int+ mo_educ_int.f + income.f + mo_age_birth , data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m2)
tbl_regression(m2, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))


m2 <- glm(caco_n ~  prs_folate_005_neg*ffq_dfe_food_new_100  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# m2 <- glm(caco_n ~  prs_folate_005*ffq_h_eat_10   + ch_birthorder_int+ mo_educ_int.f + income.f + mo_age_birth, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m2)
tbl_regression(m2, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))

m2 <- glm(caco_n ~  prs_folate_005_neg+sup_folaye_400  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
#m2 <- glm(caco_n ~  prs_folate_005*sup_folaye_400  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m2)
tbl_regression(m2, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))

#############
# m2 <- glm(caco_n ~ prs_folate_005 + factor(ffq_dfe_sup_new)  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# m2 <- glm(caco_n ~ prs_folate_005 + factor(ffq_dfe_sup_new) + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# summary(m2)
# tbl_regression(m2, exponentiate = TRUE)

m2 <- glm(caco_n ~ prs_folate_005_neg*factor(ffq_dfe_sup_new)+ ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# m2 <- glm(caco_n ~ prs_folate_005*factor(ffq_dfe_sup_new)  + ch_birthorder_int+ mo_educ_int.f + income + mo_age_birth , data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m2)
anova(m2)
tbl_regression(m2, exponentiate = TRUE)

m2 <- glm(caco_n ~  prs_folate_005_neg + ffq_h_eat_10 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# m2 <- glm(caco_n ~  prs_folate_005*ffq_h_eat_10 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# m2 <- glm(caco_n ~  prs_folate_005 + ffq_h_eat_10   + ch_birthorder_int + mo_educ_int.f + income.f + mo_age_birth, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m2)
tbl_regression(m2, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))


###########################

m3 <- glm(caco_n ~ prs_homo_01 + ffq_dfe_tot_new_100  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2 , data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# m3 <- glm(caco_n ~ prs_homo_01*ffq_dfe_tot_new_100  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2 , data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m3)
tbl_regression(m3, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))

# m3 <- glm(caco_n ~  prs_homo_01 + ffq_dfe_food_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
m3 <- glm(caco_n ~  prs_homo_01*ffq_dfe_food_new_100   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m3)
tbl_regression(m3, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))


m3 <- glm(caco_n ~  prs_homo_01 + sup_folaye_400 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# m3 <- glm(caco_n ~  prs_homo_01*sup_folaye_400   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m3)
tbl_regression(m3, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))

m3 <- glm(caco_n ~ prs_homo_01 + factor(ffq_dfe_sup_new) + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2 , data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# m3 <- glm(caco_n ~ prs_homo_01*factor(ffq_dfe_sup_new) + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2 , data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m3)
tbl_regression(m3, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))
anova(m3)

m3 <- glm(caco_n ~ prs_homo_01 + ffq_h_eat_10 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# m3 <- glm(caco_n ~ prs_homo_01*ffq_h_eat_10 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m3)
tbl_regression(m3, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))


##########


m5 <- glm(caco_n ~ rs1801133+ ffq_dfe_tot_new_100  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
m5 <- glm(caco_n ~ rs1801133 *ffq_dfe_tot_new_100   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m5)
tbl_regression(m5, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))


m5 <- glm(caco_n ~ rs1801133 *ffq_dfe_food_new_100   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m5)
tbl_regression(m5, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))


m4 <- glm(caco_n ~ rs1801133  + factor(ffq_dfe_sup_new)  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
m4 <- glm(caco_n ~ rs1801133  *factor(ffq_dfe_sup_new)   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m4)
tbl_regression(m4, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))
anova(m4)

m4 <- glm(caco_n ~ rs1801133 + sup_folaye_400  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
m4 <- glm(caco_n ~ rs1801133 * sup_folaye_400   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m4)
tbl_regression(m4, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))

m5 <- glm(caco_n ~ rs1801133 + ffq_h_eat_10  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2 , data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
m5 <- glm(caco_n ~ rs1801133*ffq_h_eat_10   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m5)
tbl_regression(m5, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))


##########################
########################################
# m4 <- glm(caco_n ~ prs_b12_02 + ffq_h_eat_10 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# # m4 <- glm(caco_n ~ prs_b12_02 + ffq_h_eat_10  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# summary(m4)
# tbl_regression(m4, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))
# 
# m4 <- glm(caco_n ~ prs_b12_02*ffq_h_eat_10  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# summary(m4)
# tbl_regression(m4, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))
# 
# m4 <- glm(caco_n ~ prs_b12_02 + ffq_dfe_tot_new_100  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# # m4 <- glm(caco_n ~ prs_b12_02 + ffq_dfe_tot_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# summary(m4)
# tbl_regression(m4, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))
# 
# # m4 <- glm(caco_n ~ prs_b12_02*ffq_dfe_tot_new_100  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# # # m4 <- glm(caco_n ~ prs_b12_02*ffq_dfe_tot_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# # summary(m4)
# # tbl_regression(m4, exponentiate = TRUE)
# 
# m4 <- glm(caco_n ~ prs_b12_02 + ffq_dfe_food_new_100  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# m4 <- glm(caco_n ~ prs_b12_02*ffq_dfe_food_new_100  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# summary(m4)
# tbl_regression(m4, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))
# 
# 
# 
# m4 <- glm(caco_n ~ prs_b12_02 + sup_folaye_400  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# # m4 <- glm(caco_n ~ prs_b12_02 + factor(ffq_dfe_sup_new) + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# summary(m4)
# tbl_regression(m4, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))
# 
# m4 <- glm(caco_n ~ prs_b12_02*sup_folaye_400  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# # # m4 <- glm(caco_n ~ prs_b12_02*factor(ffq_dfe_sup_new) + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
# # summary(m4)
# # tbl_regression(m4, exponentiate = TRUE)
# # anova(m4)


##########################

############
# CCLS NLW #
############
ccls_nlw_vb_all_filt <- ccls_nlw_vb_all %>% filter(!is.na(ffq_dfe_tot_new))%>% filter(ch_downs == 0)
table(ccls_nlw_vb_all_filt$caco_char)
# Create Table 1 for maternal and paternal variables
table1_maternal_paternal <- CreateTableOne(
  vars = unique(c(maternal_paternal_continuous, maternal_paternal_categorical)), 
  strata = "caco_char",
  data = ccls_nlw_vb_all_filt,
  factorVars = maternal_paternal_categorical
)
table1_maternal_paternal_final <- print(table1_maternal_paternal, quote = F, noSpaces = TRUE, printToggle = TRUE, showAllLevels = TRUE,nonnormal = maternal_paternal_continuous)
table1_maternal_paternal_ttest_final <- print(table1_maternal_paternal, quote = F, noSpaces = TRUE, printToggle = TRUE, showAllLevels = TRUE)

write.csv(table1_maternal_paternal_final, "/Users/xiangyijin/Desktop/Research/Wiemels/MR/Writeup/Output/table1_ccls_nlw_parental_final_1219.csv")
write.csv(table1_maternal_paternal_ttest_final, "/Users/xiangyijin/Desktop/Research/Wiemels/MR/Writeup/Output/table1_ccls_nlw_parental_test_final_1219.csv")

# Create Table 1 for child-specific variables
table1_child <- CreateTableOne(
  vars = c(child_continuous, child_categorical), 
  strata = "caco_char",
  data = ccls_nlw_vb_all_filt,
  factorVars = child_categorical
)
table1_child_final <- print(table1_child, quote = F, noSpaces = TRUE,nonnormal = child_continuous,printToggle = TRUE, showAllLevels = TRUE)
table1_child_ttest_final <- print(table1_child, quote = F, noSpaces = TRUE,printToggle = TRUE, showAllLevels = TRUE)



###########################
# Create Table 1 for maternal and paternal variables
table1_maternal_paternal <- CreateTableOne(
  vars = unique(c(maternal_paternal_continuous, maternal_paternal_categorical)), 
  strata = "ffq_dfe_na",
  data = ccls_nlw_vb_all,
  factorVars = maternal_paternal_categorical
)
table1_maternal_paternal_final <- print(table1_maternal_paternal, quote = F, noSpaces = TRUE, printToggle = TRUE, showAllLevels = TRUE,nonnormal = maternal_paternal_continuous)
table1_maternal_paternal_ttest_final <- print(table1_maternal_paternal, quote = F, noSpaces = TRUE, printToggle = TRUE, showAllLevels = TRUE)

# Create Table 1 for child-specific variables
table1_child <- CreateTableOne(
  vars = c(child_continuous, child_categorical), 
  strata = "ffq_dfe_na",
  data = ccls_nlw_vb_all,
  factorVars = child_categorical
)
table1_child_final <- print(table1_child, quote = F, noSpaces = TRUE,nonnormal = child_continuous,printToggle = TRUE, showAllLevels = TRUE)
table1_child_ttest_final <- print(table1_child, quote = F, noSpaces = TRUE,printToggle = TRUE, showAllLevels = TRUE)

####
m1 <- glm(caco_n ~ ffq_h_eat_10 + ch_age_category+ch_sex+income.f + mo_age_birth+mo_alc_freq_bef.f+ PC1 + PC2,data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m1 <- glm(caco_n ~ ffq_h_eat_10 + ch_birthyear  + ch_birthorder_int+ mo_educ_int.f + income + mo_age_birth, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)
car::vif(m1)

m1 <- glm(caco_n ~ ffq_dfe_tot_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m1 <- glm(caco_n ~ ffq_dfe_tot_new_100+ ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)

m1 <- glm(caco_n ~ ffq_dfe_food_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m1 <- glm(caco_n ~ ffq_dfe_tot_new_100+ ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)


m1 <- glm(caco_n ~ factor(ffq_dfe_sup_new) + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)
car::vif(m1)

m1 <- glm(caco_n ~ sup_folaye_400 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)


m1 <- glm(caco_n ~ prs_folate_005_neg   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)

m1 <- glm(caco_n ~ prs_homo_01 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)

# m1 <- glm(caco_n ~ prs_b12_02 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2,  data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# summary(m1)
# tbl_regression(m1, exponentiate = TRUE)


m1 <- glm(caco_n ~ as.factor(rs1801133)+ ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)
m1 <- glm(caco_n ~ rs1801133+ ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m1)
tbl_regression(m1, exponentiate = TRUE)


#########
m2 <- glm(caco_n ~ prs_folate_005_neg + ffq_dfe_tot_new_100   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m2 <- glm(caco_n ~ prs_folate_005*ffq_dfe_tot_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2,, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m2)
tbl_regression(m2, exponentiate = TRUE,pvalue_fun = label_style_pvalue(digits = 3))

m2 <- glm(caco_n ~ prs_folate_005_neg +ffq_dfe_food_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m2 <- glm(caco_n ~ prs_folate_005*ffq_dfe_food_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m2)
tbl_regression(m2, exponentiate = TRUE,pvalue_fun = label_style_pvalue(digits = 3))

m2 <- glm(caco_n ~ prs_folate_005_neg +sup_folaye_400 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m2 <- glm(caco_n ~ prs_folate_005*sup_folaye_400 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m2)
tbl_regression(m2, exponentiate = TRUE,pvalue_fun = label_style_pvalue(digits = 3))

m2 <- glm(caco_n ~ prs_folate_005_neg + factor(ffq_dfe_sup_new)  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m2 <- glm(caco_n ~ prs_folate_005*factor(ffq_dfe_sup_new)  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m2)
anova(m2)
tbl_regression(m2, exponentiate = TRUE,pvalue_fun = label_style_pvalue(digits = 3))

m2 <- glm(caco_n ~  prs_folate_005_neg + ffq_h_eat_10  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m2 <- glm(caco_n ~  prs_folate_005*ffq_h_eat_10  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m2)
tbl_regression(m2, exponentiate = TRUE,pvalue_fun = label_style_pvalue(digits = 3))



######
m3 <- glm(caco_n ~ prs_homo_01 + ffq_dfe_tot_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m3 <- glm(caco_n ~ prs_homo_01*ffq_dfe_tot_new_100  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m3)
tbl_regression(m3, exponentiate = TRUE, pvalue_fun = label_style_pvalue(digits = 3))

m3 <- glm(caco_n ~ prs_homo_01 +ffq_dfe_food_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m3 <- glm(caco_n ~ prs_homo_01*ffq_dfe_food_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m3)
tbl_regression(m3, exponentiate = TRUE, pvalue_fun = label_style_pvalue(digits = 3))

m3 <- glm(caco_n ~ prs_homo_01 +sup_folaye_400 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m3 <- glm(caco_n ~ prs_homo_01*sup_folaye_400 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m3)
tbl_regression(m3, exponentiate = TRUE, pvalue_fun = label_style_pvalue(digits = 3))

m3 <- glm(caco_n ~ prs_homo_01 + factor(ffq_dfe_sup_new)  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m3 <- glm(caco_n ~ prs_homo_01*factor(ffq_dfe_sup_new)  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2 , data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m3)
tbl_regression(m3, exponentiate = TRUE)

m3 <- glm(caco_n ~ prs_homo_01 + ffq_h_eat_10  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m3 <- glm(caco_n ~ prs_homo_01*ffq_h_eat_10  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m3)
tbl_regression(m3, exponentiate = TRUE,pvalue_fun = label_style_pvalue(digits = 3))


##########
m5 <- glm(caco_n ~ rs1801133 + ffq_h_eat_10  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2 , data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m5 <- glm(caco_n ~ as.factor(rs1801133)*ffq_h_eat_10   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m5)
tbl_regression(m5, exponentiate = TRUE, pvalue_fun = label_style_pvalue(digits = 3))

m5 <- glm(caco_n ~ rs1801133 + ffq_dfe_tot_new_100  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m5 <- glm(caco_n ~ as.factor(rs1801133) *ffq_dfe_tot_new_100   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m5)
tbl_regression(m5, exponentiate = TRUE, pvalue_fun = label_style_pvalue(digits = 3))

m4 <- glm(caco_n ~ rs1801133 + ffq_dfe_food_new_100  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m4 <- glm(caco_n ~ as.factor(rs1801133) *ffq_dfe_food_new_100   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m4)
tbl_regression(m4, exponentiate = TRUE, pvalue_fun = label_style_pvalue(digits = 3))

m4 <- glm(caco_n ~ rs1801133*ffq_dfe_food_new_100  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
tbl_regression(m4, exponentiate = TRUE, pvalue_fun = label_style_pvalue(digits = 3))

m4 <- glm(caco_n ~ rs1801133 + sup_folaye_400  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m4 <- glm(caco_n ~ as.factor(rs1801133) *sup_folaye_400   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m4)
tbl_regression(m4, exponentiate = TRUE, pvalue_fun = label_style_pvalue(digits = 3))

m4 <- glm(caco_n ~ rs1801133 + factor(ffq_dfe_sup_new)  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
# m4 <- glm(caco_n ~ as.factor(rs1801133)  *factor(ffq_dfe_sup_new)   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m4)
tbl_regression(m4, exponentiate = TRUE, pvalue_fun = label_style_pvalue(digits = 3))
anova(m4)

