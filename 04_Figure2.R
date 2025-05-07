####################################
# Generate figure 3 for regression #
####################################

# Folate PRS * tot folate
m2 <- glm(caco_n ~ prs_folate_005_neg*ffq_dfe_tot_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m2)
summary(ccls_lat_vb_all_filt$ffq_dfe_tot_new_100)
min_est = esticon(m2,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
q1_est = esticon(m2,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.695))
median_est = esticon(m2,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5.666))
q3_est = esticon(m2,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8.697))
max_est = esticon(m2,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,22.287))

m2_nlw <-glm(caco_n ~ prs_folate_005_neg+ffq_dfe_tot_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m2_nlw)
est = esticon(m2_nlw,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# Create a data frame with the estimates
odds_data1_1 <- data.frame(
  ffq_dfe_tot_new_100 = c("Min", "Q1", "Median", "Q3", "Max","Min", "Q1", "Median", "Q3", "Max"),
  group = c(rep("LAT",5),rep("NLW",5)),
  Estimate = c(min_est$estimate, q1_est$estimate, median_est$estimate, q3_est$estimate, max_est$estimate, rep(est$estimate,5)),
  SE = c(min_est$std.error, q1_est$std.error, median_est$std.error, q3_est$std.error, max_est$std.error, rep(est$std.error,5))
) %>%
  mutate(
    Odds_Ratio = exp(Estimate),
    CI_Lower = exp(Estimate - 1.96 * SE),
    CI_Upper = exp(Estimate + 1.96 * SE),
    ffq_dfe_tot_new_100 = factor(ffq_dfe_tot_new_100, 
                                 levels = c("Min", "Q1", "Median", "Q3", "Max"))
  )

# Plot the odds ratios with confidence intervals
f1<-ggplot(odds_data1_1, aes(x = factor(ffq_dfe_tot_new_100), y = Odds_Ratio, color = group)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.5), width = 0.2) +
  scale_color_manual(values = c("NLW" = "#99C2DB", "LAT" = "#0073A1")) +
  geom_hline(yintercept=1, colour="grey", linetype = "dotdash")+
  annotate("text", x = 3, y = 1.9, label = "P[\"int\"] == 0.019", size = 4, hjust = 0, color = "#0073A1", parse = TRUE) +  # subscript
  
  labs(
    x = "Total Folate (DT + SUP)",
    y = "ORs Per 0.05 Unit Increase\n in fPRS",
    color = NULL  # Remove legend title
  ) +
  theme_minimal() +
  scale_y_continuous(
    limits = c(0, 2),           # Set the range of the y-axis
    breaks = seq(0.5, 1.5, by = 0.5)  # Customize tick marks
  ) +
  theme(
    axis.text = element_text(size = 12,color = "black"),      # Increase axis text size
    axis.title  = element_text(size = 12,color = "black"),      # Increase axis text siz
    legend.text = element_text(size = 14,color = "black")
  )


# Folate PRS * tot Folate from food
m2 <- glm(caco_n ~ prs_folate_005_neg*ffq_dfe_food_new_100  + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m2)
# tbl_regression(m2, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))
summary(ccls_lat_vb_all_filt$ffq_dfe_food_new_100)
min_est = esticon(m2,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
q1_est = esticon(m2,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.496))
median_est = esticon(m2,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5.174))
q3_est = esticon(m2,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7.043))
max_est = esticon(m2,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20.184))
summary(m2)
# tbl_regression(m2, exponentiate = TRUE)
m2_nlw <-glm(caco_n ~ prs_folate_005_neg+ffq_dfe_food_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m2_nlw)
est = esticon(m2_nlw,c(0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# Create a data frame with the estimates
odds_data1_2 <- data.frame(
  ffq_dfe_food_new_100 = c("Min", "Q1", "Median", "Q3", "Max","Min", "Q1", "Median", "Q3", "Max"),
  group = c(rep("LAT",5),rep("NLW",5)),
  Estimate = c(min_est$estimate, q1_est$estimate, median_est$estimate, q3_est$estimate, max_est$estimate, rep(est$estimate,5)),
  SE = c(min_est$std.error, q1_est$std.error, median_est$std.error, q3_est$std.error, max_est$std.error, rep(est$std.error,5))
) %>%
  mutate(
    Odds_Ratio = exp(Estimate),
    CI_Lower = exp(Estimate - 1.96 * SE),
    CI_Upper = exp(Estimate + 1.96 * SE),
    ffq_dfe_food_new_100 = factor(ffq_dfe_food_new_100, 
                                  levels = c("Min", "Q1", "Median", "Q3", "Max"))
  )
# Plot the odds ratios with confidence intervals
f2<-ggplot(odds_data1_2, aes(x = factor(ffq_dfe_food_new_100), y = Odds_Ratio, color = group)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.5), width = 0.2) +
  scale_color_manual(values = c("NLW" = "#99C2DB", "LAT" = "#0073A1")) +
  geom_hline(yintercept=1, colour="grey", linetype = "dotdash")+
  annotate("text", x = 3, y = 1.9, label = "P[\"int\"] == 0.006", size = 4, hjust = 0, color = "#0073A1", parse = TRUE) +  # subscript
  
  labs(
    x = "Total Folate from Food",
    y = "ORs Per 0.05 Unit Increase\n in fPRS",
    color = NULL  # Remove legend title
  ) +
  theme_minimal() +
  scale_y_continuous(
    limits = c(0, 2),           # Set the range of the y-axis
    breaks = seq(0.5, 1.5, by = 0.5)  # Customize tick marks
  ) +
  theme(
    axis.text = element_text(size = 12,color = "black"),      # Increase axis text size
    axis.title  = element_text(size = 12,color = "black"),      # Increase axis text siz
    legend.text = element_text(size = 14,color = "black")
  )



##########################
# tHCy PRS * total folate
m3 <- glm(caco_n ~ prs_homo_01+ffq_dfe_tot_new_100   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m3)
# tbl_regression(m3, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))

summary(ccls_lat_vb_all_filt$ffq_dfe_tot_new_100)
min_est = esticon(m3,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
q1_est = esticon(m3,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
median_est = esticon(m3,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
q3_est = esticon(m3,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
max_est = esticon(m3,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

# Create a data frame with the estimates
m3_nlw <-glm(caco_n ~ prs_homo_01+ffq_dfe_tot_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m3_nlw)
est = esticon(m3_nlw,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# Create a data frame with the estimates
odds_data2_1 <- data.frame(
  ffq_dfe_tot_new_100 = c("Min", "Q1", "Median", "Q3", "Max","Min", "Q1", "Median", "Q3", "Max"),
  group = c(rep("LAT",5),rep("NLW",5)),
  Estimate = c(min_est$estimate, q1_est$estimate, median_est$estimate, q3_est$estimate, max_est$estimate, rep(est$estimate,5)),
  SE = c(min_est$std.error, q1_est$std.error, median_est$std.error, q3_est$std.error, max_est$std.error, rep(est$std.error,5))
) %>%
  mutate(
    Odds_Ratio = exp(Estimate),
    CI_Lower = exp(Estimate - 1.96 * SE),
    CI_Upper = exp(Estimate + 1.96 * SE),
    ffq_dfe_tot_new_100 = factor(ffq_dfe_tot_new_100, 
                                 levels = c("Min", "Q1", "Median", "Q3", "Max"))
  )

# Plot the odds ratios with confidence intervals
f3<-ggplot(odds_data2_1, aes(x = factor(ffq_dfe_tot_new_100), y = Odds_Ratio, color = group)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.5), width = 0.2) +
  scale_color_manual(values = c("NLW" = "#99C2DB", "LAT" = "#0073A1")) +
  geom_hline(yintercept=1, colour="grey", linetype = "dotdash")+

  labs(
    x = "Total Folate (DT + SUP)",
    y = "ORs Per 0.1 Unit Increase\n in hPRS",
    color = NULL  # Remove legend title
  ) +
  theme_minimal()  +
  scale_y_continuous(
    limits = c(0.2, 1.6),           # Set the range of the y-axis
    breaks = seq(0.4, 1.6, by = 0.4)  # Customize tick marks
  ) +
  theme(
    axis.text = element_text(size = 12,color = "black"),      # Increase axis text size
    axis.title  = element_text(size = 12,color = "black"),      # Increase axis text siz
    legend.text = element_text(size = 14,color = "black")
  )

###################################
# tHCy PRS * total Folate from food
m3 <- glm(caco_n ~ prs_homo_01*ffq_dfe_food_new_100   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m3)
tbl_regression(m3, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))

summary(ccls_lat_vb_all_filt$ffq_dfe_food_new_100)
min_est = esticon(m3,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
q1_est = esticon(m3,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.496))
median_est = esticon(m3,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5.174))
q3_est = esticon(m3,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7.043))
max_est = esticon(m3,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20.184))

# Create a data frame with the estimates
m3_nlw <-glm(caco_n ~ prs_homo_01+ffq_dfe_food_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m3_nlw)
est = esticon(m3_nlw,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# Create a data frame with the estimates
odds_data2_2 <- data.frame(
  ffq_dfe_tot_new_100 = c("Min", "Q1", "Median", "Q3", "Max","Min", "Q1", "Median", "Q3", "Max"),
  group = c(rep("LAT",5),rep("NLW",5)),
  Estimate = c(min_est$estimate, q1_est$estimate, median_est$estimate, q3_est$estimate, max_est$estimate, rep(est$estimate,5)),
  SE = c(min_est$std.error, q1_est$std.error, median_est$std.error, q3_est$std.error, max_est$std.error, rep(est$std.error,5))
) %>%
  mutate(
    Odds_Ratio = exp(Estimate),
    CI_Lower = exp(Estimate - 1.96 * SE),
    CI_Upper = exp(Estimate + 1.96 * SE),
    ffq_dfe_tot_new_100 = factor(ffq_dfe_tot_new_100, 
                                 levels = c("Min", "Q1", "Median", "Q3", "Max"))
  )

# Plot the odds ratios with confidence intervals
f4<-ggplot(odds_data2_2, aes(x = factor(ffq_dfe_tot_new_100), y = Odds_Ratio, color = group)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.5), width = 0.2) +
  scale_color_manual(values = c("NLW" = "#99C2DB", "LAT" = "#0073A1")) +
  geom_hline(yintercept=1, colour="grey", linetype = "dotdash")+
  annotate("text", x = 3, y = 1.5, label = "P[\"int\"] == 0.008", size = 4, hjust = 0, color = "#0073A1", parse = TRUE) +  # subscript
  
  labs(
    x = "Total Folate from food",
    y = "ORs Per 0.1 Unit Increase\n in hPRS",
    color = NULL  # Remove legend title
  ) +
  theme_minimal()  +
  scale_y_continuous(
    limits = c(0.2, 1.6),           # Set the range of the y-axis
    breaks = seq(0.4, 1.6, by = 0.4)  # Customize tick marks
  ) +
  theme(
    axis.text = element_text(size = 12,color = "black"),      # Increase axis text size
    axis.title  = element_text(size = 12,color = "black"),      # Increase axis text siz
    legend.text = element_text(size = 14,color = "black")
  )


##########################
##########################
# rs1801133 * total folate
m6 <- glm(caco_n ~ rs1801133 *ffq_dfe_tot_new_100   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m6)
summary(ccls_lat_vb_all_filt$ffq_dfe_tot_new_100)
# tbl_regression(m6, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))
min_est = esticon(m6,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
q1_est = esticon(m6,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.695))
median_est = esticon(m6,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5.666))
q3_est = esticon(m6,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8.697))
max_est = esticon(m6,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,22.287))

# Create a data frame with the estimates
m6_nlw <-glm(caco_n ~ rs1801133+ffq_dfe_tot_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m6_nlw)
est = esticon(m3_nlw,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

# Create a data frame with the estimates
# Create a data frame with the estimates
odds_data3_1 <- data.frame(
  ffq_dfe_tot_new_100 = c("Min", "Q1", "Median", "Q3", "Max","Min", "Q1", "Median", "Q3", "Max"),
  group = c(rep("LAT",5),rep("NLW",5)),
  Estimate = c(min_est$estimate, q1_est$estimate, median_est$estimate, q3_est$estimate, max_est$estimate, rep(est$estimate,5)),
  SE = c(min_est$std.error, q1_est$std.error, median_est$std.error, q3_est$std.error, max_est$std.error, rep(est$std.error,5))
) %>%
  mutate(
    Odds_Ratio = exp(Estimate),
    CI_Lower = exp(Estimate - 1.96 * SE),
    CI_Upper = exp(Estimate + 1.96 * SE),
    ffq_dfe_tot_new_100 = factor(ffq_dfe_tot_new_100, 
                                 levels = c("Min", "Q1", "Median", "Q3", "Max"))
  )

# Plot the odds ratios with confidence intervals
f5<-ggplot(odds_data3_1, aes(x = factor(ffq_dfe_tot_new_100), y = Odds_Ratio, color = group)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.5), width = 0.2) +
  scale_color_manual(values = c("NLW" = "#99C2DB", "LAT" = "#0073A1")) +
  geom_hline(yintercept=1, colour="grey", linetype = "dotdash")+
  annotate("text", x = 3, y = 3, label = "P[\"int\"] == 0.016", size = 4, hjust = 0, color = "#0073A1", parse = TRUE) +  # subscript
  labs(
    x = "Total Folate (DT+SUP)",
    y = "ORs Per T Allele Increase\n for rs1801133",
    color = NULL  # Remove legend title
  ) +
  theme_minimal() +
  scale_y_continuous(
    limits = c(0, 3.75),           # Set the range of the y-axis
    breaks = seq(0, 3, by = 1)  # Customize tick marks
  ) +
  theme(
    axis.text = element_text(size = 12,color = "black"),      # Increase axis text size
    axis.title  = element_text(size = 12,color = "black"),      # Increase axis text siz
    legend.text = element_text(size = 14,color = "black")
  )



#######
# rs1801133 * total folate from food
m6 <- glm(caco_n ~ rs1801133 *ffq_dfe_food_new_100   + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_lat_vb_all_filt, family = binomial(link = "logit"))
summary(m6)
summary(ccls_lat_vb_all_filt$ffq_dfe_food_new_100)
# tbl_regression(m6, exponentiate = TRUE,  pvalue_fun = label_style_pvalue(digits = 3))
min_est = esticon(m6,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
q1_est = esticon(m6,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.496))
median_est = esticon(m6,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5.174))
q3_est = esticon(m6,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7.043))
max_est = esticon(m6,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20.184))

# Create a data frame with the estimates
m6_nlw <-glm(caco_n ~ rs1801133+ffq_dfe_food_new_100 + ch_age_category+ch_sex+income.f + mo_age_birth + mo_alc_freq_bef.f+ PC1 + PC2, data = ccls_nlw_vb_all_filt, family = binomial(link = "logit"))
summary(m6_nlw)
est = esticon(m3_nlw,c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

# Create a data frame with the estimates
# Create a data frame with the estimates
odds_data3_2 <- data.frame(
  ffq_dfe_food_new_100 = c("Min", "Q1", "Median", "Q3", "Max","Min", "Q1", "Median", "Q3", "Max"),
  group = c(rep("LAT",5),rep("NLW",5)),
  Estimate = c(min_est$estimate, q1_est$estimate, median_est$estimate, q3_est$estimate, max_est$estimate, rep(est$estimate,5)),
  SE = c(min_est$std.error, q1_est$std.error, median_est$std.error, q3_est$std.error, max_est$std.error, rep(est$std.error,5))
) %>%
  mutate(
    Odds_Ratio = exp(Estimate),
    CI_Lower = exp(Estimate - 1.96 * SE),
    CI_Upper = exp(Estimate + 1.96 * SE),
    ffq_dfe_food_new_100 = factor(ffq_dfe_food_new_100, 
                                  levels = c("Min", "Q1", "Median", "Q3", "Max"))
  )

# Plot the odds ratios with confidence intervals
f6<-ggplot(odds_data3_2, aes(x = factor(ffq_dfe_food_new_100), y = Odds_Ratio, color = group)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.5), width = 0.2) +
  scale_color_manual(values = c("NLW" = "#99C2DB", "LAT" = "#0073A1")) +
  geom_hline(yintercept=1, colour="grey", linetype = "dotdash")+
  annotate("text", x = 3, y = 3, label = "P[\"int\"] == 0.003", size = 4, hjust = 0, color = "#0073A1", parse = TRUE) +  # subscript
  
  labs(
    x = "Total Folate from Food",
    y = "ORs per T Allele Increase\n for rs1801133",
    color = NULL  # Remove legend title
  ) +
  theme_minimal() +
  scale_y_continuous(
    limits = c(0, 3.75),           # Set the range of the y-axis
    breaks = seq(0, 3, by = 1)  # Customize tick marks
  ) +
  theme(
    legend.text = element_text(size = 14,color = "black"),
    axis.text = element_text(size = 12,color = "black"),
    axis.title  = element_text(size = 12,color = "black")     # Increase axis text size
  )

ggarrange(f1,f2,f3,f4,f5,f6,
          common.legend=T,  
          ncol=2,
          nrow = 3,
          legend = "top"
)



