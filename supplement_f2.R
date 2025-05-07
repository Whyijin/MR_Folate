
################################
# Distribution of FFQ variables#
################################

############################
# Test the MAD in CCLS LAT #
############################
mad_lat_data=select(ccls_lat_vb_all_filt,c("ffq_h_eat","ffq_dfe_nat_new","ffq_dfe_fort_new",
                                           "ffq_dfe_food_new","ffq_dfe_tot_new","ffq_dfe_sup_new"))

mad_lat_case_data=select(ccls_lat_vb_all_filt%>% filter(caco_char == "Case"),
                         c("ffq_h_eat","ffq_dfe_nat_new","ffq_dfe_fort_new",
                           "ffq_dfe_food_new","ffq_dfe_tot_new","ffq_dfe_sup_new")) 

mad_lat_control_data=select(ccls_lat_vb_all_filt%>% filter(caco_char == "Control"),
                            c("ffq_h_eat","ffq_dfe_nat_new","ffq_dfe_fort_new",
                              "ffq_dfe_food_new","ffq_dfe_tot_new","ffq_dfe_sup_new")) 

# Combine MAD results into one data frame
mad_lat_combined <- rbind(
  All = sapply(mad_lat_data, mad),
  Case = sapply(mad_lat_case_data, mad),
  Control = sapply(mad_lat_control_data, mad)
)

# Transpose to get variables as rows
mad_lat_combined <- t(mad_lat_combined)

############################
# Test the MAD in CCLS NLW #
############################
mad_nlw_data=select(ccls_nlw_vb_all_filt,c("ffq_h_eat","ffq_dfe_nat_new","ffq_dfe_fort_new",
                                           "ffq_dfe_food_new","ffq_dfe_tot_new","ffq_dfe_sup_new"))

mad_nlw_case_data=select(ccls_nlw_vb_all_filt%>% filter(caco_char == "Case"),
                         c("ffq_h_eat","ffq_dfe_nat_new","ffq_dfe_fort_new",
                           "ffq_dfe_food_new","ffq_dfe_tot_new","ffq_dfe_sup_new")) 

mad_nlw_control_data=select(ccls_nlw_vb_all_filt%>% filter(caco_char == "Control"),
                            c("ffq_h_eat","ffq_dfe_nat_new","ffq_dfe_fort_new",
                              "ffq_dfe_food_new","ffq_dfe_tot_new","ffq_dfe_sup_new")) 

# Combine MAD results into one data frame
mad_nlw_combined <- rbind(
  All = sapply(mad_nlw_data, mad),
  Case = sapply(mad_nlw_case_data, mad),
  Control = sapply(mad_nlw_control_data, mad)
)

# Transpose to get variables as rows
mad_nlw_combined <- t(mad_nlw_combined)
mad_data <- rbind(mad_lat_data %>% mutate(race = "LAT"),
                  mad_nlw_data %>% mutate(race = "NLW"))
p1<-ggdensity(mad_data, x = "ffq_h_eat",
              add = "median", rug = TRUE,
              color = "race", 
              # fill = "caco",
              alpha = 0.1,
              xlab = "Health Eating Index",
              ylab = "Density",
              palette = c("#0073A1", "#99C2DB"))+ 
  theme(legend.title = element_blank())

p2<-ggdensity(mad_data, x = "ffq_dfe_nat_new",
              add = "median", rug = TRUE,
              color = "race", 
              # fill = "caco",
              alpha = 0.1,
              xlab = "Natural Folate from Food (DFE)",
              ylab = "Density",
              palette = c("#0073A1", "#99C2DB"))+ 
  theme(legend.title = element_blank())+ 
  xlim(0, 1000)  # Extend x-axis range to show full tick marks

p3<-ggdensity(mad_data, x = "ffq_dfe_fort_new",
              add = "median", rug = TRUE,
              color = "race", 
              # fill = "caco",
              alpha = 0.1,
              xlab = "Fortified Folate from Food (DFE)",
              ylab = "Density",
              palette = c("#0073A1", "#99C2DB"))+ 
  theme(legend.title = element_blank())

p4<-ggdensity(mad_data, x = "ffq_dfe_food_new",
              add = "median", rug = TRUE,
              color = "race", 
              # fill = "caco",
              alpha = 0.1,
              xlab = "Total Folate from Food (DFE)",
              ylab = "Density",
              palette = c("#0073A1", "#99C2DB"))+ 
  theme(legend.title = element_blank())

p5<-ggdensity(mad_data, x = "ffq_dfe_tot_new",
              add = "median", rug = TRUE,
              color = "race", 
              # fill = "caco",
              alpha = 0.1,
              xlab = "Total Folate (DFE)",
              ylab = "Density",
              palette = c("#0073A1", "#99C2DB"))+ 
  theme(legend.title = element_blank())


# Plot
p6<-mad_data %>% group_by(race,ffq_dfe_sup_new) %>%
  summarise(Count = n()) %>%
  mutate(ffq_dfe_sup_new = factor(ffq_dfe_sup_new,
                                  levels = c(0, 194.31, 485.86, 680),
                                  labels = c("0", "194.31", "485.86", "680")))%>%
  ggplot(aes(x = ffq_dfe_sup_new, y = Count, fill = race)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("LAT" = "#0073A1", "NLW" = "#99C2DB")) + # Colors like your image
  labs(x = "Folate from Supplement (DFE)", y = "Frequency") +
  theme_minimal()+
  theme(
    legend.title = element_blank(),
    # panel.grid = element_blank(),
    axis.title.y = element_text(size = 12,colour = "black"),
    axis.text = element_text(size = 12,colour = "black"),
    axis.title.x= element_text(size = 12,colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

# Arrange with shared legend at bottom
ggarrange(p1, p2, p3, p4, p5, p6,
          ncol = 3, nrow = 2,
          common.legend = TRUE,
          legend = "top")

