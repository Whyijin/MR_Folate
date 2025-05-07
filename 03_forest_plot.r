# Generate Figure 1 Forest Plot
# Load required package
if (!requireNamespace("forestplot", quietly = TRUE)) {
  install.packages("forestplot")
}
library(forestplot)

# Take the output from the MR analysis
data_rc <- mr_result_summary %>% 
  arrange(id.exposure) %>% 
  filter(!grepl("^rs", SNP)) %>%
  filter(Q_pval <0.05 |
           SNP == "All - Inverse variance weighted (fixed effects)"|
           SNP == "All - Wald ratio") %>%
  mutate(OR = exp(b),
         LowerCI = exp(b - 1.96*se),
         UpperCI = exp(b + 1.96*se)) %>% 
  select(exposure,id.exposure,count_snp,SNP,OR,LowerCI,UpperCI,Q_pval,p)


# Prepare the table text with combined labels and statistics
tabletext <- cbind(
  c(data_rc$Label), # Labels
  c(data_rc$count_snp), # Used SNPs
  c(data_rc$OR_95CI), # OR and CI
  c(round(data_rc$p,3)) # P-values
)

# Create the forest plot
data_rc %>%
  mutate(pvalue = round(p,3))%>%
forestplot(
  labeltext = c(Label,count_snp,OR_95CI,pvalue),
  mean = OR, # Mean values for the plot
  lower = LowerCI, # Lower confidence intervals
  upper = UpperCI, # Upper confidence intervals
  xlog = TRUE, # Logarithmic x-axis
  is.summary = c(TRUE, FALSE, FALSE, FALSE,FALSE, TRUE, TRUE,TRUE,
                 TRUE, FALSE, FALSE, FALSE,FALSE, TRUE, TRUE,TRUE,TRUE,TRUE,
                 TRUE, FALSE, FALSE, FALSE,FALSE, FALSE, TRUE,TRUE,TRUE,
                 TRUE, FALSE, FALSE, FALSE,FALSE, TRUE, TRUE,TRUE),
  col = fpColors(
    box = "#0073A1",     # A deeper blue that complements the Light Sky Blue background
    line = "#99C2DB",    # A softer, pastel version of blue to blend with the background
    summary = "#0073A1"  # Match the summary color with the box for consistency
  ),
  xlab = "Odds Ratio (95% CI)", # X-axis label
  txt_gp = fpTxtGp(
                   # label = gpar(fontsize = 13, fontface = "plain"),
    label = gpar(fontsize = 12, fontface = "plain"),
    summary=gpar(fontsize = 11, fontface = "plain"),
    ticks=gpar(fontsize = 20, fontface = "bold"),
    xlab = gpar(fontsize = 20, fontface = "bold")), # Text properties

  xticks = c(0.25,0.40,0.65,1.20, 1.65), # Tick marks on x-axis
  boxsize = 0.2,
  clip = c(0.9, 1.8), 
  vertices = TRUE,
  new_page = TRUE,
  shapes_gp = fpShapesGp(default = gpar(lwd = 2.5))
)%>%
  fp_add_header(Label = "Study",
                # Only the header bold
                count_snp = "# of SNPs",
                OR_95CI = "OR (95% CI)",
                pvalue = "P-value") %>%
  fp_add_lines(h_10 = gpar(lwd = 1, col = "#999999"),
               h_20 = gpar(lwd = 1, col = "#999999"),
               h_29 = gpar(lwd = 1, col = "#999999"),
               h_37 = gpar(lwd = 1, col = "#999999"))%>%
  fp_set_zebra_style(
    odd = "#E1F5FE",  # Light Sky Blue for odd rows
    even = "#F5FFFA"  # Mint Cream for even rows
  )



# Prepare the table text with combined labels and statistics
tabletext <- cbind(
  c(data_rc$Label), # Labels
  c(data_rc$count_snp), # Used SNPs
  c(data_rc$OR_95CI), # OR and CI
  c(round(data_rc$p,3)) # P-values
)[1:18,]
# Create the forest plot
data_rc %>% 
  slice(1:18)%>%
  mutate(pvalue = round(p,3))%>%
  forestplot(
    labeltext = c(Label,count_snp,OR_95CI,pvalue),
    mean = OR, # Mean values for the plot
    lower = LowerCI, # Lower confidence intervals
    upper = UpperCI, # Upper confidence intervals
    xlog = TRUE, # Logarithmic x-axis
    is.summary = c(TRUE, FALSE, FALSE, FALSE,FALSE, TRUE, TRUE,TRUE,
                   TRUE, FALSE, FALSE, FALSE,FALSE, TRUE, TRUE,TRUE,TRUE,TRUE),
    col = fpColors(
      box = "#0073A1",     # A deeper blue that complements the Light Sky Blue background
      line = "#99C2DB",    # A softer, pastel version of blue to blend with the background
      summary = "#0073A1"  # Match the summary color with the box for consistency
    ),
    xlab = "Odds Ratio (95% CI)", # X-axis label
    txt_gp = fpTxtGp(
      # label = gpar(fontsize = 13, fontface = "plain"),
      label = gpar(fontsize = 12, fontface = "plain"),
      summary=gpar(fontsize = 11, fontface = "plain"),
      ticks=gpar(fontsize = 20, fontface = "bold"),
      xlab = gpar(fontsize = 20, fontface = "bold")), # Text properties
    
    xticks = c(0.25,0.40,0.65,1.20, 1.65), # Tick marks on x-axis
    boxsize = 0.2,
    clip = c(0.9, 1.8), 
    vertices = TRUE,
    new_page = TRUE,
    shapes_gp = fpShapesGp(default = gpar(lwd = 2.5))
  )%>%
  fp_add_header(Label = "Study",
                # Only the header bold
                count_snp = "# of SNPs",
                OR_95CI = "OR (95% CI)",
                pvalue = "P-value") %>%
  fp_add_lines(h_10 = gpar(lwd = 1, col = "#999999"),
               h_20 = gpar(lwd = 1, col = "#999999"))%>%
  fp_set_zebra_style(
    odd = "#E1F5FE",  # Light Sky Blue for odd rows
    even = "#F5FFFA"  # Mint Cream for even rows
  )
