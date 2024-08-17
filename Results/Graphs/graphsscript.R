PFSm2 <- survfit(PFS ~  final_data2$RISS_stadio_esordio, data = final_data2)


ggsurv <- ggsurvplot(
  PFSm,                     # survfit object with calculated statistics.
  data = final_data2,             # data used to fit survival curves.
  risk.table = TRUE,
  surv.median.line = "hv",# show risk table.
  pval = F,             # show p-value of log-rank test.
  conf.int = F,         # show confidence intervals for 
  # point estimates of survival curves.
  palette = c("Okabe-Ito"),
  xlim = c(0,60),         # present narrower X axis, but not affect
  # survival estimates.
  xlab = "PFS - Time in months",   # customize X axis label.
  break.time.by = 6,     # break X axis in time intervals by 500.
  ggtheme = theme_light(),
  tile="Median PFS",# customize plot and risk table with a theme.
  risk.table.y.text.col = T,# colour risk table text annotations.
  #risk.table.height = 0.25, # the height of the risk table
  risk.table.y.text = F,# show bars instead of names in text annotations
)
ggsurv






bestresp_DaraVd_M_VGPR
age_65_dara
EMD_PD
ECOG_esordio
pd_PRO_g3500
amp1q_PD
t14_16_esordio
del17_esordio
MoF
pd_LDH_g248


library(survminer)
library(ggplot2)

# Variables to loop through
variables <- c("bestresp_DaraVd_M_VGPR", "age_65_dara", "EMD_PD", "ECOG_esordio", 
               "pd_PRO_g3500","pd_PRO_g1000", "amp1q_PD", "t14_16_esordio", "del17_esordio", 
               "MoF", "pd_LDH_g248")


k<-list(
  PFSm1 <- survfit(PFS ~ bestresp_DaraVd_M_VGPR, data = final_data2),
  PFSm2 <- survfit(PFS ~ age_65_dara, data = final_data2),
  PFSm3 <- survfit(PFS ~ EMD_PD, data = final_data2),
  PFSm4 <- survfit(PFS ~ ECOG_esordio, data = final_data2),
  PFSm5 <- survfit(PFS ~ pd_PRO_g3500, data = final_data2),
  PFSm6 <- survfit(PFS ~ pd_PRO_g1000, data = final_data2),
  PFSm6 <- survfit(PFS ~ amp1q_PD, data = final_data2),
  PFSm8 <- survfit(PFS ~ t14_16_esordio, data = final_data2),
  PFSm9 <- survfit(PFS ~ del17_esordio, data = final_data2),
  PFSm10 <- survfit(PFS ~ MoF, data = final_data2),
  PFSm11 <- survfit(PFS ~ pd_LDH_g248, data = final_data2)
)
  
  
  for (i in k) {
  # Create the plot
  ggsurv <- ggsurvplot(
    i,
    data = final_data2,
    risk.table = TRUE,
    surv.median.line = "hv",
    pval = TRUE,
    conf.int = FALSE,
    palette = c("Okabe-Ito"),
    xlim = c(0, 60),
    xlab = "PFS - Time in months",
    break.time.by = 6,
    ggtheme = theme_light(),
    risk.table.y.text.col = TRUE,
    risk.table.y.text = FALSE
  )
  
  print(ggsurv$plot)
  }
  

ggsurv <- ggsurvplot(
  PFSm1,                     # survfit object with calculated statistics.
  data = final_data2,             # data used to fit survival curves.
  risk.table = TRUE,
  #surv.median.line = "hv",# show risk table.
  pval = T,             # show p-value of log-rank test.
  conf.int = F,         # show confidence intervals for 
  # point estimates of survival curves.
  palette = c("Okabe-Ito"),
  xlim = c(0,60),         # present narrower X axis, but not affect
  # survival estimates.
  xlab = "PFS - Time in months",   # customize X axis label.
  break.time.by = 6,     # break X axis in time intervals by 500.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table.y.text.col = T,# colour risk table text annotations.
  #risk.table.height = 0.25, # the height of the risk table
  risk.table.y.text = F,# show bars instead of names in text annotations
)

ggsurv <- ggsurv + 
  annotate("text", x = Inf, y = Inf, label = paste("p =", format.pval(PFSm1$pval)), 
           hjust = 1, vjust = 1, size = 4)
ggsurv




bestresp_DaraVd_M_VGPR
age_65_dara
EMD_PD
ECOG_esordio
pd_PRO_g3500
amp1q_PD
t14_16_esordio
del17_esordio
MoF
pd_LDH_g248


PFSm1 <- survfit(PFS ~ bestresp_DaraVd_M_VGPR, data = final_data2),
PFSm2 <- survfit(PFS ~ age_65_dara, data = final_data2),
PFSm3 <- survfit(PFS ~ EMD_PD, data = final_data2),
PFSm4 <- survfit(PFS ~ es_ECOG_p2, data = final_data2)
PFSm5 <- survfit(PFS ~ pd_PRO_g3500, data = final_data2),
PFSm7 <- survfit(PFS ~ pd_PRO_g1000, data = final_data2),
PFSm6 <- survfit(PFS ~ amp1q_PD, data = final_data2),
PFSm8 <- survfit(PFS ~ t14_16_esordio, data = final_data2),
PFSm9 <- survfit(PFS ~ del17_esordio, data = final_data2),
PFSm10 <- survfit(PFS ~ MoF, data = final_data2),
PFSm11 <- survfit(PFS ~ pd_LDH_g248, data = final_data2)
PFSm12 <- survfit(PFS ~ final_data2$RISS_stadio_esordio, data = final_data2)

PFSm13 <- survfit(PFS ~ final_data2$consol_postASCT_linea1, data = final_data2)


ggsurvplot(PFSm1, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 8,
           legend.labs = c("< VGPR", "≥ VGPR"),title="Best Response DaraVD")

?ggsurvplot()

ggsurvplot(PFSm2, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 8,
           legend.labs = c("< 65", "≥ 65"),title="Age at progression"
           )
 
ggsurvplot(PFSm3, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 8,
           legend.labs = c("Not Present", "Present"),title="Extramedullary disease (EMD)"
)
ggsurvplot(PFSm4, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 8,
           legend.labs = c("Not Present", "Present"),title="Age at progression"
)
ggsurvplot(PFSm4, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 8,
           legend.labs = c("< 2", "≥ 2"), title="ECOG classification"
)
ggsurvplot(PFSm5, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 8,
           legend.labs = c("< 3,500", "≥ 3,500"),title="Urinary Proteins (mg/day)"
)
ggsurvplot(PFSm6, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 8,
           legend.labs = c("Not Present", "Present"),title="FISH:amp1q"
)
ggsurvplot(PFSm7, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 8,
           legend.labs = c("< 1000", "≥ 1000"), title="Urinary Proteins (mg/day)"
)
ggsurvplot(PFSm8, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 8,
           legend.labs = c("Not Present", "Present"),title="FISH:t14_16"
)
ggsurvplot(PFSm9, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 8,
           legend.labs = c("Not Present", "Present"),title="FISH:del17"
)
ggsurvplot(PFSm10, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 8,
           legend.labs = c("Female", "Male"),title="Gender"
)
ggsurvplot(PFSm11, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 8,
           legend.labs = c("< 248", "≥ 248"),title="LDH (U/L)"
)


ggsurvplot(PFSm12, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 8,
           legend.labs = c("1", "2","3" ),title="Risk Classification (R-ISS)"
)


ggsurvplot(PFSm13, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 8,
           legend.labs = c("No", "Yes"),title="Consolidation Period Post ASCT"
)

osm <- survfit(OS ~  1, data = final_data2)

ggsurvplot(PFSm, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 6,
           legend.labs = c("Progression-free survival"),title="Median Survival PFS",
           surv.median.line = "hv", conf.int = F, xlim = c(0,60)
           
)

par(mfrow = c(4, 2))

PFSm <- survfit(PFS ~  pd_BM_g60, data = final_data2)

ggsurvplot(PFSm, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 6,
           legend.labs = c("<60%","≥ 60%"),title="Plasma cells in the bone marrow", 
           conf.int = F, xlim = c(0,60)
           
)


PFSm <- survfit(PFS ~  HR_DARA, data = final_data2)

ggsurvplot(PFSm, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 6,
           legend.labs = c("Low","High"),title="Citogenetic Risk",
           #surv.median.line = "hv", 
           conf.int = F, xlim = c(0,60)
           
)


PFSm <- survfit(PFS ~  pd_CLEARENCE_g50, data = final_data2)

ggsurvplot(PFSm, data = final_data2, risk.table = T, pval = T,
           palette = c("Okabe-Ito"), xlab = "PFS - Time in months",
           risk.table.y.text = F, break.time.by = 6,
           legend.labs = c("<50","≥ 50"),title="Clearence of the Creatinine (ml/min)",
           #surv.median.line = "hv", 
           conf.int = F, xlim = c(0,60)
           
)
