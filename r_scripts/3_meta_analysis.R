#################################
# "Meta Analysis"
#author: "Catharina Latka"
#################################

################################################################################
#load data and packages
################################################################################
options(scipen=999)
#load packages
library(glue)
library(tidyverse)
library(readxl)
library(gtsummary)
library(fixest) 
library(lme4)
library(modelsummary)
library(gt)
library(webshot2)

#read data
data<-read_csv(glue("data/processed/data.csv"))


################################################################################
#prepare data
################################################################################

data<-data%>%filter(!(is.na(first_year)))
data<-data%>%filter(!(is.na(relative_SE_WTP)))



#remove some unnecessary variables
data<-data%>%dplyr::select(-article,-product,-dependent_variable,-consumer_group_paper,-unit,-reference_product)
avg_data<-data%>%filter(consumer_type=="average")

avg_data<-tibble::rowid_to_column(avg_data, "obs_id")




avg_data<-avg_data%>%
  group_by(reference)%>%
  mutate(group_id=row_number(),
         max_id=max(group_id))%>%
  ungroup()%>%
  mutate(group_id=as.character(group_id),group_id=if_else(max_id==1,"",paste0("_",group_id)))%>%
  mutate(study_estimate=paste0(reference,group_id))

avg_data<-avg_data[order(avg_data$relative_difference,decreasing = TRUE),]



avg_data$continent <- factor(avg_data$continent)
levels(avg_data$continent)

avg_data$continent <- relevel(avg_data$continent, ref = "Europe")
levels(avg_data$continent)
#now Europe will drop out



################################################################################
#run meta regression
################################################################################

library(metafor)

# Fit a random-effects model
#single level model
res <- rma(yi = relative_difference, sei = relative_SE_WTP,
           mods = ~ 1,
           method = "REML",
           data=avg_data)
res



#check for outliers based on cook's distances and studentized residuals
inf <- influence(res)
print(inf)
plot(inf)
which(inf$is.infl)
remove<-which(inf$is.infl)
data_clean <- avg_data[-c(remove), ]

res_clean <- rma(yi = relative_difference, sei = relative_SE_WTP,
           mods = ~ 1,
           method = "REML",
           data=data_clean)
res_clean


avg_data<-data_clean

#check for publication bias and run version where a visual outlier is removed
tiff("data/output/figures/funnel1_plot.tiff", width = 6, height = 6, units = "in", res = 300)
# Create the funnel plot
funnel(res_clean, las=1)
# Close the device
dev.off()

regtest(res_clean)
#z = -1.6280, p = 0.1035
forest(res_clean)

#remove -5.7 estimate as outlier and rerun
avg_data_no_outlier<-avg_data%>%filter(relative_SE_WTP<1)
res_no_outlier <- rma(yi = relative_difference, sei = relative_SE_WTP, method = "REML",data=avg_data_no_outlier)
res_no_outlier

tiff("data/output/figures/funnel2_plot.tiff", width = 6, height = 6, units = "in", res = 300)
# Create the forest plot
funnel(res_no_outlier, las=1)
# Close the device
dev.off()
regtest(res_no_outlier)
#   z =  0.3512, p = 0.7255


#let's reproduce res with mv model
avg_data$vi<-(avg_data$relative_SE_WTP)^2
V<-diag(avg_data$vi)
avg_data$V<-V
R<-avg_data$study_ID
avg_data$R<-R


#if we add R|obs_id to random, we get same tau sq
res2 <- rma.mv(yi = relative_difference, V = V,random=~R |obs_id, 
               mods = ~ 1,
               data = avg_data,
               method = "REML")
res2

#center year - 0=2002 (earliest year)
avg_data$year_centered<-avg_data$first_year-2002

##add all moderators - full model

res_all <- rma(yi = relative_difference, sei = relative_SE_WTP,
            mods =~ product_type+innovativeness+continent+year_centered+choice_experiment+health+sustainability+animal_welfare
            ,
            data= avg_data,
            method = "REML")

#test multicollinearity
multicoll<-as.data.frame(vif(res_all))%>%dplyr::select(-coefs,-m)%>%rownames_to_column(var = "variable")%>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) 



gt_table <- gt(multicoll)
gtsave(gt_table, "data/output/tables/multicoll.docx")

#reduced models with single moderators
res2 <- rma(yi = relative_difference, sei = relative_SE_WTP,
           mods =~ product_type-1,
           data= avg_data,
           method = "REML")


res3 <- rma(yi = relative_difference, sei = relative_SE_WTP,
            mods =~ innovativeness,
            data= avg_data,
            method = "REML")

res4 <- rma(yi = relative_difference, sei = relative_SE_WTP,
            mods =~ continent-1,
            data= avg_data,
            method = "REML")


res5 <- rma(yi = relative_difference, sei = relative_SE_WTP,
            mods =~ year_centered,
            data= avg_data,
            method = "REML")

#further models for appendix
res6 <- rma(yi = relative_difference, sei = relative_SE_WTP,
            mods =~choice_experiment,
            data= avg_data,
            method = "REML")


res7 <- rma(yi = relative_difference, sei = relative_SE_WTP,
            mods =~ health,
            data= avg_data,
            method = "REML")

res8 <- rma(yi = relative_difference, sei = relative_SE_WTP,
            mods =~ sustainability,
            data= avg_data,
            method = "REML")

res9 <- rma(yi = relative_difference, sei = relative_SE_WTP,
            mods =~ animal_welfare,
            data= avg_data,
            method = "REML")




###try separate models for neg and pos wtp
avg_data<-avg_data%>%mutate(pos_wtp=if_else(relative_difference>0,"pos","neg"))

pos_data<-avg_data%>%filter(pos_wtp=="pos")
neg_data<-avg_data%>%filter(pos_wtp=="neg")

res_pos <- rma(yi = relative_difference, sei = relative_SE_WTP,
                mods =~ 1,
               data= pos_data,
                method = "REML")


res2_pos <- rma(yi = relative_difference, sei = relative_SE_WTP,
            mods =~ product_type-1,
            data= pos_data,
            method = "REML")

res3_pos <- rma(yi = relative_difference, sei = relative_SE_WTP,
                mods =~ innovativeness,
                data= pos_data,
                method = "REML")


res4_pos <- rma(yi = relative_difference, sei = relative_SE_WTP,
                mods =~ continent-1,
                data= pos_data,
                method = "REML")


res5_pos <- rma(yi = relative_difference, sei = relative_SE_WTP,
                mods =~ year_centered,
                data= pos_data,
                method = "REML")


res_neg <- rma(yi = relative_difference, sei = relative_SE_WTP,
               mods =~ 1,
               data= neg_data,
               method = "REML")


res2_neg <- rma(yi = relative_difference, sei = relative_SE_WTP,
                mods =~ product_type-1,
                data= neg_data,
                method = "REML")

res3_neg <- rma(yi = relative_difference, sei = relative_SE_WTP,
                mods =~ innovativeness,
                data= neg_data,
                method = "REML")

res4_neg <- rma(yi = relative_difference, sei = relative_SE_WTP,
                mods =~ continent-1,
                data= neg_data,
                method = "REML")

res5_neg <- rma(yi = relative_difference, sei = relative_SE_WTP,
                mods =~ year_centered,
                data= neg_data,
                method = "REML")

#alternative: interaction instead of seperate models also an option...
res_test <- rma(yi = relative_difference, sei = relative_SE_WTP,
               mods =~ pos_wtp,
               data= avg_data,
               method = "REML")


res2_test2 <- rma(yi = relative_difference, sei = relative_SE_WTP,
                mods =~ (product_type-1):pos_wtp,
                data= avg_data,
                method = "REML")
#similar results to pos-neg models


#############################################
#make output for paper
#############################################
#simple model and outlier test, and full model to check for multicollinearity
models <- list(
  "Random effects (RE) with outliers" = res,
  "RE - outlier (cook's dist)" = res_clean,
  "RE - outlier (funnel plot)" = res_no_outlier,
  "RE + moderators" = res_all
)

add_df0<-data.frame(variable=c("Tau²","Cochran's Q","I² (%)"),
                   res=c(res$tau2,res$QE,res$I2),
                   res_clean=c(res_clean$tau2,res_clean$QE,res_clean$I2),
                   res_no_outlier=c(res_no_outlier$tau2,res_no_outlier$QE,res_no_outlier$I2),
                   res_all=c(res_all$tau2,res_all$QE,res_all$I2)
)


modelsummary(models,output = "data/output/results/meta_analysis_results0.docx",
             title = "Random-Effects Meta-Analysis Results",
             fmt = 2 , 
             estimate  = "{estimate}{stars}",
             add_rows = add_df0,
             gof_omit = "i2"
)



#moderator models in main paper
models <- list(
  "Random effects (RE)" = res_clean,
  "RE + product type" = res2,
  "RE + innovativeness" = res3,
  "RE + continent" = res4,
  "RE + year" = res5
  )



#sigma_df<-data.frame("σ²","-",res1$sigma2,res2$sigma2)
add_df<-data.frame(variable=c("Tau²","Cochran's Q","I² (%)"),
                   res_clean=c(res_clean$tau2,res_clean$QE,res_clean$I2),
                   res2=c(res2$tau2,res2$QE,res2$I2),
                   res3=c(res3$tau2,res3$QE,res3$I2),
                   res4=c(res4$tau2,res4$QE,res4$I2),
                   res5=c(res5$tau2,res5$QE,res5$I2)
                   )

##todo: add more stats (all needed to discuss the heterogeneity tests)

modelsummary(models,output = "data/output/results/meta_analysis_results.docx",
             title = "Random-Effects Meta-Analysis Results with Moderators",
             fmt = 2 , 
             estimate  = "{estimate}{stars}",
             add_rows = add_df,
             gof_omit = "i2"
)


######for positive and negative models
models2 <- list(
  "pos RE" = res_pos,
  "neg RE" = res_neg,
  "pos RE + product type" = res2_pos,
  "neg RE + product type" = res2_neg,
  "pos RE + innovativeness" = res3_pos,
  "neg RE + innovativeness" = res3_neg,
  "pos RE + continent" = res4_pos,
  "neg RE + continent" = res4_neg,
  "pos RE + year" = res5_pos,
  "neg RE + year" = res5_neg
)


add_df2<-data.frame(variable=c("Tau²","Cochran's Q","I² (%)"),
                   res_pos=c(res_pos$tau2,res_pos$QE,res_pos$I2),
                   res_neg=c(res_neg$tau2,res_neg$QE,res_neg$I2),
                   res2_pos=c(res2_pos$tau2,res2_pos$QE,res2_pos$I2),
                   res2_neg=c(res2_neg$tau2,res2_neg$QE,res2_neg$I2),
                   res3_pos=c(res3_pos$tau2,res3_pos$QE,res3_pos$I2),
                   res3_neg=c(res3_neg$tau2,res3_neg$QE,res3_neg$I2),
                   res4_pos=c(res4_pos$tau2,res4_pos$QE,res4_pos$I2),
                   res4_neg=c(res4_neg$tau2,res4_neg$QE,res4_neg$I2),
                   res5_pos=c(res5_pos$tau2,res5_pos$QE,res5_pos$I2),
                   res5_neg=c(res5_neg$tau2,res5_neg$QE,res5_neg$I2)
)


modelsummary(models2,output = "data/output/results/meta_analysis_results_posneg.docx",
             title = "Random-Effects Meta-Analysis Results positive vs. negative estimates",
             fmt = 2 , # 
             estimate  = "{estimate}{stars}",
             add_rows = add_df2,
             gof_omit = "i2"
)

#for additional models in appendix
models <- list(
  "Random effects (RE)" = res_clean,
  "RE + CE" = res6,
  "RE + health" = res7,
  "RE + sustainability" = res8,
  "RE + animal welfare" = res9
)

add_dfapp<-data.frame(variable=c("Tau²","Cochran's Q","I² (%)"),
                      res_clean=c(res_clean$tau2,res_clean$QE,res_clean$I2),
                    res6=c(res6$tau2,res6$QE,res6$I2),
                    res7=c(res7$tau2,res7$QE,res7$I2),
                    res8=c(res8$tau2,res8$QE,res8$I2),
                    res9=c(res9$tau2,res9$QE,res9$I2)
                    
)


modelsummary(models,output = "data/output/results/meta_analysis_results_additional.docx",
             title = "Random-Effects Meta-Analysis Results",
             fmt = 2 , 
             estimate  = "{estimate}{stars}",
             add_rows = add_dfapp,
             gof_omit = "i2"
             
)


