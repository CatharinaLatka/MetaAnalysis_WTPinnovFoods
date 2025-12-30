#################################
# "Load and clean input data"
#author: "Catharina Latka"
#################################

#load packages
library(glue)
library(tidyverse)
library(readxl)

#read data
input_data <- read_excel(glue("data/input/Supplementary_data_meta_analysis.xlsx"),sheet="Data_extraction")
   
#clean data
data<-input_data%>%
  dplyr::mutate(relative_difference=as.numeric(`relative_difference`),
                relative_SE_WTP=as.numeric(`relative_SE_WTP`),
                group_share=as.numeric(`group_share`),
                first_year=as.numeric(`first_year`)
                )%>%
  drop_na(relative_difference)


colnames(data)


data<-data%>%dplyr::select(-study_type)

#write output file
write_csv(data,glue("data/processed/data.csv"))

rm(list=ls())
gc()
