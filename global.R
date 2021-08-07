library(data.table)
library(shiny)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(shinyWidgets)
library(RColorBrewer)

#read cleaned datasets
data_cleaned_2015 = read.csv('data/data_cleaned_2015.csv')
data_cleaned_2021 = read.csv('data/data_cleaned_2021.csv')
