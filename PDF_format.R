#
# include libraries
# 
library(tidyverse)
library(pdftools)
library(stringr)
library(dplyr)
#
# create question paper data frame
# 1.1 Read PDF File 
qp1_string <- pdf_text("English_HL_P3_Nov_2019.pdf")
qp1_pages <- str_split(qp_string1, "\n")
qp1_pages 

qp2_string <- pdf_text("Afrikaans_SAL_P1_Nov_2019_Gauteng.pdf")
qp2_pages <- str_split(qp_string2, "\n")
qp2_pages
#DBE_index <- str_which(qp2_pages, "DBE", negate = FALSE)
#DBE_index

qp2_page2 <- qp2_pages[[2]]
qp2_page2[1]
DBE_index <- str_which(qp2_page2, "DBE", negate = FALSE)
DBE_index