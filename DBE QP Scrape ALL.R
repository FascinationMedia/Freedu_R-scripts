##############################################################################################################
# 000) Get the required R libraries 
# 050) Set url of website to be scraped 
# 100) Get the Page Title 
# 200) Get the links to the PDFS for all subjects 
#       2a) filter to links for subject only
# 300) Get the URL text for all the links
# 400) Get the module numbers from each link
# 500) build subject lookup table 
# 600) merge links df and subject df
#############################################################################################################
#
#000)
# Required libraries 
#
library(tidyverse)
library(rvest)
library(stringr)
library(dplyr)
#
# DBE Past paper home URL
DBE_URL_Past_Paper_home_URL <- "https://www.education.gov.za/Curriculum/NationalSeniorCertificate(NSC)Examinations/NSCPastExaminationpapers.aspx"

DBE_URL_Past_Paper_home_HTML <- read_html(DBE_URL_Past_Paper_home)
#html_nodes(DBE_URL_Past_Paper_home_HTML, "a")
html_attr(html_nodes(DBE_URL_Past_Paper_home_HTML, "a"), "href")
  