##############################################################################################################
# 000) Get the required R libraries 
# 100) Setup DOE past papers URL
# 110) Extract all the past paper URLs into dataframe 
# 120) Extract all the past paper URL text into dataframe
# 130) merge data frames from 110 & 120 
#
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
# 100 
#
DBE_PastPapers_URL <- "https://www.education.gov.za/Curriculum/NationalSeniorCertificate(NSC)Examinations/NSCPastExaminationpapers.aspx"
#
# 110 
#
DBE_URL_PastPaper_child_URLs = data.frame(read_html(DBE_PastPapers_URL) %>%  html_nodes(".DnnModule-1741") %>%  
  html_nodes("a") %>% html_attr("href") %>% url_absolute(DBE_PastPapers_URL))
DBE_URL_PastPaper_child_URLs <- DBE_URL_PastPaper_child_URLs %>% mutate(ROW = row(DBE_URL_PastPaper_child_URLs))
DBE_URL_PastPaper_child_URLs

#
# 120 
#
BE_URL_PastPaper_child_URLtext = data.frame(read_html(DBE_PastPapers_URL) %>% html_nodes(".DnnModule-1741")  %>%
  html_nodes("a") %>% html_text())
BE_URL_PastPaper_child_URLtext <- BE_URL_PastPaper_child_URLtext %>% mutate(ROW = row(BE_URL_PastPaper_child_URLtext))
BE_URL_PastPaper_child_URLtext
#
# 130 
#
df_URLnText <-  merge(x=DBE_URL_PastPaper_child_URLs, y=BE_URL_PastPaper_child_URLtext, by="ROW")

df_URLnText




html_nodes(DBE_URL_Past_Paper_home_HTML)
#html_attr(html_nodes(DBE_URL_Past_Paper_home_HTML, "a"), "href")
  