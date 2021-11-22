#library(tidyverse)
library(rvest)
library(stringr)
library(dplyr)
library(pdftools)

#-------------------------------------------------------------------------------
# hardcoded variables for extract - must be made dynamic ????
#-------------------------------------------------------------------------------
start_pat  <- "^1.1.1 "
end_pat    <- "^1.2 "
subject    <- "Agricultural"
Null_Value <- ""
PDF_url    <- ("https://www.education.gov.za/LinkClick.aspx?fileticket=Lo_oEjh0HMs%3d&tabid=2837&portalid=0&mid=9895")
#
#-------------------------------------------------------------------------------
#  convert PDF to a list 
#-------------------------------------------------------------------------------
#
PDF_list   <- pdf_text(PDF_url) %>% readr::read_lines() %>% str_trim()
PDF_list
#
#-------------------------------------------------------------------------------
# find multiple choice start
#-------------------------------------------------------------------------------
#
strt_inx   <- (str_detect(PDF_list, start_pat))
#
#-------------------------------------------------------------------------------
# find multiple choice end
#-------------------------------------------------------------------------------
#
end_inx <- (str_detect(PDF_list, end_pat))
#
#-------------------------------------------------------------------------------
# extract all multiple choice using above start and end 
#-------------------------------------------------------------------------------
#
PDF_list_extr <- PDF_list[as.numeric(which(strt_inx)):as.numeric(which(end_inx))-1] %>% tolower()
#-------------------------------------------------------------------------------
# tidy the list "only keeping questions and possible answer.  
#-------------------------------------------------------------------------------
subject    <- c("agricultural management practices|copyright|nsc")
not_req_string <-  str_detect(PDF_list_extr, subject)
PDF_list_with_empties <-  PDF_list_extr[!not_req_string]
PDF_list_tidy <-  PDF_list_with_empties[PDF_list_with_empties != ""]
PDF_list_tidy
typeof(PDF_list_tidy)
#-------------------------------------------------------------------------------
# Save to a text file 
#-------------------------------------------------------------------------------
writeLines(PDF_list_tidy,"AgriQP.txt")

#-------------------------------------------------------------------------------
# hardcoded variables for extract - must be made dynamic ????
#-------------------------------------------------------------------------------
start_pat  <- "^1.1.1 "
end_pat    <- "^1.2 "
subject    <- "Agricultural"
Null_Value <- ""
PDF_url    <- ("https://www.education.gov.za/LinkClick.aspx?fileticket=dHjFThcHQhk%3d&tabid=2837&portalid=0&mid=9895")
#
#-------------------------------------------------------------------------------
#  convert PDF to a list 
#-------------------------------------------------------------------------------
#
PDF_list   <- pdf_text(PDF_url) %>% readr::read_lines() %>% str_trim()
PDF_list

#
#-------------------------------------------------------------------------------
# find multiple choice start
#-------------------------------------------------------------------------------
#
strt_inx   <- (str_detect(PDF_list, start_pat))
#
#-------------------------------------------------------------------------------
# find multiple choice end
#-------------------------------------------------------------------------------
#
end_inx <- (str_detect(PDF_list, end_pat))
#
#-------------------------------------------------------------------------------
# extract all multiple choice using above start and end 
#-------------------------------------------------------------------------------
#
PDF_list_extr <- PDF_list[as.numeric(which(strt_inx)):as.numeric(which(end_inx))-1] %>% tolower()
#-------------------------------------------------------------------------------
# tidy the list "only keeping questions and possible answer.  
#-------------------------------------------------------------------------------
subject    <- c("agricultural management practices|copyright|nsc")
not_req_string <-  str_detect(PDF_list_extr, subject)
PDF_list_with_empties <-  PDF_list_extr[!not_req_string]
PDF_list_tidy <-  PDF_list_with_empties[PDF_list_with_empties != ""]
PDF_list_tidy
typeof(PDF_list_tidy)
#-------------------------------------------------------------------------------
# Save to a text file 
#-------------------------------------------------------------------------------
writeLines(PDF_list_tidy,"AgriMemo.txt")