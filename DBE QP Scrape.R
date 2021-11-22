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
# URL to be scraped 
# 2019 May/June
#My_Url <- "https://www.education.gov.za/2019JuneNSCExamPapers.aspx"
# 2019 May/Junev
My_Url <- "https://www.education.gov.za/2019NovExams.aspx"
# 2020 November
#My_Url <- "https://www.education.gov.za/Curriculum/NationalSeniorCertificate(NSC)Examinations/2020NSCExamPapers.aspx"
#
# Examination Sitting. 
#--------------------
c_Title_text <- read_html(My_Url) %>% html_nodes("title") %>% html_text %>% str_replace_all("\\r", "") %>% 
  str_replace_all("\\n", "") %>% str_replace_all("\\t", "")
c_Title_text
#
# build subject data frame 
#-------------------------
l_subjects <- read_html(My_Url) %>%  html_nodes(".eds_containerTitle")
subject_df <- data.frame( Module_No = as.numeric(str_extract_all(l_subjects, "[0-9]+")), 
                          Module_name = html_text(l_subjects))
subject_df

# build pdf link data frame 
#--------------------------
Page_URLs   <- data.frame(
  PDF_URL = read_html(My_Url) %>%  html_nodes(".TitleCell") %>%  
    html_nodes("a") %>% html_attr("href") %>% url_absolute(My_Url),
  Subject_Name = read_html(My_Url) %>% html_nodes(".TitleCell")  %>%
    html_nodes("a") %>% html_text() )
Page_URLs   <- Page_URLs %>% 
  mutate (Module_No = as.numeric(str_extract(PDF_URL,"\\d{4}$")),
          Grade=c("12"), 
          Year_title = c(c_Title_text),  
          Paper_type = NA,
          Paper_Language = NA)
Page_URLs             
df_FULL <-  merge(x=Page_URLs, y=subject_df, by="Module_No")
df_FULL
df_FULL$Paper_type <- ifelse(str_detect(df_FULL$Subject_Name, regex("memo", ignore_case = TRUE)) & is.na(df_FULL$Paper_type),"Memo",df_FULL$Paper_type)
df_FULL$Paper_type <- ifelse(str_detect(df_FULL$Subject_Name, regex("Paper", ignore_case = TRUE)) & is.na(df_FULL$Paper_type), "Question Paper", df_FULL$Paper_type) 
df_FULL$Paper_type <- ifelse(str_detect(df_FULL$Subject_Name, regex(" P1", ignore_case = TRUE)) & is.na(df_FULL$Paper_type), "Question Paper", df_FULL$Paper_type) 
df_FULL$Paper_type <- ifelse(str_detect(df_FULL$Subject_Name, regex(" P2", ignore_case = TRUE)) & is.na(df_FULL$Paper_type), "Question Paper", df_FULL$Paper_type) 
df_FULL$Paper_type <- ifelse(str_detect(df_FULL$Subject_Name, regex(" P3", ignore_case = TRUE)) & is.na(df_FULL$Paper_type), "Question Paper", df_FULL$Paper_type) 
df_FULL$Subject_Name <- str_replace(df_FULL$Subject_Name, "P1", "Paper 1" )
df_FULL$Subject_Name <- str_replace(df_FULL$Subject_Name, "P2", "Paper 2" )
df_FULL$Subject_Name <- str_replace(df_FULL$Subject_Name, "P3", "Paper 3" )
df_FULL$Subject_Name <- str_replace(df_FULL$Subject_Name, "Memo 1", "Paper 1 Memo" )
df_FULL$Subject_Name <- str_replace(df_FULL$Subject_Name, "Memo 2", "Paper 2 Memo" )
df_FULL$Paper_Language <- ifelse(str_detect(df_FULL$Subject_Name, regex("English & Afrikaans", ignore_case = TRUE)) & is.na(df_FULL$Paper_Language), "English and Afrikaans", df_FULL$Paper_Language) 
df_FULL$Paper_Language <- ifelse(str_detect(df_FULL$Subject_Name, regex("Afrikaans", ignore_case = TRUE)) & is.na(df_FULL$Paper_Language), "Afrikaans", df_FULL$Paper_Language) 
df_FULL$Paper_Language <- ifelse(str_detect(df_FULL$Subject_Name, regex("English", ignore_case = TRUE)) & is.na(df_FULL$Paper_Language), "English", df_FULL$Paper_Language) 
df_FULL$Paper_Language <- ifelse(str_detect(df_FULL$Subject_Name, regex(" Afr ", ignore_case = TRUE)) & is.na(df_FULL$Paper_Language), "Afrikaans", df_FULL$Paper_Language) 
df_FULL$Paper_Language <- ifelse(str_detect(df_FULL$Subject_Name, regex(" Eng ", ignore_case = TRUE)) & is.na(df_FULL$Paper_Language), "English", df_FULL$Paper_Language) 
df_FULL$Paper_Language <- ifelse(str_detect(df_FULL$Subject_Name, regex("(Afr)", ignore_case = TRUE)) & is.na(df_FULL$Paper_Language), "Afrikaans", df_FULL$Paper_Language) 
df_FULL$Paper_Language <- ifelse(str_detect(df_FULL$Subject_Name, regex("(Eng)", ignore_case = TRUE)) & is.na(df_FULL$Paper_Language), "English", df_FULL$Paper_Language) 
df_FULL$Paper_Language <- ifelse(is.na(df_FULL$Paper_Language), "English and Afrikaans", df_FULL$Paper_Language) 

df_FULL$Match_Title <- df_FULL$Subject_Name
df_FULL$Match_Title <- str_replace(df_FULL$Match_Title, "Memo", "")
df_FULL$Match_Title <- str_replace(df_FULL$Match_Title, "memo", "")
df_FULL$Match_Title <- str_squish(df_FULL$Match_Title)
Exam_languages <- c("afrikaans", "English","isindebele","isixhosa", "isizulu", "sepedi", "sesotho", "setswana", "siswati", "tshivenda", "xitsonga")
df_FULL$Paper_Language <-ifelse(is.na(df_FULL$Paper_Language) & 
                                  tolower(word(df_FULL$Subject_Name, 1, sep=" ")) %in% Exam_languages,
                                word(df_FULL$Subject_Name, 1, sep=" "),
                                df_FULL$Paper_Language)
df_FULL
NA_DF <- df_FULL %>% filter_all(any_vars(is.na(.))) 
NA_DF     

NA_DF_unlist <- unlist(NA_DF)
typeof(NA_DF_unlist)
writeLines(NA_DF_unlist,"DOElist.txt")
