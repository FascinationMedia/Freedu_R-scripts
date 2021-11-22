
###<include libraries>
library(rvest)
library(stringr)
library(dplyr)
library(pdftools)
library(httr)
####</include libraries>

#-------------------------------------------------------------------------------
# PART 1: build past papers per year link. 
#-------------------------------------------------------------------------------
# Assign DOE HOMR page 
DOE_PastPaper_Home_URL   <- "https://www.education.gov.za/Curriculum/NationalSeniorCertificate(NSC)Examinations/NSCPastExaminationpapers.aspx"
#
#  Build data frame of all links and test (i.e. past paper link and everything else)
#
DOE_PastPaper_URL_per_year<- data.frame( 
 
    URL = read_html(DOE_PastPaper_Home_URL)  
      %>% html_nodes("a") 
      %>% html_attr("href")
      %>% url_absolute(DOE_PastPaper_Home_URL),
                                        
    URL_text = read_html(DOE_PastPaper_Home_URL) 
      %>% html_nodes("a") 
      %>% html_text() )
#
# extract links where the link text contains "exam" and all links where the text starts with four digits representing the year"   
#
DOE_PastPaper_Exam_URLs <- filter(DOE_PastPaper_URL_per_year, str_detect(DOE_PastPaper_URL_per_year$URL_text, regex("exam", ignore_case= TRUE)))
DOE_PastPaper_Exam_URLs <- filter(DOE_PastPaper_URL_per_year, str_detect(DOE_PastPaper_URL_per_year$URL_text, regex("^\\d{4}")))
#                                                        
# Add a Year and Sitting (i.e. FEB/MAY/NOV) to the data frame
#  year 
DOE_PastPaper_Exam_URLs <- DOE_PastPaper_Exam_URLs %>% mutate(Exam_year = as.numeric(str_extract(DOE_PastPaper_Exam_URLs$URL_text,"\\d{4}")), Sitting = NA)   
#  November sitting 
DOE_PastPaper_Exam_URLs$Sitting <- ifelse(str_detect(DOE_PastPaper_Exam_URLs$URL_text, regex("nov", ignore_case = TRUE)) 
                                      & is.na(DOE_PastPaper_Exam_URLs$Sitting),"November",DOE_PastPaper_Exam_URLs$Sitting)
# February sitting
DOE_PastPaper_Exam_URLs$Sitting <- ifelse(str_detect(DOE_PastPaper_Exam_URLs$URL_text, regex("feb", ignore_case = TRUE)) 
                                      & is.na(DOE_PastPaper_Exam_URLs$Sitting),"February",DOE_PastPaper_Exam_URLs$Sitting)
# May sitting
DOE_PastPaper_Exam_URLs$Sitting <- ifelse(str_detect(DOE_PastPaper_Exam_URLs$URL_text, regex("may", ignore_case = TRUE)) 
                                    & is.na(DOE_PastPaper_Exam_URLs$Sitting),"May",DOE_PastPaper_Exam_URLs$Sitting)
# 
write.csv(DOE_PastPaper_Exam_URLs,"PastpaperYearLink.csv", row.names = FALSE)

rm(DOE_PastPaper_URL_per_year)
rmrm(DOE_PastPaper_Exam_URLs)
#-------------------------------------------------------------------------------
# PART 1: END
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# PART 2: build past papers per year per subject link. 
#-------------------------------------------------------------------------------
#                                                        
# Create Annual link as driver for subject per year scrape
#
DF_PastpaperYear <- as.data.frame(read.csv("PastpaperYearLink.csv", sep = ',', header = TRUE ))
#                                                      

# list of PDF url and other info per subject per year 
#
DF_All_Subject_Link_S <- data.frame()

#
# loop through per annums to buoild per year per subject 
#
for (row in 22:nrow(DF_PastpaperYear)) {
#for (row in 1:2) {  
  message("Processing Row no: ", row)
  Annual_link                    <- DF_PastpaperYear$URL[row]
  Annual_link_HTML               <- read_html(Annual_link)
  Title_text_c                     <- DF_PastpaperYear$URL_text[row]
  Exam_Year_c                      <- DF_PastpaperYear$Exam_year[row]
  Exam_Sitting_c                   <- DF_PastpaperYear$Sitting[row]
  message ("building dataframe links")
  DF_YearSubject              <- data.frame( 
    Exam_Year = Exam_Year_c,
    Exam_Sitting = Exam_Sitting_c,
    Title = Title_text_c,
    Module_No =
      as.numeric(
        Annual_link_HTML %>%
          html_nodes(".TitleCell") %>%  
          html_nodes("a") %>% 
          html_attr("href") %>%
          url_absolute(Annual_link) %>% 
          str_extract("\\d{4}$")),
    Module_name = Annual_link_HTML %>%  
       html_nodes(".TitleCell") %>%  
     html_nodes("a") %>% 
     html_text(),
    Module_URL = 
        Annual_link_HTML %>%
          html_nodes(".TitleCell") %>%  
          html_nodes("a") %>% 
          html_attr("href") %>%
          url_absolute(Annual_link))
    message("building dataframe subjects")
  l_subjects <- Annual_link_HTML %>% html_nodes(".eds_containerTitle")
  subject_df <- data.frame( Module_No = as.numeric(str_extract_all(l_subjects, "[0-9]+")), 
                            Module_name = html_text(l_subjects))
  message("merging")
  
  df_FULL <-  merge(x=DF_YearSubject, y=subject_df, by="Module_No")

      
  DF_All_Subject_Link_S <- rbind(DF_All_Subject_Link_S, df_FULL )
}
# 
write.csv(DF_All_Subject_Link_S,"PastpaperYearSubjectsLink.csv", row.names = FALSE)

rm(DOE_PastPaperYearURL)
rm(DF_PastpaperYear)
rm(DF_YearSubject)
rm(DF_Annual_link_Subject_Name)
rm(Annual_link_Subjects_HTML)
rm(DF_All_Subject_Link_S)
rm(Annual_link_HTML)
rm(df_FULL)
rm(l_subjects)
rm(subject_df)
#-------------------------------------------------------------------------------
# PART 2: end 
#-------------------------------------------------------------------------------

#
# tidy the data 
#
DF_All_Subject_Link_S$Subject_Name <- str_replace(DF_All_Subject_Link_S$Subject_Name, "P1", "Paper 1")
DF_All_Subject_Link_S$Subject_Name <- str_replace(DF_All_Subject_Link_S$Subject_Name, "P2", "Paper 2")
DF_All_Subject_Link_S$Subject_Name <- str_replace(DF_All_Subject_Link_S$Subject_Name, "P3", "Paper 3")
DF_All_Subject_Link_S$Subject_Name <- str_replace(DF_All_Subject_Link_S$Subject_Name, "memo 1", "Memo Paper 1")
DF_All_Subject_Link_S$Subject_Name <- str_replace(DF_All_Subject_Link_S$Subject_Name, "Memo 1", "Memo Paper 1")
DF_All_Subject_Link_S$Subject_Name <- str_replace(DF_All_Subject_Link_S$Subject_Name, "memo 2", "Memo Paper 2")
DF_All_Subject_Link_S$Subject_Name <- str_replace(DF_All_Subject_Link_S$Subject_Name, "Memo 2", "Memo Paper 2")
DF_All_Subject_Link_S$Subject_Name <- str_replace(DF_All_Subject_Link_S$Subject_Name, "memo 3", "Memo Paper 3")
DF_All_Subject_Link_S$Subject_Name <- str_replace(DF_All_Subject_Link_S$Subject_Name, "Memo 3", "Memo Paper 3")

DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex("English & Afrikaans", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_Language), "English and Afrikaans", DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex("Afrikaans", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_Language), "Afrikaans", DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex("English", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_Language), "English", DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex(" Afr ", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_Language), "Afrikaans", DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex(" Eng ", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_Language), "English", DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex("(Afr)", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_Language), "Afrikaans", DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex("(Eng)", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_Language), "English", DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse((is.na(DF_All_Subject_Link_S$Paper_Language) & (str_detect(DF_All_Subject_Link_S$Subject_Name, regex(" HL | FAL | SAL ", ignore_case = TRUE)))), DF_All_Subject_Link_S$Module_name, DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex(" HL | FAL | SAL ", ignore_case = TRUE)), DF_All_Subject_Link_S$Module_name, DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_type     <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex(" Memo ", ignore_case = TRUE)), "Memo", DF_All_Subject_Link_S$Paper_type) 
DF_All_Subject_Link_S$Paper_type     <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex(" Answerbook ", ignore_case = TRUE)), "AnswerBook", DF_All_Subject_Link_S$Paper_type) 

DF_All_Subject_Link_S <- DF_All_Subject_Link_S %>%  
  mutate (Year = str_extract(DF_All_Subject_Link_S$Year_title,regex("\\d{4}")), Paper_No = NA)

DF_All_Subject_Link_S$Paper_No <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex("paper 1", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_No), "paper 1", DF_All_Subject_Link_S$Paper_No) 
DF_All_Subject_Link_S$Paper_No <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex("paper 2", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_No), "paper 2", DF_All_Subject_Link_S$Paper_No)
DF_All_Subject_Link_S$Paper_No <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex("paper 3", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_No), "paper 3", DF_All_Subject_Link_S$Paper_No) 
DF_All_Subject_Link_S$paper  <- tolower(paste(DF_All_Subject_Link_S$Module_name, DF_All_Subject_Link_S$Paper_No))
#
# list of subjects with mulitple choice and the reg of where it is found 
#
my_data <- as.data.frame(read.table("list of paper with multichoice.txt", sep = '/', header = TRUE ))
my_data <- my_data %>% mutate(paper=tolower(paste(my_data$subject, my_data$paper_no)))

DF_final  <- merge(x=DF_All_Subject_Link_S, y=my_data, by="paper")

my_data <- as.data.frame(read.table("list of paper with multichoice.txt", sep = '/', header = TRUE ))

#
# <add document type word / pdf>
#

write.csv(DF_final,"Freeedu.csv", row.names = FALSE)
DF_final <- as.data.frame(read.csv("Freeedu.csv", sep = ',', header = TRUE ))
DF_final2 <- DF_final %>% arrange(subject, Year, Paper_Language)

library(httr)
temp_DF <- data.frame()
for (row in 1:nrow(DF_final2)) {
# for (row in 1:20)     { 
  r <- GET(DF_final2$PDF_URL[row])
  temp_DF <- rbind(temp_DF, cbind(type=r$headers$`content-type`,PDF_URL=DF_final2$PDF_URL[row]))
}
DF_Final3   <- merge(x=DF_final2, y=temp_DF, by="PDF_URL")
DF_Final3 <- DF_Final3 %>%  arrange(subject, Year, Paper_Language)
write.csv(DF_Final3,"MultipleChoice_QPs.csv", row.names = FALSE)
#
# </add document type word / pdf>
#

#
# <1) Remove word documents>
#

DF_AllTypes   <- as.data.frame(read.csv("MultipleChoice_QPs.csv", sep = ',', header = TRUE ))
DF_OnlyPDFs <- filter(DF_AllTypes,   str_detect(DF_AllTypes$type,   regex("pdf", ignore_case= TRUE)))
#DF_Empty   <- as.data.frame(read.csv("Empty.csv", sep = ',', header = TRUE ))
#
# </1) Remove word documents>
#


#
# </Build 1) Strip out word documents 2) split into memo and papers 3) combine memos and papers>
#
Question_df <- data.frame()
Question_df

#for (row in 1:nrow(DF_OnlyPDFs)) {
for (row in 1:10) { 
  message( 'row no', row)
  message( 'URL>', DF_OnlyPDFs$PDF_URL[row], '</URL')
  paper_link  <- DF_OnlyPDFs$PDF_URL[row]
  Paper_Year  <- DF_OnlyPDFs$Year[row]
#  PDF_list   <- pdf_text(paper_link) %>% readr::read_lines() %>% str_trim()
  
  tryCatch(
     {
      message("trying readr")
#      Paper_unfmt <- pdf_text(paper_link) %>% readr::read_lines() %>% str_trim()
      Paper_raw <- pdf_text(paper_link)
      message(c("----------------------------------------Start-----------------------------------------------------"))
      message("Year:", DF_OnlyPDFs$Year[row])
      message("subject:", DF_OnlyPDFs$subject[row])
      message(c("-----------------------------------------one----------------------------------------------------"))
      message(Paper_raw[1])
      message(c("-----------------------------------------two----------------------------------------------------"))
      message(Paper_raw[2])
#      paper_split <- str_split(Paper_raw, "\n")
#      message(paper_split)
#      message(typeof(paper_split))
    },
    error = function(err) 
      {
        message("read error for row ", row, "error")
       temp_DF = data.frame()
      }
  )
  #

#  subject    <- ("copyright|nsc|nss|kopiereg|please turn|blaai om|dbe/|dbo/|doe/")
#  not_req_string <-  str_detect(temp_DF, subject)
#  PDF_list_with_empties <-  temp_DF[!not_req_string]
#  PDF_list_tidy <-  data.frame("QP-TEXT"=PDF_list_with_empties[PDF_list_with_empties != ""])
  
#  Questions_enriched <- data.frame(PDF_list_tidy %>% mutate(P_Year = DF_OnlyPDFs$Year[row], subject = DF_OnlyPDFs$Subject_Name[row]))
#  Question_df1 <- as.data.frame(Questions_enriched)
#  Question_df <- rbind(Question_df, Question_df1)
##  Question_df <- rbind(Question_df, DF_Empty)
##  message(Questions_df)
}
Question_df
write.csv(Question_df,"Dwnld_QPs.csv", row.names = FALSE)
# 
# //# 
# #for (row in 1:nrow(DF_OnlyPDFs)) {
# for (row in 1:20) { 
#   message( 'row no', row)
#   message( 'URL>', DF_OnlyPDFs$PDF_URL[row], '</URL')
#   paper_link  <- DF_OnlyPDFs$PDF_URL[row]
#   Paper_Year  <- DF_OnlyPDFs$Year[row]

#   #  PDF_list   <- pdf_text(paper_link) %>% readr::read_lines() %>% str_trim()
#   
#   tryCatch(
#     {
#       message("trying readr")
#       PDF_list = pdf_text(paper_link) %>% readr::read_lines() %>% str_trim()
#       #      message(PDF_list)
#     },
#     error = function(err) 
#     {
#       message("read error for row ", row, "error")
#     }
#   )
#
#----------------------------------------------------------------------------------------------------
#  add column to signify if the question [paper is a PDF-doc] 
#----------------------------------------------------------------------------------------------------
gc()
rm(DF_List)
DF_List <- as.data.frame(read.csv("PastpaperYearSubjectsLink.csv", sep = ',', header = TRUE ))
if ("Doc_Type" %in% colnames(DF_List))
  {
   DF_List <- select(DF_List,-c(Doc_Type))
}
DF_List <- DF_List %>% mutate(Doc_Type = NA)
write.csv(DF_List,"PastpaperYearSubjectsLink.csv", row.names = FALSE)
rm(DF_List)

DF_List <- as.data.frame(read.csv("PastpaperYearSubjectsLink.csv", sep = ',', header = TRUE ))

break_value <-  9
RowsProcessedThisLoop <- 0 
run_type <- 'prod'
RowsToProcess = ifelse((run_type == "prod"), nrow(DF_List), 300)
message(nrow(DF_List))
if (RowsToProcess > nrow(DF_List))
{
  RowsToProcess <- nrow(DF_List)
}

if (RowsToProcess < break_value)
{
  break_value = RowsToProcess
}
for (row in 4925:RowsToProcess) {
  RowsProcessedThisLoop = RowsProcessedThisLoop + 1
    if (RowsProcessedThisLoop > break_value)
    {
      RowsProcessedThisLoop = 0 
      write.csv(DF_List,"PastpaperYearSubjectsLink.csv", row.names = FALSE)
#      DF_List <- as.data.frame(read.csv("PastpaperYearSubjectsLink.csv", sep = ',', header = TRUE ))
      message ("stopped on stop value", row)
      WriteToCSV = TRUE
    }
  r <- GET(DF_List$Module_URL[row])
  DocType <- as.character(headers(r)['content-type'])
  DF_List$Doc_Type[row] <- DocType
  gc()
  rm(r)
}
if (RowsProcessedThisLoop != break_value)
{
  write.csv(DF_List,"PastpaperYearSubjectsLink.csv", row.names = FALSE)
}

#   #-------------------------------------------------------------------------------
#   # find multiple choice start
#   #-------------------------------------------------------------------------------
#   #
   regex_S     <- paste0("^",DF_final$regex_start[row]," ")
   strt_inx    <- (str_detect(PDF_list, regex_S))
#   #  message("start inx", strt_inx)
#   #
#   #-------------------------------------------------------------------------------
#   # find multiple choice end
#   #-------------------------------------------------------------------------------
#   #
   regex_end <- paste0("^",toString(DF_final$regex_end[row])," ")
   end_inx <- (str_detect(PDF_list, regex_end))
#   #
#   #-------------------------------------------------------------------------------
#   # extract all multiple choice using above start and end 
#   #-------------------------------------------------------------------------------
#   #
  tryCatch(
    {
      message("trying start & end")
      temp_DF <- PDF_list[as.numeric(which(strt_inx)):as.numeric(which(end_inx))-1] %>% tolower()
      message("temp_DF type", typeof(temp_DF))
    },
    error = function(err)
    {
      message("start end error", err)
      temp_DF <- ""
    }
  )
  #  temp_DF <- PDF_list[as.numeric(which(strt_inx)):as.numeric(which(end_inx))-1] %>% tolower()

  subject    <- ("copyright|nsc|nss|kopiereg|please turn|blaai om|dbe/|dbo/|doe/")
  not_req_string <-  str_detect(temp_DF, subject)
  PDF_list_with_empties <-  temp_DF[!not_req_string]
  PDF_list_tidy <-  data.frame(PDF_list_with_empties[PDF_list_with_empties != ""])

  Questions_enriched <- data.frame(PDF_list_tidy %>% mutate(P_Year = DF_OnlyPDFs$Year[row], subject = DF_OnlyPDFs$Subject_Name[row]))
  Question_df1 <- as.data.frame(Questions_enriched)
  Question_df <- rbind(Question_df, Question_df1)
  #  message(Questions_df)
}
Question_df
