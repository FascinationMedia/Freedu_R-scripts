
###<include libraries>
library(rvest)
library(stringr)
library(dplyr)
library(pdftools)
library(httr)
library(mime)
####</include libraries>
#
# run config 
#
Prod_Run  <- TRUE
Rows2Test <- 10 
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
rm(DOE_PastPaper_Exam_URLs)
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
# loop through per annum to build per year per subject 
#
if (Prod_Run) 
{
  Rows2Process <-   nrow(DF_PastpaperYear)
} else {
  Rows2Process <-   Rows2Test
} 
for (row in 1:Rows2Process) {
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
          str_extract("\\d{1,6}$")),
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
    
    l_subjects <- Annual_link_HTML %>% html_nodes(".eds_containerTitle") 
#  l_subjects <- as.list(Annual_link_HTML %>% html_nodes(".eds_containerTitle") %>% html_attr("id"))
  subject_df <- data.frame( 
    Module_No =
      Annual_link_HTML %>% html_nodes(".eds_containerTitle") %>% html_attr("id"),
    Module_name = Annual_link_HTML %>% html_nodes(".eds_containerTitle") %>% html_text())
  subject_df$Module_No <- gsub("[^0-9]", "", subject_df$Module_No) 
  
#  subject_df$Module_No <- subject_df$Module_No %>% str_replace("dnn_ctr1","") %>% str_replace("_dnnTITLE","") %>% str_replace("_titleLabel","") 
  df_FULL <-  merge(x=DF_YearSubject, y=subject_df, by="Module_No")
  DF_All_Subject_Link_S <- rbind(DF_All_Subject_Link_S, df_FULL )
}
# 
write.csv(DF_All_Subject_Link_S,"PastpaperYearSubjectsLink.csv", row.names = FALSE)

rm(DF_PastpaperYear)
rm(Annual_link_HTML)
rm(df_FULL)
rm(l_subjects)
rm(subject_df)
#-------------------------------------------------------------------------------
# PART 2: end 
#-------------------------------------------------------------------------------

#
# tidy the data 
DF_All_Subject_Link_S <- as.data.frame(read.csv("PastpaperYearSubjectsLink.csv", sep = ',', header = TRUE ))
DF_All_Subject_Link_S['Paper_Language'] <- NA
DF_All_Subject_Link_S['Paper_Type'] <- NA
DF_All_Subject_Link_S['Paper_No'] <- NA
colnames(DF_All_Subject_Link_S) <- c('Module_No','Exam_Year','Exam_Sitting','Title','Paper','Module_URL','Subject','Paper_Language', 'Paper_type', 'Paper_No')
#
DF_All_Subject_Link_S$Paper <- str_replace(DF_All_Subject_Link_S$Paper, "P1", "Paper 1")
DF_All_Subject_Link_S$Paper <- str_replace(DF_All_Subject_Link_S$Paper, "P2", "Paper 2")
DF_All_Subject_Link_S$Paper <- str_replace(DF_All_Subject_Link_S$Paper, "P3", "Paper 3")
DF_All_Subject_Link_S$Paper <- str_replace(DF_All_Subject_Link_S$Paper, "memo 1", "Memo Paper 1")
DF_All_Subject_Link_S$Paper <- str_replace(DF_All_Subject_Link_S$Paper, "Memo 1", "Memo Paper 1")
DF_All_Subject_Link_S$Paper <- str_replace(DF_All_Subject_Link_S$Paper, "memo 2", "Memo Paper 2")
DF_All_Subject_Link_S$Paper <- str_replace(DF_All_Subject_Link_S$Paper, "Memo 2", "Memo Paper 2")
DF_All_Subject_Link_S$Paper <- str_replace(DF_All_Subject_Link_S$Paper, "memo 3", "Memo Paper 3")
DF_All_Subject_Link_S$Paper <- str_replace(DF_All_Subject_Link_S$Paper, "Memo 3", "Memo Paper 3")

DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Paper, regex("English & Afrikaans", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_Language), "English and Afrikaans", DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Paper, regex("Afrikaans", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_Language), "Afrikaans", DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Paper, regex("English", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_Language), "English", DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Paper, regex(" Afr ", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_Language), "Afrikaans", DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Paper, regex(" Eng ", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_Language), "English", DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Paper, regex("(Afr)", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_Language), "Afrikaans", DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Paper, regex("(Eng)", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_Language), "English", DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse((is.na(DF_All_Subject_Link_S$Paper_Language) & (str_detect(DF_All_Subject_Link_S$Paper, regex(" HL | FAL | SAL ", ignore_case = TRUE)))), DF_All_Subject_Link_S$Subject, DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_Language <- ifelse(str_detect(DF_All_Subject_Link_S$Paper, regex(" HL | FAL | SAL ", ignore_case = TRUE)), DF_All_Subject_Link_S$Subject, DF_All_Subject_Link_S$Paper_Language) 
DF_All_Subject_Link_S$Paper_type     <- ifelse(str_detect(DF_All_Subject_Link_S$Paper, regex(" Memo ", ignore_case = TRUE)), "Memo", DF_All_Subject_Link_S$Paper_type) 
DF_All_Subject_Link_S$Paper_type     <- ifelse(str_detect(DF_All_Subject_Link_S$Paper, regex(" Answerbook ", ignore_case = TRUE)), "AnswerBook", DF_All_Subject_Link_S$Paper_type) 

#DF_All_Subject_Link_S <- DF_All_Subject_Link_S %>%  
#  mutate (Year = str_extract(DF_All_Subject_Link_S$Year_title,regex("\\d{4}")), Paper_No = NA)

DF_All_Subject_Link_S$Paper_No <- ifelse(str_detect(DF_All_Subject_Link_S$Paper, regex("paper 1", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_No), "paper 1", DF_All_Subject_Link_S$Paper_No) 
DF_All_Subject_Link_S$Paper_No <- ifelse(str_detect(DF_All_Subject_Link_S$Paper, regex("paper 2", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_No), "paper 2", DF_All_Subject_Link_S$Paper_No)
DF_All_Subject_Link_S$Paper_No <- ifelse(str_detect(DF_All_Subject_Link_S$Paper, regex("paper 3", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_No), "paper 3", DF_All_Subject_Link_S$Paper_No) 
DF_All_Subject_Link_S$Paper_No  <- tolower(paste(DF_All_Subject_Link_S$Subject, DF_All_Subject_Link_S$Paper_No))
#
# list of subjects with mulitple choice and the reg of where it is found 
#
my_data <- as.data.frame(read.table("list of paper with multichoice.txt", sep = '/', header = TRUE ))
my_data <- my_data %>% mutate(paper=tolower(paste(my_data$subject, my_data$paper_no)))

DF_final  <- merge(x=DF_All_Subject_Link_S, y=my_data, by.x="Paper_No", by.y="paper")

#
# <add document type word / pdf>
#

write.csv(DF_final,"Freeedu.csv", row.names = FALSE)
DF_final <- as.data.frame(read.csv("Freeedu.csv", sep = ',', header = TRUE ))
DF_final2 <- DF_final %>% arrange(Subject, Exam_Year, Paper_Language)

rm(DF_All_Subject_Link_S)

temp_DF <- data.frame()
if (Prod_Run) 
{
  Rows2Process <-   nrow(DF_All_Subject_Link_S)
} else {
  Rows2Process <-   Rows2Test
} 
#for (row in 85:90) {
for (row in 91:Rows2Process) {
  
  link        <-  DF_All_Subject_Link_S$Module_URL[row]
  TempFile    <- tempfile()
  r           <- GET(link, write_disk(TempFile, overwrite = TRUE))
  tmp_fld     <- as.character(headers(r)['content-type'])
  
# Determine the file extension based on MIME type
  file_extension <- " "
  if (tmp_fld == "application/pdf")
  {
    file_extension <- ".pdf"
  }
  
  if (tmp_fld == "text/csv")
  {
    file_extension <- ".csv"
  }
  
  if (tmp_fld == "text/plain")
  {
    file_extension <- ".txt"
  }
  
  if (tmp_fld == "application/msword")
  {
    file_extension <- ".doc"
  }
  
 #   file_extension <- switch(tmp_fld,
#                  "image/jpeg" = ".jpg",
#                  "image/png" = ".png",
#                  "application/zip" = ".zip",
  if(file_extension == " ") 
  { 
    message (" skipped entry ", row, " file type ", tmp_fld, "filename ", fname )
    next 
  }
        
  fname <- paste("C:/DEV/R/data/pastpapers/",
                 DF_All_Subject_Link_S$Exam_Year[row], 
                 " ",
                 DF_All_Subject_Link_S$Exam_Sitting[row],
                 " ",
                 DF_All_Subject_Link_S$Subject[row],
                 " ",
                 DF_All_Subject_Link_S$Paper[row],
                 file_extension
  )
#    
#  fname <- paste("C:/DEV/R/data/pastpapers/",
##                 DF_final2$Exam_Year[row], 
#                 " ",
#                 DF_final2$Exam_Sitting[row],
#                 " ",
#                 DF_final2$Subject[row],
#                 " ",
#                 DF_final2$Paper[row],
#                 file_extension
#              )
  file.copy(TempFile, fname, overwrite = TRUE)
# Remove the temporary file
  unlink(TempFile)
  
  message ("added paper no ", row, " file ", fname)
#  temp_DF <- rbind(temp_DF, cbind(type=r$headers$`content-type`,Module_URL=DF_final2$Module_URL[row]))
}
DF_Final3   <- merge(x=DF_final2, y=temp_DF, by="Module_URL")
DF_Final3 <- DF_Final3 %>%  arrange(subject, Exam_Year, Paper_Language)
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
Q_df <- data.frame()

for (row in 1:nrow(DF_OnlyPDFs)) {
#for (row in 1:10) { 
  message( 'row no', row)
  message( 'URL>', DF_OnlyPDFs$PDF_URL[row], '</URL')
  qp_url  <- DF_OnlyPDFs$PDF_URL[row]
  Paper_Year  <- DF_OnlyPDFs$Year[row]
  
  tryCatch(
     {
      message("trying readr")
   

    },
    error = function(err) 
      {
        message("read error for row ", row, "error")
       temp_DF = data.frame()
      }
  )
}
write.csv(temp_DF,"Dwnld_QPs.csv", row.names = FALSE)
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
#run_type <- 'test'
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
for (row in 1:RowsToProcess) {
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
#-------------------------------------------------------------------------------
# find multiple choice start
#-------------------------------------------------------------------------------


rm(DF_List)
rm(DF_temp)

DF_List <- as.data.frame(read.csv("PastpaperYearSubjectsLink.csv", sep = ',', header = TRUE ))
DF_temp <- data.frame()

DF_temp <- filter(DF_List, Doc_Type == "application/pdf")
DF_temp$Module_name.x <- str_replace(DF_temp$Module_name.x, "P1", "Paper 1")
DF_temp$Module_name.x <- str_replace(DF_temp$Module_name.x, "P2", "Paper 2")
DF_temp$Module_name.x <- str_replace(DF_temp$Module_name.x, "P3", "Paper 3")
DF_temp$Module_name.x <- str_replace(DF_temp$Module_name.x, "memo 1", "Memo Paper 1")
DF_temp$Module_name.x <- str_replace(DF_temp$Module_name.x, "Memo 1", "Memo Paper 1")
DF_temp$Module_name.x <- str_replace(DF_temp$Module_name.x, "memo 2", "Memo Paper 2")
DF_temp$Module_name.x <- str_replace(DF_temp$Module_name.x, "Memo 2", "Memo Paper 2")
DF_temp$Module_name.x <- str_replace(DF_temp$Module_name.x, "memo 3", "Memo Paper 3")
DF_temp$Module_name.x <- str_replace(DF_temp$Module_name.x, "Memo 3", "Memo Paper 3")
DF_temp$Module_name.x <- str_replace(DF_temp$Module_name.x, " & ", " and ")

DF_temp <-   mutate(DF_temp,language = NA, paper = NA, annex = NA, region = NA, HL_SAL_FAL = NA, control = NA)
DF_temp$paper <- ifelse(str_detect(DF_temp$Module_name.x, regex("paper 1", ignore_case = TRUE)) & is.na(DF_temp$paper), "paper 1", DF_temp$paper) 
DF_temp$paper <- ifelse(str_detect(DF_temp$Module_name.x, regex("paper 2", ignore_case = TRUE)) & is.na(DF_temp$paper), "paper 2", DF_temp$paper) 
DF_temp$paper <- ifelse(str_detect(DF_temp$Module_name.x, regex("paper 3", ignore_case = TRUE)) & is.na(DF_temp$paper), "paper 3", DF_temp$paper) 
#
DF_temp$language <- ifelse(str_detect(DF_temp$Module_name.x, regex("afrikaans and english", ignore_case = TRUE)) & is.na(DF_temp$language), "english and afrikaans",  DF_temp$language) 
DF_temp$language <- ifelse(str_detect(DF_temp$Module_name.x, regex("english and afrikaans", ignore_case = TRUE)) & is.na(DF_temp$language), "english and afrikaans",  DF_temp$language) 
DF_temp$language <- ifelse(str_detect(DF_temp$Module_name.x, regex("english",                ignore_case = TRUE)) & is.na(DF_temp$language), "english",               DF_temp$language) 
DF_temp$language <- ifelse(str_detect(DF_temp$Module_name.x, regex("afrikaans",              ignore_case = TRUE)) & is.na(DF_temp$language), "afrikaans",             DF_temp$language) 
DF_temp$language <- ifelse(str_detect(DF_temp$Module_name.x, regex("(eng)",                  ignore_case = TRUE)) & is.na(DF_temp$language), "english",               DF_temp$language) 
DF_temp$language <- ifelse(str_detect(DF_temp$Module_name.x, regex("(afr)",                  ignore_case = TRUE)) & is.na(DF_temp$language), "afrikaans",             DF_temp$language) 
DF_temp$language <- ifelse(str_detect(DF_temp$Module_name.x, regex(" HL | FAL | SAL ",       ignore_case = TRUE)) & is.na(DF_temp$language), DF_temp$Module_name.y,   DF_temp$language) 
#
DF_temp$region   <- ifelse(str_detect(DF_temp$Module_name.x, regex("Limpopo ",                        ignore_case = TRUE)) & is.na(DF_temp$region), "Limpopo",       DF_temp$region) 
DF_temp$region   <- ifelse(str_detect(DF_temp$Module_name.x, regex("Gauteng",                         ignore_case = TRUE)) & is.na(DF_temp$region), "Gauteng",       DF_temp$region) 
DF_temp$region   <- ifelse(str_detect(DF_temp$Module_name.x, regex("North West",                      ignore_case = TRUE)) & is.na(DF_temp$region), "North West",    DF_temp$region) 
DF_temp$region   <- ifelse(str_detect(DF_temp$Module_name.x, regex("Western Cape",                    ignore_case = TRUE)) & is.na(DF_temp$region), "Western Cape",  DF_temp$region) 
DF_temp$region   <- ifelse(str_detect(DF_temp$Module_name.x, regex("Mpumalanga",                      ignore_case = TRUE)) & is.na(DF_temp$region), "Mpumalanga",    DF_temp$region) 
DF_temp$region   <- ifelse(str_detect(DF_temp$Module_name.x, regex("Eastern Cape",                    ignore_case = TRUE)) & is.na(DF_temp$region), "Eastern Cape",  DF_temp$region) 
DF_temp$region   <- ifelse(str_detect(DF_temp$Module_name.x, regex("Northern Cape",                   ignore_case = TRUE)) & is.na(DF_temp$region), "Northern Cape", DF_temp$region) 
DF_temp$region   <- ifelse(str_detect(DF_temp$Module_name.x, regex("Free State",                      ignore_case = TRUE)) & is.na(DF_temp$region), "Free State",    DF_temp$region) 
DF_temp$region   <- ifelse(str_detect(DF_temp$Module_name.x, regex("KZN|KwaZulu-Natal|KwaZulu Natal", ignore_case = TRUE)) & is.na(DF_temp$region), "KwaZulu-Natal", DF_temp$region) 
DF_temp$region   <- ifelse( is.na(DF_temp$region), "All",DF_temp$region) 

DF_temp$HL_SAL_FAL <- ifelse(str_detect(DF_temp$Module_name.x, regex(" HL ",        ignore_case = TRUE)) & is.na(DF_temp$HL_SAL_FAL), "HL" , DF_temp$HL_SAL_FAL) 
DF_temp$HL_SAL_FAL <- ifelse(str_detect(DF_temp$Module_name.x, regex(" SAL ",       ignore_case = TRUE)) & is.na(DF_temp$HL_SAL_FAL), "SAL", DF_temp$HL_SAL_FAL) 
DF_temp$HL_SAL_FAL <- ifelse(str_detect(DF_temp$Module_name.x, regex(" FAL ",       ignore_case = TRUE)) & is.na(DF_temp$HL_SAL_FAL), "FAL", DF_temp$HL_SAL_FAL) 

DF_temp$control  <- DF_temp$Module_name.x 
DF_temp$control  <- str_replace(DF_temp$control, DF_temp$Module_name.y, "")
DF_temp$control  <- str_replace(tolower(DF_temp$control), regex("hl|fal|sal", ignore_case = TRUE), "") 
DF_temp$control  <- str_replace(tolower(DF_temp$control),DF_temp$paper, "")
DF_temp$control  <- str_replace(tolower(DF_temp$control),DF_temp$language, "")
DF_temp$control  <- str_replace(tolower(DF_temp$control), regex("Limpopo|Gauteng|North West|Western Cape|Mpumalanga|Eastern Cape|Northern Cape|Free State|KZN|KwaZulu-Natal|KwaZulu Natal",ignore_case = TRUE),"")
DF_temp$control  <- str_replace(tolower(DF_temp$control),"and afrikaans", "")
DF_temp$control  <- str_replace(DF_temp$control,regex("\\("),"")
DF_temp$control  <- str_replace(DF_temp$control,"\\)","")
DF_temp          <- DF_temp[with(DF_temp, order(Module_name.y,-Exam_Year, Exam_Sitting,paper, language, HL_SAL_FAL,region, control)),]
write.csv(DF_temp,"PastpaperYearSubjectsLink.csv", row.names = FALSE)


DF_MCsubjects <- as.data.frame(read.csv("list of paper with multichoice.txt", sep = '/', header = TRUE ))
DF_temp$Module_name.y     <- tolower(DF_temp$Module_name.y) 
#names(DF_temp2)[7]       <- "Subjects" 
DF_MCsubjects             <- mutate(DF_MCsubjects, DFkey2 = tolower(paste(str_trim(subject), str_trim(paper_no)))) 
DF_temp                  <- mutate(DF_temp, DFkey2 = paste(Module_name.y, paper)) 

MCSubjects                <- tolower(DF_MCsubjects$subject) 
DF_tempjoin               <- left_join(x=DF_temp, y=DF_MCsubjects, by="DFkey2" ) 
DF_temp                   <- DF_temp[DF_temp$Module_name.y %in% MCSubjects, ]
DF_tempQPs                <- DF_temp[str_trim(DF_temp$control) == "",]
DF_tempMemos              <- DF_temp[str_trim(DF_temp$control) == "memo",]
names(DF_tempQPs)[6]      <- "QP_URL" 
DF_tempQPs                <- mutate(DF_tempQPs, DFkey = paste(Module_name.y,Exam_Year, Exam_Sitting,paper, language, HL_SAL_FAL,region))
DF_tempMemos              <- mutate(DF_tempMemos, DFkey = paste(Module_name.y,Exam_Year, Exam_Sitting,paper, language, HL_SAL_FAL,region))
names(DF_tempMemos)[6]    <- "Memo_URL" 
rm(DF_tempCombined)
DF_tempMemos              <- DF_tempMemos[with(DF_tempMemos, order(DFkey)),]
DF_tempQPs                <- DF_tempQPs[with(DF_tempQPs, order(DFkey)),]
DF_tempCombined           <- left_join(x=DF_tempQPs, y=DF_tempMemos, by="DFkey")

DF_tempCombined[c(18,19,20,21,24,25,26,27,28,29,30,31,32,33)] <-list(NULL)
DF_MCsubjects$subject     <- tolower(DF_MCsubjects$subject)  
DF_MCsubjects             <- mutate(DF_MCsubjects, DFkey2 = tolower(paste(str_trim(subject), str_trim(paper_no)))) 
DF_tempCombined           <- mutate(DF_tempCombined, DFkey2 = paste(Module_name.y.x, paper.x)) 
DF_tempwregex             <- full_join(x=DF_tempCombined, y=DF_MCsubjects, by="DFkey2" ) 

library(tidyr)
DF_Final             <- DF_tempwregex %>% drop_na(QP_URL)
DF_Final             <- DF_Final %>% drop_na(Memo_URL)

#
write.csv(DF_Final,"DownloadList.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------------
# Restart Point 
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# loop through Download list to create "questions.dart"for 
#-----------------------------------------------------------------------------------------------------------
# 1. Load           URLS into dataframe.   
# 2. Load Exclusion URLS into dataframe.
# 3. Filter 2014 and up.
# 4. Create empty frames.
#
QPnMemo_DF_temp <- as.data.frame(read.csv("DownloadList.csv",  sep = ',', header = TRUE ))
Exclusion_DF    <- as.data.frame(read.csv("ExclusionList.csv", sep = ',', header = TRUE ))
QPnMemo_DF      <- QPnMemo_DF_temp[QPnMemo_DF_temp$Exam_Year.x > 2014,]
Freedu_App_Fmt  <- data.frame()
IDCntr <- 0 
#for (row in 1:nrow(DF_tempCombined)) 
row <- 1 
# for (row in 1:20)
for (row in 1:nrow(QPnMemo_DF))
 {
#
# check if the question paper and memo renders correctly.   
#  if not skip the question paper and memo 
#
  message("question paper ", row, " " , QPnMemo_DF$DFkey[row] )
  if (grepl(QPnMemo_DF$DFkey[row], Exclusion_DF))
   {
    message("skipped paper")
    next 
   }
  qp_url    <- QPnMemo_DF$QP_URL[row]
  memo_url  <- QPnMemo_DF$Memo_URL[row]
#
# QUESTION PAPER scrub   
# 1. - request URL
# 3. - remove strings that are not part of a question.
# 4. - remove the points allocation at the end of each question eg. (5).
# 5. - remove empty lines
# 6. - determine the start and end index of the multiple questions (usually question 1)
# 7. - read Memo.v
#  
# scrub the data
#1.
  QP_PDF_text  <- pdf_text(qp_url) %>% readr::read_lines() %>% str_trim() %>% tolower()
#3.
  remove_strings <- ("copyright|nsc |nss|kopiereg|please turn|blaai om|dbe/|dbo/|doe/|totaal afdeling|groottotaal")
  extr_list      <- str_detect(QP_PDF_text,remove_strings)
  QP_PDF_text    <- QP_PDF_text[!extr_list]
#4
  QP_PDF_text    <- gsub("\\(\\d+\\)", "", QP_PDF_text)
  QP_PDF_text    <- gsub("\\(\\d+\\s*[Xx]\\s*\\d+\\)", "", QP_PDF_text)
  QP_PDF_text    <- gsub("\\[\\d+\\]", "", QP_PDF_text)
  QP_PDF_text    <- gsub("\\[\\d+\\s*[Xx]\\s*\\d+\\]", "", QP_PDF_text)
#5 
  extr_list      <- nchar(QP_PDF_text) > 0 & !grepl("^\\s*$", QP_PDF_text)
  QP_PDF_text    <- QP_PDF_text[extr_list]
#6
  QP_PDF_text    <- rev(QP_PDF_text)
#7
  Memo_PDF_text  <- pdf_text(memo_url) %>% readr::read_lines() %>% str_trim() %>% tolower()
#8.
  Regex_S        <- paste0("^",toString(QPnMemo_DF$regex_start[row])," ")
  regex_E       <- paste0("^",toString(QPnMemo_DF$regex_end[row])," ")
  inx_S          <- 0
  inx_E          <- 0
  for (x in 1:length(QP_PDF_text))
  {
   if (str_detect(QP_PDF_text[x], regex_S))
    {
      if (inx_S == 0)
      {
       inx_S <- x
      }
    }
   if (str_detect(QP_PDF_text[x], regex_E))
    {
      inx_E <- x - 1  
    }
  }
  if (inx_S == 0 | inx_E == 0)
  {
    message("error for ", QPnMemo_DF$DFkey[row], "one or more index = 0 inx_S ", inx_S, " inx_E ", inx_E )
    next
  } 
  
  QP_text_mc   <- QP_PDF_text[inx_E:inx_S ]
  QP_text_mc   <- rev(QP_text_mc)
  inx_E        <- 0
  for (x in 1:length(QP_text_mc))
  {
    if (str_detect(QP_text_mc[x], regex_E))
    {
      if (inx_E == 0)
      {
        inx_E <- x - 1
      }
    }
  }
  QP_text_mc   <- QP_text_mc[1:inx_E]
#-----------------------------------------------------------------------------------------
# build Json table for flutter 
#-----------------------------------------------------------------------------------------
# build Json Language string 
#
  LangString <- NULL
  LangString <- paste0(LangString,'"language":')
  LangString <- paste0(LangString,'"')
  LangString <- paste0(LangString,QPnMemo_DF$language.x[row])
  LangString <- paste0(LangString,'",')
#  
# build Json Subject string 
#
  SubjString <- NULL
  SubjString <- paste0(SubjString,'"subject":')
  SubjString <- paste(SubjString,'"')
  SubjString <- paste0(SubjString,QPnMemo_DF$Module_name.y.x[row])
  SubjString <- paste0(SubjString,'",')
#  
# build Json Grade  string hardcoded it must be corrected  
#
  GradeString <- NULL
  GradeString <- paste0(GradeString,'"grade": "Grade 12",')
#  
# build Json Paper string pdf
#
  PaperString <- NULL
  PaperString <- paste0(PaperString,'"paper":')
  PaperString <- paste(PaperString,'"')
  PaperString <- paste0(PaperString,QPnMemo_DF$DFkey2.x[row])
  PaperString <- paste(PaperString,str_sub(QPnMemo_DF$Exam_Sitting.x[row],1,3))
  PaperString <- paste(PaperString,str_sub(QPnMemo_DF$Exam_Year.x[row],-4,-1))
  PaperString <- paste0(PaperString,'",')
#  
# build Questions  
#
  PasteString    <- NULL
  Option_count   <- 1
  Options_bool   <- FALSE
  Questions_bool <- FALSE
  
  options_string_found     <- FALSE
  answer_string_found      <- FALSE

  row01          <- 1
  rm(Q_df)
  Q_df <- data.frame()

  for (row01 in 1:length(QP_text_mc))
  {
#
    if (grepl("[1-9]\\.[1-9]\\.[1-99]", QP_text_mc[row01]))
    {
# if not first break write out the options closing bracket
      if (row01 != 1)  
      {
        IDString  <- NULL
        IDCntr    <- IDCntr + 1
        IDString  <- paste0('{ "id":', IDCntr, ',')
        Q_df[nrow(Q_df) + 1,1] = paste0(IDString)
        Q_df[nrow(Q_df) + 1,1] = paste0(LangString)
        Q_df[nrow(Q_df) + 1,1] = paste0(SubjString)
        Q_df[nrow(Q_df) + 1,1] = paste0(GradeString)
        Q_df[nrow(Q_df) + 1,1] = paste0(PaperString)
        Q_df[nrow(Q_df) + 1,1] = paste0(Question_String)
        
        Options_String        <- paste0(Options_String,"],")
        AnswerString          <- paste0('"answer_index":', Answer_index, ",},")
        Q_df[nrow(Q_df) + 1,1] = paste0(Options_String)
        Q_df[nrow(Q_df) + 1,1] = paste0(AnswerString)
        if ( answer_string_found && options_string_found)
        {
          for (x in 1:nrow(Q_df))
          {
            Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] <- Q_df[x,1]
          }
        }
        rm(Q_df)
        Q_df <- data.frame()
        answer_string_found     <- FALSE
        options_string_found    <- FALSE
      }
      Questions_bool   <- TRUE
      Options_bool     <- FALSE
      Question_String  <- NULL
      
      Question_String <- paste0(Question_String,'"Question":"',QP_text_mc[row01])
      Question_String_7 <- substr(QP_text_mc[row01],start=1,stop=8)
      Question_Number <- paste(trimws(Question_String_7)," ")
      if (length(which(grepl(Question_Number, Memo_PDF_text)) > 0))
      {
        answer_string_found      <- TRUE
        first_occurence <- which(grepl(Question_Number, Memo_PDF_text))[1]
        Full_string     <- Memo_PDF_text[first_occurence]
        Qno_start_pos   <- str_locate(Full_string, Question_Number)
        Answer_string   <- substr(Full_string, start = Qno_start_pos[1,2]+1,stop = nchar(Full_string))
        Answer_string_trimmed  <- str_squish(Answer_string)
        Answer_letter <- substr(Answer_string_trimmed,start= 1,stop = 1)
        Answer_index <- grep(Answer_letter, c("a","b","c","d","e","f","g","h","i","j"))
      } else {
        Question_String_7 <- substr(QP_text_mc[row01],start=1,stop=7)
        Question_Number   <- Question_String_7
        if (length(which(grepl(Question_Number, Memo_PDF_text)) > 0))
        {
          answer_string_found      <- TRUE
          first_occurence <- which(grepl(Question_Number, Memo_PDF_text))[1]
          Full_string     <- Memo_PDF_text[first_occurence]
          startnend       <- str_locate(Full_string, Question_Number)
          Qno_start_pos   <- str_locate(Full_string, Question_Number)
          Answer_string   <- substr(Full_string, start = Qno_start_pos[1,2]+1,stop = nchar(Full_string))
          Answer_string_trimmed  <- str_squish(Answer_string)
          Answer_letter <- substr(Answer_string_trimmed,start= 1,stop = 1)
          Answer_index <- grep(Answer_letter, c("a","b","c","d","e","f","g","h","i","j"))
        } 
      }
      next
    }
    if (grepl("^[a-d]\\s\\s", QP_text_mc[row01]))
    {
      options_string_found <- TRUE
      if (Questions_bool) 
      {
        Question_String <- paste0(Question_String,"',")
        Questions_bool <- FALSE
        Options_bool   <- TRUE
        Options_String <- NULL
        Options_String <- paste0(Options_String,'"Options":[')
        Options_String <- paste0(Options_String,"'",QP_text_mc[row01],"'")
        next
      }
    }
    if (Questions_bool)
      {
        Question_String <- paste0(Question_String," ",QP_text_mc[row01])
        next
      }
    if (Options_bool)
      {
        Options_String <- paste0(Options_String,",'",QP_text_mc[row01],"'")
        next
      }
  }
  IDString <- NULL
  IDCntr <- IDCntr + 1
  IDString <- paste0('{ "id":', IDCntr, ',')
  Options_String <- paste0(Options_String,"],")
#  message("before squish", Options_String)
  Options_String <- str_squish(Options_String)
#  message("after squish", Options_String)
  AnswerString <- paste0('"answer_index":', Answer_index, ",},")
  Q_df[nrow(Q_df) + 1,1] = paste0(IDString)
  Q_df[nrow(Q_df) + 1,1] = paste0(LangString)
  Q_df[nrow(Q_df) + 1,1] = paste0(SubjString)
  Q_df[nrow(Q_df) + 1,1] = paste0(GradeString)
  Q_df[nrow(Q_df) + 1,1] = paste0(PaperString)
  Q_df[nrow(Q_df) + 1,1] = paste0(Question_String)
  Q_df[nrow(Q_df) + 1,1] = paste0(Options_String)
  Q_df[nrow(Q_df) + 1,1] = paste0(AnswerString)
  for (x in 1:nrow(Q_df))
  {
    Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] <- Q_df[x,1]          
  }
  rm(Q_df)
  Q_df <- data.frame()
}
write.csv(Freedu_App_Fmt,"jsonfmt3.csv", row.names = FALSE, quote = FALSE)
