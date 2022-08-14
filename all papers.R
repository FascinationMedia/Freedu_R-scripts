
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
for (row in 1:nrow(DF_PastpaperYear)) {
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
  View(DF_YearSubject)
    message("building dataframe subjects")
  l_subjects <- Annual_link_HTML %>% html_nodes(".eds_containerTitle")
  subject_df <- data.frame( Module_No = as.numeric(str_extract_all(l_subjects, "[0-9]{4}")), 
                            Module_name = html_text(l_subjects))
  message("merging")
  df_FULL <-  merge(x=DF_YearSubject, y=subject_df, by="Module_No")
  DF_All_Subject_Link_S <- rbind(DF_All_Subject_Link_S, df_FULL )
}
# 
write.csv(DF_All_Subject_Link_S,"PastpaperYearSubjectsLink.csv", row.names = FALSE)

rm(DOE_PastPaperYearURL)
rm(DF_PastpaperYear)
rm(Annual_link_Subjects_HTML)
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

library(httr)
temp_DF <- data.frame()
for (row in 1:nrow(DF_final2)) {
# for (row in 1:20)     { 
  r <- GET(DF_final2$Module_URL[row])
  temp_DF <- rbind(temp_DF, cbind(type=r$headers$`content-type`,Module_URL=DF_final2$Module_URL[row]))
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
Question_df <- data.frame()
Question_df

for (row in 1:nrow(DF_OnlyPDFs)) {
#for (row in 1:10) { 
  message( 'row no', row)
  message( 'URL>', DF_OnlyPDFs$PDF_URL[row], '</URL')
  paper_link  <- DF_OnlyPDFs$PDF_URL[row]
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
Question_df
write.csv(temp_DF,"Dwnld_QPs.csv", row.names = FALSE)
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
#  read document to check if it is a PDF. 
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
DF_Final <- as.data.frame(read.csv("DownloadList.csv", sep = ',', header = TRUE ))
Freedu_App_Fmt <- data.frame()
#for (row in 1:norows(DF_tempCombined)) 
for (row in 1:10)
{
message( 'row no', row)
#message( 'URL>', DF_OnlyPDFs$PDF_URL[row], '</URL')
paper_link  <- DF_Final$QP_URL[row]
PDF_list    <- pdf_text(paper_link) %>% readr::read_lines() %>% str_trim()
#
#-------------------------------------------------------------------------------
# find multiple choice start
#-------------------------------------------------------------------------------
#   
   regex_S     <- paste0("^",DF_Final$regex_start[row]," ")
   strt_inx    <- (str_detect(PDF_list, regex_S))
#   
#-------------------------------------------------------------------------------
# find multiple choice end
#-------------------------------------------------------------------------------
#   
   regex_end <- paste0("^",toString(DF_Final$regex_end[row])," ")
   end_inx <- (str_detect(PDF_list, regex_end))
#   
#-------------------------------------------------------------------------------
# extract all multiple choice using above start and end 
#-------------------------------------------------------------------------------
# 
  tryCatch(
    {
      message("start & end successfull")
      temp_DF <- PDF_list[which(strt_inx):(which(end_inx))-1] %>% tolower()
    },
    error = function(err)
    {
      message("start end error", err)
      temp_DF <- ""
    }
  )
# remove non question related content from pdf
  subject    <- ("copyright|nsc|nss|kopiereg|please turn|blaai om|dbe/|dbo/|doe/")
  not_req_string <-  str_detect(temp_DF, subject)
  PDF_list_with_empties <-  temp_DF[!not_req_string]
# remove non question related content from pdf
  PDF_list_tidy <-  data.frame(PDF_list_with_empties[PDF_list_with_empties != ""])
  PDF_list_rev  <- rev(PDF_list_tidy)
# reverse the order.
  PDF_list_rev <- PDF_list_tidy[rev(seq_len(nrow(PDF_list_tidy))), , drop = FALSE]
  
#-----------------------------------------------------------------------------------------
# build Json table for flutter 
#--2---------------------------------------------------------------------------------------
  
#
# build Json Language string 
#
  TempString <- NULL
  TempString <- paste0(TempString,'"language":')
  TempString <- paste(TempString,'"')
  TempString <- paste0(TempString,DF_Final$language.x[row])
  TempString <- paste0(TempString,'",')
  Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(TempString)
#  
# build Json Subject string 
#
  TempString <- NULL
  TempString <- paste0(TempString,'"subject":')
  TempString <- paste(TempString,'"')
  TempString <- paste0(TempString,DF_Final$Module_name.y.x[row])
  TempString <- paste0(TempString,'",')
  Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(TempString)

#  
# build Json Grade  string hardcoded it must be corrected  
#
  TempString <- NULL
  TempString <- paste0(TempString,'"grade": "Grade 12"')
#  TempString <- paste(TempString,'"')
#  TempString <- paste0(TempString,DF_Final$Module_name.y.x[row])
#  TempString <- paste0(TempString,'",')
  Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(TempString)
#  
# build Json Paper string 
#
  TempString <- NULL
  TempString <- paste0(TempString,'"paper":')
  TempString <- paste(TempString,'"')
  TempString <- paste0(TempString,DF_Final$DFkey2.x[row])
  TempString <- paste(TempString,str_sub(DF_Final$Exam_Year.x[row],-2,-1))
  TempString <- paste(TempString,str_sub(DF_Final$Exam_Sitting.x[row],1,3))
  TempString <- paste0(TempString,'"')
  Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(TempString)
#  
# build Questions  
#
  PDF_List_vector <- unlist(PDF_list_tidy)
  Q_bool <- FALSE
  Q_regex <- regex("^[1-9]\\.[1-9]\\.[1-20]")
  O_bool <- FALSE
  O_regex <- regex("^[a-d]\\S$")
  Question <- NULL
  Answers <- NULL
  for (row01 in 1:10)
  {
    message(str_sub(PDF_List_vector[row01],1,7))
    if(str_detect(PDF_List_vector[row01],Q_regex)) 
    {
       message("detected question start")
    }
  }
}
#  
# build Json ID string 
#
  TempString <- NULL
  TempString <- paste0(TempString,'"ID":')
  TempString <- paste(TempString,'"')
# <format YEAR portion of ID>
  TempString <- paste(TempString,str_sub(DF_Final$Exam_Year.x[row],-2,-1))
# </format YEAR portion of ID>
  # </format YEAR portion of ID>
  TempString   <- ifelse(str_sub(DF_Final$language.x[row],1,1) == "e", 
                         paste0(TempString,"0"),paste0(TempString,"1")) 
  if(row < 010) { TempString <- paste0(TempString,"0")}
  if(row < 100) { TempString <- paste0(TempString,"0")}
  TempString <- paste0(TempString,row)
# </format row portion of ID>
  
  TempString <- paste0(TempString,'"')
  Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(TempString)
#  
# build Json options string 
#
  TempString <- NULL
  QBool <- FALSE
  Q
  Obool <- Fasle
  
  if(str_detect(DF_temp$Module_name.x, regex("Limpopo ",                        ignore_case = TRUE)) & is.na(DF_temp$region), "Limpopo",       DF_temp$region) 
  TempString <- paste0(TempString,'"ID":')
  TempString <- paste(TempString,'"')
  # <format YEAR portion of ID>
  TempString <- paste(TempString,str_sub(DF_Final$Exam_Year.x[row],-2,-1))
# </format YEAR portion of ID>
  TempString   <- ifelse(str_sub(DF_Final$language.x[row],1,1) == "e", 
                         paste0(TempString,"0"),paste0(TempString,"1")) 
  if(row < 010) { TempString <- paste0(TempString,"0")}
  if(row < 100) { TempString <- paste0(TempString,"0")}
  TempString <- paste0(TempString,row)
  # </format row portion of ID>
  
  
  DOE_PastPaper_Exam_URLs <- filter(DOE_PastPaper_URL_per_year, str_detect(DOE_PastPaper_URL_per_year$URL_text, cregex("^\\d{4}")))
  PDFQuestions_enriched <- data.frame(PDF_list_tidy %>% mutate(P_Year = DF_OnlyPDFs$Year[row], subject = DF_OnlyPDFs$Subject_Name[row]))
  Question_df1 <- as.data.frame(Questions_enriched)
  Question_df <- rbind(Question_df, Question_df1)
  #  message(Questions_df)
#}
Question_df
