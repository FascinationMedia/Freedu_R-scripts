
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
#for (row in 1:5) {  
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
    message (typeof(Annual_link_HTML))
    message (Annual_link_HTML %>% html_nodes(".eds_containerTitle"))
    message(" ")
    message (Annual_link_HTML %>% html_nodes(".eds_containerTitle") %>% html_attr("id")%>% substr(9,4))
    message(" ")
    message (Annual_link_HTML %>% html_nodes(".eds_containerTitle") %>% html_text())
    l_subjects <- Annual_link_HTML %>% html_nodes(".eds_containerTitle") 
#  l_subjects <- as.list(Annual_link_HTML %>% html_nodes(".eds_containerTitle") %>% html_attr("id"))
  message (typeof(l_subjects))
  subject_df <- data.frame( 
    Module_No =
      Annual_link_HTML %>% html_nodes(".eds_containerTitle") %>% html_attr("id"),
    Module_name = Annual_link_HTML %>% html_nodes(".eds_containerTitle") %>% html_text())
  message("merging")
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
Question_df
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
QPnMemo_DF_temp <- as.data.frame(read.csv("DownloadList.csv", sep = ',', header = TRUE ))
#QPnMemo_DF <- QPnMemo_DF_temp[QPnMemo_DF_temp$Exam_Year.x>2014,]
QPnMemo_DF <- QPnMemo_DF_temp[QPnMemo_DF_temp$Exam_Year.x > 2014,]
Freedu_App_Fmt <- data.frame()
IDCntr <- 0 
#for (row in 1:nrow(DF_tempCombined)) 
#row <- 1 
for (row in 1:20)
# for (row in 1:nrow(QPnMemo_DF))
  {
#
# check if the question paper and memo renders correctly.   
#  if not skip the question paper and memo 
#
  message("question paper ", QPnMemo_DF$DFkey[row] )
  qp_url  <- QPnMemo_DF$QP_URL[row]
  memo_url  <- QPnMemo_DF$Memo_URL[row]

#
# QUESTION PAPER  
# request URL
# extract text from PDF
# scrub the data
  QP_PDF_text <- pdf_text(qp_url) %>% readr::read_lines() %>% str_trim()
  QP_PDF_text <- tolower(QP_PDF_text) 
#  QP_PDF_text <- QP_PDF_text[grep("\\S", QP_PDF_text)]
  QP_PDF_text <- gsub("\\(\\d+\\)", "", QP_PDF_text)
  QP_PDF_text <- gsub("\\(\\d+\\s*[Xx]\\s*\\d+\\)", "", QP_PDF_text)
  temp_df <- data.frame(QP_PDF_text)
# remove non question related content from pdf
  subject    <- ("copyright|nsc|nss|kopiereg|please turn|blaai om|dbe/|dbo/|doe/")
  not_req_string <-  str_detect(temp_df, subject)
  temp_df <-  temp_df[!not_req_string]
  temp_df <-  temp_df[temp_df$QP_text != "",]
  temp_df <-  rev(temp_df)
  temp_df <- data.frame(temp_df)
  temp_df <-  temp_df[!not_req_string]
  #
  #-------------------------------------------------------------------------------
  # setup end regex
  #-------------------------------------------------------------------------------
  #   
  regex_S     <- paste0("^",QPnMemo_DF$regex_start[row]," ")
#  strt_inx    <- (str_detect(QP_text, regex_S))
  #   
  #-------------------------------------------------------------------------------
  # setup start regex
  #-------------------------------------------------------------------------------
  #   
  regex_end <- paste0("^",toString(QPnMemo_DF$regex_end[row])," ")
#  end_inx <- (str_detect(QP_text, regex_end))
  #   
  #-------------------------------------------------------------------------------
  
  for (x in 1:nrow(temp_df))
  {
   message("x = ", x, "str_detect ", str_detect(temp_df$temp_df[x], regex_end))
  }
  
# remove non question related content from pdf
#  QP_text_tidy <-  data.frame(QP_text_with_empties[QP_text_with_empties != ""])
# reverse the order.
#  QP_text_rev  <- rev(QP_text_tidy)
#  QP_text_rev <- QP_text_tidy[rev(seq_len(nrow(QP_text_tidy))), , drop = FALSE]
  
#
# MEMO   
# request URL
# extract text from PDF
# scrub the data
#  
  memo_url  <- QPnMemo_DF$Memo_URL[row]
  memo_text <- pdf_text(memo_url) %>% readr::read_lines() %>% str_trim()
  memo_text <- gsub("\\(\\d+\\)", "", memo_text)
  memo_text <- gsub("\\(\\d+\\s*[Xx]\\s*\\d+\\)", "", memo_text)
  
#
#-------------------------------------------------------------------------------
# find multiple choice start
#-------------------------------------------------------------------------------
#   
   regex_S     <- paste0("^",QPnMemo_DF$regex_start[row]," ")
   strt_inx    <- (str_detect(QP_text, regex_S))
#   
#-------------------------------------------------------------------------------
# find multiple choice end
#-------------------------------------------------------------------------------
#   
   regex_end <- paste0("^",toString(QPnMemo_DF$regex_end[row])," ")
   end_inx <- (str_detect(QP_text, regex_end))
#   
#-------------------------------------------------------------------------------
# extract all multiple choice using above start and end 
#-------------------------------------------------------------------------------
# 
  tryCatch(
    {
      temp_DF <- QP_text[which(strt_inx):(which(end_inx))-1] %>% tolower()
    },
    error = function(err)
    {
      message("start end error is :")
      message(err)
      temp_DF <- ""
    }
  )
# remove non question related content from pdf
  subject    <- ("copyright|nsc|nss|kopiereg|please turn|blaai om|dbe/|dbo/|doe/")
  not_req_string <-  str_detect(temp_DF, subject)
  QP_text_with_empties <-  temp_DF[!not_req_string]
# remove non question related content from pdf
  QP_text_tidy <-  data.frame(QP_text_with_empties[QP_text_with_empties != ""])
  QP_text_rev  <- rev(QP_text_tidy)
# reverse the order.
  QP_text_rev <- QP_text_tidy[rev(seq_len(nrow(QP_text_tidy))), , drop = FALSE]
  
#-----------------------------------------------------------------------------------------
# build Json table for flutter 
#-----------------------------------------------------------------------------------------
  
#
# build Json Language string 
#
  
  LangString <- NULL
  LangString <- paste0(LangString,'"language":')
  LangString <- paste0(LangString,'"')
  LangString <- paste0(LangString,QPnMemo_DF$language.x[row])
  LangString <- paste0(LangString,'",')
#  Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(LangString)
#  
# build Json Subject string 
#
  SubjString <- NULL
  SubjString <- paste0(SubjString,'"subject":')
  SubjString <- paste(SubjString,'"')
  SubjString <- paste0(SubjString,QPnMemo_DF$Module_name.y.x[row])
  SubjString <- paste0(SubjString,'",')
#  Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(SubjString)

#  
# build Json Grade  string hardcoded it must be corrected  
#
  GradeString <- NULL
  GradeString <- paste0(GradeString,'"grade": "Grade 12",')
#  Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(GradeString)
#  
# build Json Paper string pdf
#
  PaperString <- NULL
  PaperString <- paste0(PaperString,'"paper":')
  PaperString <- paste(PaperString,'"')
  PaperString <- paste0(PaperString,QPnMemo_DF$DFkey2.x[row])
  PaperString <- paste(PaperString,str_sub(QPnMemo_DF$Exam_Year.x[row],-2,-1))
  PaperString <- paste(PaperString,str_sub(QPnMemo_DF$Exam_Sitting.x[row],1,3))
  PaperString <- paste0(PaperString,'",')
#  Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(PaperString)
#  
# build Questions  
#
  PDF_List_vector <- unlist(QP_text_rev)
  Q_regex <- regex("[1-9]\\.[1-9]\\.[1-9]")
  O_regex <- regex("^[a-d]\\s\\s")
  Question <- c("nullquestion")
  Answers <- c("nullanswer")
  First_Read_bool <- TRUE
  PasteString <- NULL
  Option_count <- 1
  Options_bool <- TRUE
  Questions_bool <- FALSE
  row01 <- 1

  for (row01 in 1:nrow(QP_text_rev))
  {
  if (Options_bool)
  {
    if(Option_count < 5)
      {
        if(Option_count == 1)
          {
            IDString <- NULL
            IDString <- paste0(IDString,'"ID":')
            IDString <- paste0(IDString,'"')
            IDCntr <- IDCntr + 1
            IDString <- paste0(IDString, IDCntr)
            IDString <- paste0(IDString,'",')
            Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(IDString)
            Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(LangString)
            Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(SubjString)
            Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(GradeString)
            Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(PaperString)
            PasteString <- paste0(PasteString,'"Options":')
            PasteString <- paste(PasteString,'[')
          }
          PasteString <- paste(PasteString,"'")
          PasteString <- paste(PasteString,QP_text_rev[row01,1])
          PasteString <- paste(PasteString,"',")
          Option_count <- Option_count + 1 
      }
    else 
      {
      PasteString <- paste(PasteString,'],')
      PasteString <- gsub("\\(\\)", "", PasteString)
      Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(PasteString)
      Option_count    <- 1
      Options_bool    <- FALSE
      Questions_bool  <- TRUE
      PasteString <- paste0('",')
      }
  }
    if (Questions_bool)
    {
      PasteString <- paste0(paste(QP_text_rev[row01,1]),PasteString)
      if(str_detect(QP_text_rev[row01,1],Q_regex)) 
      {
        PasteString <- paste0('"question": "',PasteString)
        Options_bool <- TRUE
        Option_count <- 1 
        Questions_bool <- FALSE
        Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(PasteString)
        PasteString <- ""
        Question_String <- str_trim(str_sub(QP_text_rev[row01,1],1,6))
        Answer_List <- memo_text[str_detect(memo_text, Question_String)]
        ans_inx <- unlist(gregexpr(Question_String, Answer_List))[1]
        Answer_option <- str_sub(Answer_List,ans_inx,-1L)
        Answer_option <- str_replace_all(Answer_option, "[^a-zA-Z0-9.]", " ")
        Answer_option <- str_trim(str_replace_all(Answer_option, Question_String, ""))
        Answer_option <- str_sub(Answer_option[1],1,1)
        AnsString <- NULL
        AnsString <- paste0(AnsString,'"answer_index":')
        AnsString <- paste0(AnsString,'"')
        AnsString <- paste0(AnsString,Answer_option)
        AnsString <- paste0(AnsString,'",')
        Freedu_App_Fmt[nrow(Freedu_App_Fmt) + 1,1] = paste0(AnsString)
        }
    }
  }
}
write.csv(Freedu_App_Fmt,"jsonfmt2.csv", row.names = FALSE, quote = FALSE)

