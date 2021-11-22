#-------------------------------------------------------------------------------
# included libraries 
#-------------------------------------------------------------------------------

library(rvest)
library(stringr)
library(dplyr)
library(pdftools)

DOE_PastPaper_Home_URL   <- "https://www.education.gov.za/Curriculum/NationalSeniorCertificate(NSC)Examinations/NSCPastExaminationpapers.aspx"
DOE_PastPaper_Home_URL
DOE_PastPaper_All_URL <- data.frame( 
 
    URL = read_html(DOE_PastPaper_Home_URL)  
      %>% html_nodes("a") 
      %>% html_attr("href")
      %>% url_absolute(DOE_PastPaper_Home_URL),
                                        
    URL_text = read_html(DOE_PastPaper_Home_URL) 
      %>% html_nodes("a") 
      %>% html_text() )

DOE_PastPaper_Exam_URLs <- filter(DOE_PastPaper_All_URL, str_detect(DOE_PastPaper_All_URL$URL_text, regex("exam", ignore_case= TRUE)))
DOE_PastPaper_Exam_URLs <-  filter(DOE_PastPaper_Exam_URLs, str_detect(DOE_PastPaper_Exam_URLs$URL_text, regex("\\d{4}")))
                                                        
DOE_PastPaper_Exam_URLs <- DOE_PastPaper_Exam_URLs %>% mutate(Exam_year = as.numeric(str_extract(DOE_PastPaper_Exam_URLs$URL_text,"\\d{4}")), Sitting = NA)   

DOE_PastPaper_Exam_URLs$Sitting <- ifelse(str_detect(DOE_PastPaper_Exam_URLs$URL_text, regex("nov", ignore_case = TRUE)) 
                                      & is.na(DOE_PastPaper_Exam_URLs$Sitting),"November",DOE_PastPaper_Exam_URLs$Sitting)
                                                            
                                                              
DOE_PastPaper_Exam_URLs$Sitting <- ifelse(str_detect(DOE_PastPaper_Exam_URLs$URL_text, regex("feb", ignore_case = TRUE)) 
                                      & is.na(DOE_PastPaper_Exam_URLs$Sitting),"February",DOE_PastPaper_Exam_URLs$Sitting)
                                                              
DOE_PastPaper_Exam_URLs$Sitting <- ifelse(str_detect(DOE_PastPaper_Exam_URLs$URL_text, regex("may", ignore_case = TRUE)) 
                                    & is.na(DOE_PastPaper_Exam_URLs$Sitting),"May",DOE_PastPaper_Exam_URLs$Sitting)

DOE_PastPaper_Exam_URLs
DOE_PastPaper_Exam_links <- as.list(DOE_PastPaper_Exam_URLs$URL) 

DF_All_Subject_Link_S <- data.frame()
for (row in 1:nrow(DOE_PastPaper_Exam_URLs)) {
# for (row in 1:3) {  
  print(DOE_PastPaper_Exam_URLs$URL[row])
  Annual_link                    <- DOE_PastPaper_Exam_URLs$URL[row]
  Annual_link_HTML               <- read_html(Annual_link)
  Title_text                     <- Annual_link_HTML %>% html_nodes("title") %>% html_text %>% str_replace_all("\\r", "") %>% str_replace_all("\\n", "") %>% str_replace_all("\\t", "")
  Annual_link_Subjects           <- Annual_link_HTML %>% html_nodes(".eds_containerTitle")                              
  DF_Annual_link_Subject_Name    <- data.frame( Module_No = as.numeric(str_extract_all(Annual_link_Subjects, "\\d{4}")), Module_name = html_text(Annual_link_Subjects))
  DF_Annual_link_Subject_Link    <- data.frame(
 ##   PDF_URL = Annual_link_HTML %>%  html_nodes(".TitleCell") %>%  html_nodes("a") %>% html_attr("href") %>% url_absolute(DOE_PastPaper_Exam_URLs$URL[row]),
##    PDF_URL = Annual_link_HTML %>%  html_nodes(".TitleCell") %>%  html_nodes("a") %>% html_attr("href") %>% url_absolute(DOE_PastPaper_Exam_URLs$URL[row]),
    PDF_URL = Annual_link_HTML %>%  html_nodes(".TitleCell") %>%  html_nodes("a") %>% html_attr("href") %>% url_absolute(Annual_link),
    Subject_Name = Annual_link_HTML %>% html_nodes(".TitleCell")  %>% html_nodes("a") %>% html_text() )
  
  DF_Annual_link_Subject_Link    <- DF_Annual_link_Subject_Link %>% 
     mutate (Module_No = str_extract(PDF_URL,"\\d{4}$"),
            Grade=c("12"), 
            Year_title = c(Title_text),  
            Paper_type = NA,
            Paper_Language = NA)
  
  DF_Annual_link_Subject_Link_S  <- merge(x=DF_Annual_link_Subject_Link, y=DF_Annual_link_Subject_Name, by="Module_No")

  DF_All_Subject_Link_S <- rbind(DF_All_Subject_Link_S, DF_Annual_link_Subject_Link_S )
  
  rm(Annual_link_HTML)
  rm(Annual_link_Subjects)
  rm(DF_Annual_link_Subject_Name)
  rm(DF_Annual_link_Subject_Link)
}

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
DF_All_Subject_Link_S

DF_All_Subject_Link_S$Paper_No <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex("paper 1", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_No), "paper 1", DF_All_Subject_Link_S$Paper_No) 
DF_All_Subject_Link_S$Paper_No <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex("paper 2", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_No), "paper 2", DF_All_Subject_Link_S$Paper_No)
DF_All_Subject_Link_S$Paper_No <- ifelse(str_detect(DF_All_Subject_Link_S$Subject_Name, regex("paper 3", ignore_case = TRUE)) & is.na(DF_All_Subject_Link_S$Paper_No), "paper 3", DF_All_Subject_Link_S$Paper_No) 
DF_All_Subject_Link_S$paper  <- tolower(paste(DF_All_Subject_Link_S$Module_name, DF_All_Subject_Link_S$Paper_No))

DF_All_Subject_Link_S
my_data <- as.data.frame(read.table("list of paper with multichoice.txt", sep = '/', header = TRUE ))
my_data <- my_data %>% mutate(my_data =tolower(paste(my_data$subject, my_data$paper_no)) ) 
my_data

DF_final  <- merge(x=DF_All_Subject_Link_S, y=my_data, by="paper")
DF_final
typeof(my_data)

write.csv(DF_All_Subject_Link_S,"Freeedu.csv", row.names = FALSE)
#
# extract papers with multiple choice questions 
# 

DF_All_Subject_Link_2 <- DF_All_Subject_Link_S %>%  filter(toupper(DF_All_Subject_Link_S$Module_name) == "AGRICULTURAL MANAGEMENT PRACTICES")
#DF_All_Subject_Link_2 <- DF_All_Subject_Link_2 %>%  filter(!(tolower(DF_All_Subject_Link_2$Module_name) == "english")) 
#DF_All_Subject_Link_2 <- DF_All_Subject_Link_2 %>%  filter(!(tolower(DF_All_Subject_Link_2$Module_name) == "xitsonga")) 
#DF_All_Subject_Link_2 <- DF_All_Subject_Link_2 %>%  filter(!(tolower(DF_All_Subject_Link_2$Module_name) == "tshivenda")) 
#DF_All_Subject_Link_2 <- DF_All_Subject_Link_2 %>%  filter(!(tolower(DF_All_Subject_Link_2$Module_name) == "siswati")) 
#DF_All_Subject_Link_2 <- DF_All_Subject_Link_2 %>%  filter(!(tolower(DF_All_Subject_Link_2$Module_name) == "sepedi")) 
#DF_All_Subject_Link_2 <- DF_All_Subject_Link_2 %>%  filter(!(tolower(DF_All_Subject_Link_2$Module_name) == "setswana"))
#DF_All_Subject_Link_2 <- DF_All_Subject_Link_2 %>%  filter(!(tolower(DF_All_Subject_Link_2$Module_name) == "accounting"))

DF_All_Subject_Link_2
