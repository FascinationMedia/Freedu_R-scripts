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
# 1. Read the links from the save spreadsheet.
# 2. add some new coulumns
# 3. Name the columns
#1. 
DF_All_Subject_Link_S <- as.data.frame(read.csv("PastpaperYearSubjectsLink.csv", sep = ',', header = TRUE ))
#2. 
DF_All_Subject_Link_S['Paper_Language'] <- NA
DF_All_Subject_Link_S['Paper_Type'] <- NA
DF_All_Subject_Link_S['Paper_No'] <- NA
DF_All_Subject_Link_S['Paper_HLFALSAL'] <- NA
DF_All_Subject_Link_S['Paper_Region'] <- NA
DF_All_Subject_Link_S['filename'] <- NA
#3. 
colnames(DF_All_Subject_Link_S) <- c('Module_No','Exam_Year','Exam_Sitting','Title','Paper','Module_URL','Subject','Paper_Language', 'Paper_type', 'Paper_No', 'HL_FAL_SAL', 'Region')
#
# tidy the data 
# expand the P1/P2/P3 
# Expand the Memo 1/2/3
DF_All_Subject_Link_S$Paper <- tolower(DF_All_Subject_Link_S$Paper)
DF_All_Subject_Link_S$Paper <- str_replace(DF_All_Subject_Link_S$Paper, "p1", "paper 1")
DF_All_Subject_Link_S$Paper <- str_replace(DF_All_Subject_Link_S$Paper, "p2", "paper 2")
DF_All_Subject_Link_S$Paper <- str_replace(DF_All_Subject_Link_S$Paper, "p3", "paper 3")
DF_All_Subject_Link_S$Paper <- str_replace(DF_All_Subject_Link_S$Paper, "memo 1", "memo paper 1")
DF_All_Subject_Link_S$Paper <- str_replace(DF_All_Subject_Link_S$Paper, "memo 2", "memo paper 2")
DF_All_Subject_Link_S$Paper <- str_replace(DF_All_Subject_Link_S$Paper, "memo 3", "memo paper 3")

#1. populate the language. 
DF_All_Subject_Link_S$Paper_Language <- DF_All_Subject_Link_S$Subject
if (Prod_Run) 
{
  Rows2Process <-   nrow(DF_All_Subject_Link_S)
} else {
  Rows2Process <-   Rows2Test
} 
#for (row in 85:90) {
for (row in 1:Rows2Process) 
  {
# populate paper language
    if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("afrikaans", ignore_case = TRUE)) &
       str_detect(DF_All_Subject_Link_S$Paper[row], regex("english", ignore_case = TRUE)))
    {
      DF_All_Subject_Link_S$Paper_Language[row] = "english and afrikaans"                          
    } else {
      if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("afrikaans", ignore_case = TRUE)))
      {
        DF_All_Subject_Link_S$Paper_Language[row] = "afrikaans"                          
      } else {
        if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("english", ignore_case = TRUE)))
        {
          DF_All_Subject_Link_S$Paper_Language[row] = "english"                          
        } else {DF_All_Subject_Link_S$Paper_Language[row] =  tolower(DF_All_Subject_Link_S$Subject[row])}
    }
    }
# populate paper fal / sal / hl 
  if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("hl", ignore_case = TRUE)))
  {
    DF_All_Subject_Link_S$HL_FAL_SAL[row] = "hl"                          
  } else {
    if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("fal", ignore_case = TRUE)))
    {
      DF_All_Subject_Link_S$HL_FAL_SAL[row] = "fal"                          
    } else {
      if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("sal", ignore_case = TRUE)))
      {
        DF_All_Subject_Link_S$HL_FAL_SAL[row] = "sal"                          
      } else { DF_All_Subject_Link_S$HL_FAL_SAL[row] = "" }
    }
  }
# populate paper number
  if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("paper 1", ignore_case = TRUE)))
  {
    DF_All_Subject_Link_S$Paper_No[row] = "paper 1"                          
  } else {
    if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("paper 2", ignore_case = TRUE)))
    {
      DF_All_Subject_Link_S$Paper_No[row] = "paper 2"                          
    } else {
      if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("paper 3", ignore_case = TRUE)))
      {
        DF_All_Subject_Link_S$Paper_No[row] = "paper 3"                          
      }
    }
  }
# populate paper region
  DF_All_Subject_Link_S$Region[row] = ""
  if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("eastern cape", ignore_case = TRUE)))
  {
    DF_All_Subject_Link_S$Region[row] = "eastern cape"                          
  }
  if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("free state", ignore_case = TRUE)))
  {
    DF_All_Subject_Link_S$Region[row] = "eastern cape"                          
  }
  if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("gauteng", ignore_case = TRUE)))
  {
    DF_All_Subject_Link_S$Region[row] = "gauteng"                          
  }
  if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("natal", ignore_case = TRUE)))
  {
    DF_All_Subject_Link_S$Region[row] = "kwazulu-natal"                          
  }
  if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("kwa", ignore_case = TRUE)))
  {
    DF_All_Subject_Link_S$Region[row] = "kwazulu-natal"                          
  }
  if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("limpopo", ignore_case = TRUE)))
  {
    DF_All_Subject_Link_S$Region[row] = "limpopo"                          
  }
  if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("mpumalanga", ignore_case = TRUE)))
  {
    DF_All_Subject_Link_S$Region[row] = "mpumalanga"                          
  }
  if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("north west", ignore_case = TRUE)))
  {
    DF_All_Subject_Link_S$Region[row] = "north west"                          
  }
  if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("northwest", ignore_case = TRUE)))
  {
    DF_All_Subject_Link_S$Region[row] = "north west"                          
  }
  if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("northern", ignore_case = TRUE)))
  {
    DF_All_Subject_Link_S$Region[row] = "northern cape"                          
  }
  if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("western", ignore_case = TRUE)))
  {
    DF_All_Subject_Link_S$Region[row] = "western cape"                          
  }
# populate paper typememo
  if(str_detect(DF_All_Subject_Link_S$Paper[row], regex("memo", ignore_case = TRUE)))
  {
    DF_All_Subject_Link_S$Paper_type[row] = "memo"                          
  } else { 
    DF_All_Subject_Link_S$Paper_type[row] = ""                          
    }
#file name 
  fname <- paste("C:/DEV/R/data/pastpapers/",
                 DF_All_Subject_Link_S$Exam_Year[row], 
                 " ",
                 DF_All_Subject_Link_S$Exam_Sitting[row],
                 " ",
                 DF_All_Subject_Link_S$Subject[row],
                 " ",
                 DF_All_Subject_Link_S$Paper_No[row],
                 " ",
                 DF_All_Subject_Link_S$Paper_Language[row],
                 " ",
                 DF_All_Subject_Link_S$HL_FAL_SAL[row],
                  " ",
                 DF_All_Subject_Link_S$Region[row],
                 " ",
                  DF_All_Subject_Link_S$Paper_type[row])  
  DF_All_Subject_Link_S$filename[row] <- str_squish(fname)
              
    }

temp_DF <- data.frame()
if (Prod_Run) 
{
  Rows2Process <-   nrow(DF_All_Subject_Link_S)
} else {
  Rows2Process <-   Rows2Test
} 
#for (row in 1:30) {
for (row in 3085:Rows2Process) {
  
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
  
  if(file_extension == " ") 
  { 
    message (" skipped entry ", row, " file type ", tmp_fld, "filename ", fname )
    next 
  }
  
  fname <- paste(DF_All_Subject_Link_S$filename[row],
                 file_extension
  )
  #    
  file.copy(TempFile, fname, overwrite = TRUE)
  # Remove the temporary file
  unlink(TempFile)
  message ("added paper no ", row, " file ", fname)
}
write.csv(DF_All_Subject_Link_S,"All_QP_ALL_SUBJECT.csv", row.names = FALSE)

