
###<include libraries>
library(rvest)
library(stringr)
library(dplyr)
library(pdftools)
library(httr)
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

DF_QP_ALL           <- as.data.frame(read.csv("All_QP_ALL_SUBJECT.csv", sep = ',', header = TRUE ))
DF_QP_MC            <- as.data.frame(read.csv("list of paper with multichoice.txt", sep = '/', header = TRUE ))
DF_QP_MC$subject    <- tolower(DF_QP_MC$subject)
DF_QP_MC$paper_no   <- tolower(DF_QP_MC$paper_no)
DF_QP_ALL$Subject   <- tolower(DF_QP_ALL$Subject)
DF_QP_ALL$Paper_No  <- tolower(DF_QP_ALL$Paper_No)
DF_QP_ALL           <- DF_QP_ALL %>% filter(!is.na(DF_QP_ALL$Subject))   
DF_QP_ALL           <- DF_QP_ALL %>% filter(!is.na(DF_QP_ALL$Paper_No))   


DF_QP_ALL$mc_start <- ""
DF_QP_ALL$mc_end   <- ""
DF_QP_ALL$mc   <- FALSE
DF_QP_ALL          <- DF_QP_ALL %>% arrange( Subject, Paper_No)
DF_QP_MC           <- DF_QP_MC %>% arrange( subject, paper_no)
if (Prod_Run) 
  {
    Rows2Process <- nrow(DF_QP_ALL)
  } else 
  {
    Rows2Process <- Rows2Test 
  }

#for (row in 300 :Rows2Process)
for (row in 1 :Rows2Process)
  {
  for (x in 1:nrow(DF_QP_MC)) 
    {
    if (DF_QP_ALL$Subject[row] == DF_QP_MC$subject[x])
    {
      if (DF_QP_ALL$Paper_No[row] == DF_QP_MC$paper_no[x])
      {
        DF_QP_ALL$mc_start[row] <- DF_QP_MC$regex_start[x]
        DF_QP_ALL$mc_end[row]   <- DF_QP_MC$regex_end[x]
        DF_QP_ALL$mc[row]       <- TRUE
      }
    }
}
}
write.csv(DF_QP_ALL,"DF_QP_ALL_MC.csv", row.names = FALSE)

DF_QP_ALL       <- as.data.frame(read.csv("DF_QP_ALL_MC.csv", sep = ',', header = TRUE )) 
DF_QP_MC        <- filter(DF_QP_ALL, DF_QP_ALL$mc)
DF_QP_MC_memos  <- filter(DF_QP_MC, DF_QP_MC$Paper_type == 'memo') 
DF_MC_QPnMemos  <- DF_QP_MC_memos
DF_MC_QPnMemos  <- DF_MC_QPnMemos[0,]
DF_QP_MC_memos[is.na(DF_QP_MC_memos)] <- ""
DF_QP_MC[is.na(DF_QP_MC)] <- ""

for (x in 1:nrow(DF_QP_MC_memos))
{
  for (y in 1:nrow(DF_QP_MC))
  {
    if(DF_QP_MC$Exam_Year[y] == DF_QP_MC_memos$Exam_Year[x])
    {
      if(DF_QP_MC$Exam_Sitting[y] == DF_QP_MC_memos$Exam_Sitting[x])
      {
        if(DF_QP_MC$Subject[y] == DF_QP_MC_memos$Subject[x])
        {
          if(DF_QP_MC$Paper_Language[y] == DF_QP_MC_memos$Paper_Language[x])
          {
            if(DF_QP_MC$Paper_No[y] == DF_QP_MC_memos$Paper_No[x])
            {
              DF_MC_QPnMemos[nrow(DF_MC_QPnMemos)+1,] = DF_QP_MC[y,]
            }
          }
        }  
      }
    } 
  }
}
write.csv(DF_MC_QPnMemos,"DF_MC_QPnMemos.csv", row.names = FALSE)


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
DF_MC_QPnMemos <- as.data.frame(read.csv("DF_MC_QPnMemos.csv",  sep = ',', header = TRUE ))
DF_Memos <- filter(DF_QP_MC, DF_QP_MC$Paper_type == 'memo')
DF_QPs   <- filter(DF_QP_MC, DF_QP_MC$Paper_type != 'memo')
DF_QPnMemos <- left_join(DF_QPs,DF_Memos, by =c("Exam_Year","Exam_Sitting","Subject","Paper_Language","Paper_No"))
Freedu_App_Fmt  <- data.frame()
IDCntr <- 0 
#for (row in 1:nrow(DF_tempCombined)) 
row <- 1 
# for (row in 1:20)
for (row in 1:nrow(DF_QPnMemos))
 {
#
# check if the question paper and memo renders correctly.   
#  if not skip the question paper and memo 
#
  message("question paper ", row, " " , DF_QPnMemos$filename.x[row])
  qp_url    <- paste0(DF_QPnMemos$filename.x[row],DF_QPnMemos$fileext.x[row])
  memo_url  <- paste0(DF_QPnMemos$filename.y[row],DF_QPnMemos$fileext.y[row])
}
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
  Regex_S        <- paste0("^",toString(DF_QPnMemos$mc_start.x[row])," ")
  Regex_E        <- paste0("^",toString(DF_QPnMemos$mc_end.x[row])," ")
  inx_S          <- 0
  inx_E          <- 0
  for (x in 1:length(QP_PDF_text))
  {
   if (str_detect(QP_PDF_text[x], Regex_S))
    {
      if (inx_S == 0)
      {
       inx_S <- x
      }
    }
   if (str_detect(QP_PDF_text[x], Regex_E))
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
    if (str_detect(QP_text_mc[x], Regex_E))
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
  LangString <- paste0(LangString,DF_QPnMemos$Paper_Language[row])
  LangString <- paste0(LangString,'",')
#  
# build Json Subject string 
#
  SubjString <- NULL
  SubjString <- paste0(SubjString,'"subject":')
  SubjString <- paste(SubjString,'"')
  SubjString <- paste0(SubjString,DF_QPnMemos$Subject[row])
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
  PaperString <- paste0(PaperString,str_trim(DF_QPnMemos$Exam_Year[row]))
  PaperString <- paste(PaperString,str_trim(DF_QPnMemos$Exam_Sitting[row]))
  PaperString <- paste(PaperString,str_trim(DF_QPnMemos$Subject[row]))
  PaperString <- paste(PaperString,str_trim(DF_QPnMemos$Paper_No[row]))
  PaperString <- paste(PaperString,str_trim(DF_QPnMemos$Paper_Language[row]))
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
