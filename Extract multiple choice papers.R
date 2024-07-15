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
dir_path  <- "C:/DEV/R/data/pastpapers"
files_in_directory           <- as.data.frame(str_squish(list.files(dir_path)))
colnames(files_in_directory) <- c('filename')
files_in_directory$filename  <- tolower(files_in_directory$filename)
pdf_files_in_directory       <- filter(files_in_directory, str_detect(files_in_directory$filename, regex("pdf", ignore_case= TRUE)))
memo_files_in_directory      <- filter(pdf_files_in_directory, str_detect(pdf_files_in_directory$filename, regex("memo", ignore_case= TRUE)))
qp_files_with_memo           <- memo_files_in_directory
qp_files_with_memo$mc        <- NA

mc_qp_list                       <- as.data.frame(read.table("list of paper with multichoice.txt", sep = '/', header = TRUE ))
mc_qp_list$subject               <- tolower(mc_qp_list$subject)
mc_qp_list$paper_no              <- tolower(mc_qp_list$paper_no)
if (Prod_Run) 
{
  Rows2Process <-   nrow(memo_files_in_directory)
} else {
  Rows2Process <-   Rows2Test
} 
for (row in 1:Rows2Process) {
  qp_files_with_memo$mc[row] <- FALSE
  for (x in 1:nrow(mc_qp_list)) {
    if (str_detect(qp_files_with_memo$filename[row], mc_qp_list$subject[x])){
      if (str_detect(qp_files_with_memo$filename[row], mc_qp_list$paper_no[x])){
        qp_files_with_memo$mc[row] <- TRUE
        x <- nrow(mc_qp_list) + 1
      }
    }
  }
}
mc_qp_df <- filter(qp_files_with_memo, qp_files_with_memo$mc)
write.csv(mc_qp_df,"QPs_multiplechoice.csv", row.names = FALSE)  

rm(mc_qp_df)
rm(mc_qp_list)
rm(mc_qps)
rm(files_in_directory)
