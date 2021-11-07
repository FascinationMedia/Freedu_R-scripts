library(tidyverse)
library(rvest)
library(stringr)
library(dplyr)
library(pdftools)

QP_PDF_url <- ("https://www.education.gov.za/LinkClick.aspx?fileticket=Lo_oEjh0HMs%3d&tabid=2837&portalid=0&mid=9895")
my_pdf_text <- pdf_text(QP_PDF_url)
my_pdf_text
my_pdf_txt_list <- str_split(my_pdf_text, "\n")
my_pdf_txt_list
typeof(my_pdf_txt_list)
str_detect(my_pdf_txt_list, regex(" 1.1", ignore_case = TRUE))







Maths_Single_url <- ("https://www.education.gov.za/LinkClick.aspx?link=https%3a%2f%2fwww.education.gov.za%5cPortals%5c0%5cCD%5c2019+November+past+papers%5cNon-Languages+Nov+2019+Memos+PDF%5cMathematics%5cMathematics+P1+Nov+2019+Memo+Afr+%26+Eng.pdf&tabid=2556&portalid=0&mid=9143")
my_pdf_text <- pdf_text(Maths_Single_url)
my_pdf_text
my_pdf_txt_list <- str_split(my_pdf_text, "\n")
my_pdf_txt_list

