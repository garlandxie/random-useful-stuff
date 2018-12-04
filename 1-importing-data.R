# SMU R Coding workshop - importing data 
# Purpose: teach students and professors how to import data
# as well as creating R scripts (so it's 2-in-1 thing here)
# Code developed by Garland Xie (garlandxie@smu.ca)

# Install libraries  ------------------------------------------------------
install.packages(c("readr", "readxl"))

# Import libraries --------------------------------------------------------
library(readr)   # for reading in csv files 
library(readxl)  # for reading in excel files 

# Import data -------------------------------------------------------------

# Importing data comes in many shapes and sizes in R
# Relatively easy to import data if they have a defined structure: 
  # text files, excel files, or google spreadsheets (so think table formats)
# A little bit harder when it's more messy and unstructured: 
  # web pages (think scraping data from lots of lines of code)

# In my experience, a lot of people import their data either through:
  # Excel 
  # Google spreadsheets
  # Comma-seprated delimited files(CSV)
# So we'll just use these two examples for today
  # Each of these two examples have their own advantages and disadvantages

# Importing csv files ------------------------------------------------------

  # The set-up fora comma-separated delimited files is simple:
  # 1,2,3 ==> each row has its values separated by just commas
  # the beauty of this file format is that it'll never degrade
  # excel file formats might upgraded and any changes might affect import
  # plus, the simplicity of this format means a smaller file size 

# automatically: strings are NOT factors (which is good)
df_csv <- read_csv(file_name)

# a little fancier: telling R what data types for each column
# why: by default, read_csv tries to guess what the data types are
# in some cases, you might need to tell R to change into a factor 
# e.g., experimental treatments are masked as integers rather than factors
df_csv <- read_csv(file_name, col_types = list(col_double(),
                                               col_character()))

# Importing excel files ----------------------------------------------------

# Unlike csv files, excel files are a bit more complex
  # Different work-sheets piled into one file 
df_xlsx <- read_excel(file_name, sheet = "hi") # avoiding using numbers

# similarly, we can use explicit column specifications
df_xlsx <- read_excel(file_name,
                      sheet = "hi",
                      cols_types = list(col_double(),
                                        col_character()))

# Importing Google spreadsheets ---------------------------------------------

# if you'd like, you can import google spreadsheet files
# sometimes, entering data through the cloud is more beneficial than excel files
# such as ease of portability (can access files whenever you have internet)




