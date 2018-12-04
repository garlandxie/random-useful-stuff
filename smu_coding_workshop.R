# SMU R Coding workshop for the Biology Department 

# Data types in R --------------------------------------------------------------

# integers (think of ratings ==> how cute is your cat (from 1 to 5)?)

1 # really ugly 
2
3
4
5 # the cutest

# numeric (think of  height ==> how tall is your cat?)

14.5
15.5
10.1
11.0
12.4

# logical - TRUE or FALSE (think of: is your cat cute or not?)
TRUE
FALSE
TRUE
TRUE
FALSE

# strings - text characters (think of cat names!)
"Simba"
"Tigger"
"Max"
"Felix"
"Garfield"

# Data types - missing values (think: did the person bail out on the survey?)
NA
NA
NA
NA
NA

# Operators --------------------------------------------------------------------

# Arithmetic operators
1 + 1
2 - 2
3 * 3
4 / 4

# Logical operators - think of TRUE and FALSE again!
"Simba" == "Simba" # equals
"Simba" != "Tigger" # does not equals
3 > 4 # greater than
4 < 3 # less than
1 >= 2 # greater or equals to
2 <= 2 # less or equals to

# Vectors - aka a group of values -----------------------------------------------

# we can group values together using "vectors" 
# in some cases, you can think of vectors as group of recorded observations
# we have to give R a specific instruction to do so using the "c()" function

# vectors with the same data types 
c(1, 2, 3, 4, 5) 
c(TRUE, FALSE, TRUE)
c("Simba", "Tigger", "Max")

# remember: R can force vectors into a single data type!!
c(1, 2, "Max")     # turns it into a vector of string characters
c(1, TRUE, "Max")  # again, another vector of string characters

# exception: R usually won't change missing data types
c(NA, 1, "Max")

# Objects - aka trying to remember our data.. ----------------------------------

# so we can tell R to intepret some data for us
# but how do we remember our data if we need to recall it later on?
# well, we can assign any type of information to a given name (called objects)
# we have to explicitly assign names by using "-->"
# shortcut for this is "ALt + -" in R studio

# (Not so great) examples of naming objects
foo <- c("Simba", "Tigger", "Max", "Felix", "Garfield") # generic name
c <-  c("Simba", "Tigger", "Max") # conflicting function name
TRUE <-  c("Simba", "Tigger", "Max") # conflicting with logical operators
cat.names <-  c("Simba", "Tigger", "Max") # conflicting with S3 objects

# (Better) examples of naming objects
cat_id         <- c(1, 2, 3, 4, 5, 6)
cat_rating     <- c(1, 2, 3, 4, 5, NA)
cat_names      <- c("Simba", "Tigger", "Max", "Felix", "Garfield", NA)
cat_height_mm  <- c(14.5, 15.5, 10.1, 11.0, 12.4, NA)
is_cute_or_not <- c(TRUE, FALSE, FALSE, TRUE, TRUE, NA)

# Note: naming objects is harder than it looks - has to be memorable and short!

# You'll also notice the objects names pop up in the right-hand window 
# Here, you're storing the objects in a "local workspace" - global environment
# But it's only temporary - once you close R, (ideally) they should be gone

# if you want to remove an object
rm(foo)

# if you want to remove all objects - clear entire workspace 
# rm(list=ls())

# Vector indexing aka finding specific values in a vector ----------------------
# Sometimes we just want to access certain parts of our vector
# We can do this through indexing
# Think of place-holder numbers (like waiting line numbers) for each item 

# Recall: 
cat_names <- c("Simba", "Tigger", "Max", "Felix", "Garfield", NA)

# Getting specific cat names by their position in the vector
# Remember: first position is 1
cat_names[1]
cat_names[2]
cat_names[5]

# Getting creative: does the vector of cat names contain "Simba"?
cat_names %in% "Simba" # result: first place is Simba which is correct!

# Data frames - aka creating rows and columns ---------------------------------

# What if you want to recreate an table with rows and columns by hand?
# We can use another tool (data.frame) using two good methods: 

# 1) using assigned vectors with object names as columns
cat_df1 <- data.frame(cat_id,
                      cat_names,
                      cat_rating,
                      cat_height_mm,
                      is_cute_or_not)

# 2) manually typing in the vectors 
# notice how I've formatted my column names (col_name)
cat_df2 <- data.frame(
  id       = c(1, 2, 3, 4, 5, 6),
  names    = c("Simba", "Tigger", "Max", "Felix", "Garfield", NA),
  ratings  = c(1, 2, 3, 4, 5, NA),
  height   = c(14.5, 15.5, 10.1, 11.0, 12.4, NA),
  cute_ugly = c(TRUE, FALSE, FALSE, TRUE, TRUE, NA)
)

# Data frame indexing ----------------------------------------------------------

# just like vectors, we can also recall certain rows or columns
# [ , ] : left side is rows, right side is columns

# let's try just grabbing some columns first
cat_df1[, "cat_height_mm"] # calls out a vector 
cat_df1[, c("cat_height_mm", "cat_id")] # calls out a data-frame

# now, let's just grab some rows
cat_df1[1, ]     # first row
cat_df1[1:2, ]   # first two rows 
cat_df1[c(1,5), ] # first row and fifth row 
cat_df1[-1, ]    # grab everything but the first row 

# now let's combine both rows and columns
cat_df1[1:2, "cat_height_mm"]

# why do we have to know this?
# sometimes, we just want to quickly find certain parts of data 
# indexing helps us achieve this goal
# e.g., finding extreme values that don't make sense (subsetting rows)
# e.g., only analyzing only two factors in an experiment (subsetting columns)











