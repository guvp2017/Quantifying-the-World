
### -----------------------------------------------
## Section 0: Load the packages for this cse study
### -----------------------------------------------

Packages <- c("xml2", "rvest", "stringr", "purrr", 'RColorBrewer', 'ggplot2', 'tidyr', 'broom', 'dplyr', 'ggthemes')
lapply(Packages, library, character.only = TRUE)

## --------------------------------------------------------------------------
## Section 1: Scraping the race results from the web (www.cherryblossom.org)
## --------------------------------------------------------------------------

## Use the 'read_html() in the 'rvest' package to scrape
## The online table was previously inserted into a '<pre>' node from the HTML source

ubase <- 'http://www.cherryblossom.org/'
url <- paste0(ubase, 'results/1999/award.html')
doc <- read_html(url)
pre_node <- html_nodes(doc, 'pre') # access all '<pre>' nodes in the document with the CSS selector 'pre'
txt <- html_text(pre_node) # extract the text content from this node 

nchar(txt) # examine the contents of of 'txt'

str_sub(txt, 1, 50)
str_sub(txt, nchar(txt) - 50, nchar(txt))

els <- str_split(txt, '\\r\\n')[[1]] # use '\r\n' to split up the 690904 characters into separate strings
length(els)
head(els, 3)
tail(els, 3)

## Formalize these codes into a function to take as input the URL for the Web page 
## and return a character vector with one element per line, 
## including the header lines and the rows in the table of results
extract_res_table <- function(url) {
    read_html(url) %>% 
    html_nodes('pre') %>% 
    html_text() %>% 
    str_split('\\r\\n') %>% 
    .[[1]]
}

## Try this function with the 1999 men's results
m1999 <- extract_res_table(url)
identical(m1999, els)

## Construct the 'urls' vector from proper web addresses for all the years
men_urls <- c(
    'results/1999/cb99m.html',
    'results/2000/Cb003m.htm',
    'results/2001/oof_m.html',
    'results/2002/oofm.htm',
    'results/2003/CB03-M.HTM',
    'results/2004/men.htm',
    'results/2005/CB05-M.htm',
    'results/2006/men.htm',
    'results/2007/men.htm',
    'results/2008/men.htm',
    'results/2009/09cucb-M.htm',
    'results/2010/2010cucb10m-m.htm',
    'results/2011/2011cucb10m-m.htm',
    'results/2012/2012cucb10m-m.htm'
)

men_urls <- paste0(ubase, men_urls)
head(men_urls, 3)

# Fix some issues ('\n' for year 1999, 'html_nodes(font)')
extract_res_table <- function(url, year = 2001) {
    # handle weird cases
    if (year == 1999) {
        read_html(url) %>% 
        html_nodes('pre') %>%
        html_text() %>% 
        str_split('\\n') %>% 
        .[[1]]
    } else if (year == 2000) {
        read_html(url) %>% 
        html_nodes('font') %>%
        .[[4]] %>% 
        html_text() %>% 
        str_split('\\r\\n') %>% 
        .[[1]]
    } else {
        read_html(url) %>% 
        html_nodes('pre') %>% 
        html_text() %>% 
        str_split('\\r\\n') %>% 
        .[[1]]
    }
}

## Use map2() to call 'extract_res_table()' to check and find year 2009 still has problem
years <- 1999:2012
men_tables <- map2(men_urls, years, extract_res_table)
names(men_tables) <- years
map_int(men_tables, length)

## Final modified extract_res_table()
extract_res_table <- function(url, year = 2001) {
    selector <- if (year == 2000) 'font' else 'pre'
        regexp <- if (year == 1999) '\\n' else '\\r\\n'
            result <- read_html(url) %>% 
        html_nodes(selector)
    if (year == 2000) result <- result[[4]]
    result <- result %>% 
    html_text()
    
    if (year == 2009) return(result)
    result %>% 
    str_split(regexp) %>% 
    .[[1]]
}

## Read results into R and check the results
## Now, we have the function working for the web pages of men's results
men_tables <- map2(men_urls, years, extract_res_table)
names(men_tables) <- years
map_int(men_tables, length)

## Try this on the women's pages 
women_urls <- c(
    'results/1999/cb99f.html',
    'results/2000/Cb003f.htm',
    'results/2001/oof_f.html',
    'results/2002/ooff.htm',
    'results/2003/CB03-F.HTM',
    'results/2004/women.htm',
    'results/2005/CB05-F.htm',
    'results/2006/women.htm',
    'results/2007/women.htm',
    'results/2008/women.htm',
    'results/2009/09cucb-F.htm',
    'results/2010/2010cucb10m-f.htm',
    'results/2011/2011cucb10m-f.htm',
    'results/2012/2012cucb10m-f.htm'
)

women_urls <- paste0(ubase, women_urls)
women_tables <- map2(women_urls, years, extract_res_table)
names(women_tables) <- years

## Modify extract_res_table for 2009 (no need to change)

extract_res_table <- function(url, year = 2001, female = TRUE) {
    selector <- if (year == 2000) 'font' else 'pre'
        regexp <- if (year == 1999) '\\n' else '\\r\\n'
            result <- read_html(url) %>% 
        html_nodes(selector)
    
    if (year == 2000) result <- result[[4]]
    result <- result %>% 
    html_text()
    
    if (year == 2009 && female == FALSE) return(result)
    result %>% 
    str_split(regexp) %>% 
    .[[1]]
}

## Check the finally extracted tables for men and women and for all the years form 1999 to 2012
men_tables <- map2(men_urls, years, extract_res_table, female = FALSE)
women_tables <- map2(women_urls, years, extract_res_table, female = TRUE)

names(men_tables) <- years
names(women_tables) <- years

map_int(men_tables, length)
map_int(women_tables, length)

## Save the data in the text files for further processing
dir.create('men')
dir.create('women')

walk2(men_tables, 
      paste('men', paste(years, 'txt', sep = '.'), sep = '/'),
      writeLines)

walk2(women_tables,
       paste('women', paste(years, 'txt', sep = '.'), sep = '/'),
       writeLines)

## ---------------------------------------------
## Section 2: Reading race result tables into R 
## ---------------------------------------------

## Try read.table() and find 'skip=8'is not working
## m2012 <- read.table('men/2012.txt', skip = 8)

## Use readLines() and check the first 10 rows of the 2012 men's table
els2012 <- readLines('men/2012.txt')
head(els2012, 10)

## Use readLines() and check the first 10 rows of the 1999 men's table
els1999 <- readLines('men/1999.txt')
head(els1999, 10)

## Use the 2012 men's results as test case for developing the code to read in all files
eq_idx <- str_which(els2012, '^===') # search through the character strings in els2012

# Extract the key row and the row above it in the table, discard earlier rows
spacer_row <- els2012[eq_idx] 
header_row <- els2012[eq_idx - 1]
body <- tail(els2012, -eq_idx)

# Extract runner's age by converting the column names to lower case
header_row <- str_to_lower(header_row)

# Search through 'header_row' for 'ag'
age_location <- str_locate(header_row, 'ag')
age_location # the location of runner's age (starts at 49 and ends at 50 in each row of the table)

# Use above information to extract each runner's age using 'str_sub()'
age <- str_sub(body, start = age_location[1,1], end = age_location[1,2])
head(age)
summary(as.numeric(age)) # age range from 9 to 89, and 2 did not report ages

# Find the locations of all of the blanks in the line of ‘=’ characters
blank_locs <- str_locate_all(spacer_row, ' ')[[1]][ ,1]
blank_locs # Blank spaces are found at the 6th, 18th, 25th, 48th, ...

# Augment `blank_locs` with 0 so the first column starts one character after 0
search_locs <- c(0, blank_locs)

## Extract all the columns with 'str_sub()'
values <- map(body, str_sub,
              start = head(search_locs, -1) + 1,
              end = tail(search_locs, -1) - 1)

## Encapsulate the task of finding the starting and ending positions of the columns into a function
find_col_locs <- function(spacer_row) {
  space_locs <- str_locate_all(spacer_row, ' ')[[1]][ ,1]
  row_length <- nchar(spacer_row)
  
  if (!str_detect(spacer_row, ' $')) {
    return(c(0, space_locs, row_length + 1))
  } else return(c(0, space_locs))
}

## Encapsulate into a function the code to extract the locations of the desired columns. 
## This function need the desired columns names, the header row (contains the column names), 
## and the locations of the blanks in the separator row
select_cols <- function(col_names, header_row, search_locs) {
  select_col <- function(name, header_row, search_locs) {
    start_pos <- str_locate(header_row, name)[1,1]
    if (is.na(start_pos)) return(c(NA, NA))
    index <- sum(start_pos >= search_locs)
    c(search_locs[index] + 1, search_locs[index + 1] - 1)
  }
  
  map(col_names, select_col, 
      header_row = header_row, 
      search_locs = search_locs) %>% 
    do.call('rbind', .)
}

## Example of using this function to find age variable columns
search_locs <- find_col_locs(spacer_row)
age_loc <- select_cols('ag', header_row, search_locs)
ages <- map_chr(body, str_sub,
                start = age_loc[ ,1],
                end = age_loc[ ,2])
summary(as.numeric(ages))

## Use only the first few characters to uniquely identify the desired columns
short_col_names = c("name", "home", "ag", "gun", "net", "time")

## Want the values for that variable to be 'NA' if a file does not have one of the desired variables
loc_cols <- select_cols(short_col_names, header_row, search_locs)
values <- map(body, str_sub,
              start = loc_cols[ ,1],
              end = loc_cols[ ,2]) %>% 
  do.call('rbind', .)

## Check the type of the return value
class(values)

## The results form a matrix of character strings
## Here to show the first few rows of the matrix 
colnames(values) <- short_col_names
head(values)

## The 2012 table has a column for time and not gun and net times so the gun and net values are NA. 
## Here to check the last few lines
tail(values)

## Wrap up the process of extracting the columns into a function so we can apply it to each year’s data
extract_variables <- function(file, var_names = c("name", "home", "ag", "gun", "net", "time")) {
    # find the index of the row with =s
    eq_idx <- str_which(file, '^===')
    # extract the two key rows and the data
    spacer_row <- file[eq_idx]
    header_row <- file[eq_idx - 1] %>% str_to_lower()
    body <- tail(file, - eq_idx)
    
    # get starting and ending positions of variables
    search_locs <- find_col_locs(spacer_row)
    loc_cols <- select_cols(var_names, header_row, search_locs)
    values <- map(body, str_sub, loc_cols) %>% do.call('rbind', .)
    colnames(values) <- var_names
    invisible(values)
}

## Read the lines of the tables for men into R
mfilenames <- list.files('men', pattern = '.txt$', full.names = TRUE)
men_files <- map(mfilenames, readLines)
names(men_files) <- str_match(mfilenames, 'men/(.*).txt')[ ,2]

## Read the lines of the tables for women into R
wfilenames <- list.files('women', pattern = '.txt$', full.names = TRUE)
women_files <- map(wfilenames, readLines)
names(women_files) <- str_match(wfilenames, 'women/(.*).txt')[ ,2]

## Apply the 'extract_variables()' function to 'men_files 'to obtain a list of character matrices 
men_res_mat <- map(men_files, extract_variables)
length(men_res_mat)
map_int(men_res_mat, nrow)

## There is a problem with one of the women's files
## In 2001, the separator row of '=' characters does not exist
men_file_2001 <- men_files$'2001'
women_file_2001 <- women_files$'2001'

eq_idx_2001 <- str_which(men_file_2001, '^===')
spacer_row_2001 <- men_file_2001[eq_idx_2001]
header_row_2001 <- men_file_2001[eq_idx_2001 - 1] %>% str_to_lower()

women_files$'2001'[2] <- header_row_2001
women_files$'2001'[3] <- spacer_row_2001

## Get reasonable values for the number of rows in our matrices
women_res_mat <- map(women_files, extract_variables)
length(women_res_mat)
map_int(women_res_mat, nrow)

## ------------------------------------------------------------------
## Section 4: Clean data and reformat variables to be different types
## ------------------------------------------------------------------

mfilenames = paste("men/", 1999:2012, ".txt", sep = "")
menFiles = lapply(mfilenames, readLines)
names(menFiles) = 1999:2012

wfilenames = paste("women/", 1999:2012, ".txt", sep = "")
womenFiles = lapply(wfilenames, readLines)
names(womenFiles) = 1999:2012

## Convert the list of character matrices into an appropriate format for analysis
men_res_mat <- map(men_files, extract_variables)
length(men_res_mat)
map_int(men_res_mat, nrow)

## Create the numeric variable 'age' with 'as.numeric()' ( example for the 2012 males)
age <- as.numeric(men_res_mat[['2012']][ ,'ag'])

## Check a few `age` values with tail()
tail(age)

## Check more thoroughly to confirm data extraction works as expected by summarizing each year’s ages 
## Receive warning messages that our conversion of the character values for age into numeric resulted in `NA` values,
## meaning that some of the values do not correspond to numbers
age <- map(men_res_mat, ~ as.numeric(.x[ ,'ag']))

## Create side-by-side boxplots of the yearly distribution of the age of the runners 
## Quick check on the reasonableness of the values
boxplot(age, ylab = "Age", xlab = "Year")

dev.copy(png,filename="./cs2Figures/age_runners_01.png", width=400, height=400);
dev.off ();
## There are problems for 2 years. 
## All of the runners in 2003 were under 10 
## More than 1 in 4 runners in 2006 were under 10

## In 2003, the age values are shifted to the right one space in comparison to the location of the ‘=’ characters
## This means that we are picking up only the digit in the tens place
head(men_files$'2003')

## In 2006, some but not all of the rows have values that are off by one character
men_files$'2006'[2200:2205]

## Modify the line in `select_cols()` that locates the end of a column to include the blank position
select_cols <- function(col_names, header_row, search_locs) {
  select_col <- function(name, header_row, search_locs) {
    start_pos <- str_locate(header_row, name)[1,1]
    if (is.na(start_pos)) return(c(NA, NA))
    index <- sum(start_pos >= search_locs)
    c(search_locs[index] + 1, search_locs[index + 1])
  }
  
  map(col_names, select_col, 
      header_row = header_row, 
      search_locs = search_locs) %>% 
    do.call('rbind', .)
}

## Remove blanks with rgular expressions
men_res_mat <- map(men_files, extract_variables)
women_res_mat <- map(women_files, extract_variables)
age <- map(men_res_mat, ~ as.numeric(.x[ ,'ag']))

## Boxplots show reasonable age distributions and the problem above has cleaned up
boxplot(age, ylab = "Age", xlab = "Year")

dev.copy(png,filename="./cs2Figures/age_runners_02.png", width=400, height=400);
dev.off ();

## For the warning messages "NAs introduced by coercion", count the number of 'NA' values in each year
map_int(age, ~ sum(is.na(.)))

## Check why year 2001 has 62 'NA's for age
age2001 <- age$'2001'

## Add an offset to the lication of the 'NA's in'age2001'
offset <- str_which(men_files$'2001', '^===')
offset

## Find the lines in the original file that have the bad age values
## All rows are empty except the footnote row
bad_age_idx <- which(is.na(age2001)) + offset
tail(men_files$'2001'[bad_age_idx])

## Find where in the table are these rows located
bad_age_idx

## Modify the extraction by checking for blank rows and removing them
blanks <- str_which(men_files[['2001']], '^[[:blank:]]*$')

## modified 'extract_variables()'function
extract_variables <- function(file, var_names = c("name", "home", "ag", "gun", "net", "time")) {
    # find the index of the row with =s
    eq_idx <- str_which(file, '^===')
    
    # extract the two key rows and the data
    spacer_row <- file[eq_idx]
    header_row <- file[eq_idx - 1] %>% str_to_lower()
    
    # find blank lines
    blanks <- str_which(file, '^[[:blank:]]*$')
    # find comments
    comments <- str_which(file, '^[[:blank:]]*[#\\*]')
    # remove header, blank lines, and comments
    body <- file[-c(1:eq_idx, blanks, comments)]
    
    # get starting and ending positions of variables
    search_locs <- find_col_locs(spacer_row)
    loc_cols <- select_cols(var_names, header_row, search_locs)
    values <- map(body, str_sub, loc_cols) %>% do.call('rbind', .)
    colnames(values) <- var_names
    invisible(values)
    }

## After additional cleaning, the 61 'NA' are gone, but not others
men_res_mat <- map(men_files, extract_variables)
women_res_mat <- map(women_files, extract_variables)

map_int(age, ~ sum(is.na(.)))

## Find which runners have an age under 5 and look at their records in the original table
which(age2001 < 5)
men_files[['2001']][which(age2001 < 5) + offset]

## Create time variable()
char_time <- men_res_mat[['2012']][ ,'time']
head(char_time)
tail(char_time)

## Use str_split() to split each character string up into its parts
time_pieces <- str_split(char_time, ':')

## Check heand() and ()
head(time_pieces, 1)
tail(time_pieces, 1)

## Convert these elements to numeric values 
## and combine them into one value that reports time in minutes
run_time <- map_dbl(time_pieces, function(x) {
  x <- as.numeric(x)
  if (length(x) == 2) x[1] + x[2] / 60
  else 60 * x[1] + x[2] + x[3] / 60
})

## Check with summary()
summary(run_time)

## Encapsulate this conversion into a function called 'convert_time()'
convert_time <- function(t) {
  time_pieces <- str_split(t, ':')
  map_dbl(time_pieces, function(x) {
    x <- as.numeric(x)
    if (length(x) == 2) x[1] + x[2] / 60
    else 60 * x[1] + x[2] + x[3] / 60
  })
}

## Wrap these conversions into a function creat_df()
create_df <- function(res, year, sex) {
  # determine which time to use
  use_time <- if (!is.na(res[1,'net'])) {
                res[ ,'net']
              } else if (!is.na(res[1,'gun'])) {
                res[ ,'gun']
              } else {
                res[ ,'time']
              }
  run_time <- convert_time(use_time)
  
  results <- data.frame(year = year,
                        sex = sex,
                        name = res[ ,'name'],
                        home = res[ ,'home'],
                        age = as.numeric(res[ ,'ag']),
                        time = run_time)
  
  invisible(results)
}

## Apply to the character matrices in menResMat and return a data frame with variables for analysis
men_df <- map2(men_res_mat, 1999:2012, create_df, sex = 'm')

## There are a large number of 'NA' in 2007, 2009, 2010 and all of the tume values for 2006
map_dbl(men_df, ~ sum(is.na(.x$time)))

## These are caused by runners who completed half the race but have no final times 
## and by runners who have a footnote after their time
men_files[['2007']][c(8, 5280)]

## Modify 'create_df()'' to eliminate the footnote symbols 
create_df <- function(res, year, sex) {
    use_time <- if (!is.na(res[1,'net'])) {
                res[ ,'net']
              } else if (!is.na(res[1,'gun'])) {
                res[ ,'gun']
              } else {
                res[ ,'time']
              }
  # remove #, *, and blanks from time
  use_time <- str_replace_all(use_time, '[#\\*[:blank:]]', '')
  # drop rows with no time
  res <- res[use_time != '', ]
  run_time <- convert_time(use_time[use_time != ''])
  
  results <- data.frame(year = year,
                        sex = sex,
                        name = res[ ,'name'],
                        home = res[ ,'home'],
                        age = as.numeric(res[ ,'ag']),
                        time = run_time)
  
  invisible(results)
}

## Apply this revised function to 'men_res_mat' to create data frame
men_df <- map2(men_res_mat, 1999:2012, create_df, sex = 'm')

## Most missing values are gone except for 2006
map_dbl(men_df, ~ sum(is.na(.x$time)))

## Look at the missing value for 2001
men_df[['2001']] %>% filter(is.na(time))
i <- grep('Peter HUI', men_files[['2001']])
men_files[['2001']][(i-1):(i+1)]

## This runner's time got entered incorrectly
## Fill in that value manually
men_df[['2001']]$time[2250] <- 91 + 47/60

## As for 2006, the problem can be seen when we look at the header lines of that file
men_files[['2006']][7:11]

## Alter the separator line directly and rerun 'extract_variables()'
spacer_row_2006 <- men_files[['2006']][8]
str_sub(spacer_row_2006, 64, 64) <- ' '
men_files[['2006']][8] <- spacer_row_2006
men_files[['2006']][7:11]

# library(dplyr)

## combine the race results for all years and men into one data frame
men_res_mat <- map(men_files, extract_variables)
men_df <- map2_dfr(men_res_mat, 1999:2012, create_df, sex = 'm') %>% 
  mutate_if(is_character, funs(str_trim(., side = 'both'))) %>% 
  mutate(name = str_to_title(name))
men_df[which(men_df$year == 2001 & men_df$name == 'Peter Hui'),'time'] <- 91 + 47/60

## Write men_df.scv file for future processing
write.csv(men_df, 'men/men_df.csv')

## Check the dimension of our amalgamated data frame
dim(men_df)

## Apply this revised function to 'women_res_mat' to create data frame
women_df <- map2(women_res_mat, 1999:2012, create_df, sex = 'f')

## There are a large number of 'NA' in 2007, 2009, 2010 and all of the tume values for 2006
map_dbl(women_df, ~ sum(is.na(.x$time)))

## As for 2006, the problem can be seen when we look at the header lines of that file
women_files[['2006']][7:11]

## Alter the separator line directly and rerun 'extract_variables()'
spacer_row_2006 <- women_files[['2006']][8]
str_sub(spacer_row_2006, 64, 64) <- ' '
women_files[['2006']][8] <- spacer_row_2006
women_files[['2006']][7:11]

## Clean and reformat women's race results and save to women_df.csv
women_res_mat <- map(women_files, extract_variables)
women_files[['2006']][8] <- spacer_row_2006
women_df <- map2_dfr(women_res_mat, 1999:2012, create_df, sex = 'f')
women_df[grep('^Marie-Laure Poir', women_df$name),'name'] <- 'Marie-Laure Poire'
women_df <- women_df %>%
mutate_if(is_character, funs(str_trim(., side = 'both'))) %>%
mutate(name = str_to_title(name))

## Write women_df.scv file for future processing
write.csv(women_df, 'women/women_df.csv')

## Check the dimension of our amalgamated data frame
dim(women_df)

plot(time ~ age, data = men_df, 
     ylim = c(40, 180), xlab = "Men ages (years)", ylab = "Run Time (minutes)", 
     col =  "#3366FF")

dev.copy(png,filename="./cs2Figures/scatter_men.png", width=400, height=400);
dev.off ();

## 
plot(time ~ age, data = women_df, 
     ylim = c(40, 180), xlab = "Women ages (years)", ylab = "Run Time (minutes)",
     col = "#CC6666")

dev.copy(png,filename="./cs2Figures/scatter_women.png", width=400, height=400);
dev.off ();

# display.brewer.all()

# Purples8 = brewer.pal(9, "Purples")[8]
# Purples8

# Purples8A = paste(Purples8, "14", sep = "")

men_df %>% 
filter(age > 5) %>% 
ggplot(aes(age, time)) +
geom_jitter(shape = '.', size = 2, alpha = 0.2, height = 0, width = 0.5, color = '#54278f')

dev.copy(png,filename="./cs2Figures/scatter5_men.png", width=400, height=400);
dev.off ();

women_df %>% 
filter(age > 5) %>% 
ggplot(aes(age, time)) +
geom_jitter(shape = '.', size = 2, alpha = 0.2, height = 0, width = 0.5, color = '#54278f')

dev.copy(png,filename="./cs2Figures/scatter5_women.png", width=400, height=400);
dev.off ();

men_df %>% 
  filter(age > 5) %>% 
  ggplot(aes(age, time)) +
  stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE) +
  scale_fill_gradientn(colors = c('white', 'dodgerblue3', 'dodgerblue4'), values = c(0, 0.5, 1)) +
  geom_hline(yintercept = 0, color = 'navy')

dev.copy(png,filename="./cs2Figures/scatter5_density_men.png", width=400, height=400);
dev.off ();

women_df %>% 
  filter(age > 5) %>% 
  ggplot(aes(age, time)) +
  stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE) +
  scale_fill_gradientn(colors = c('white', 'dodgerblue3', 'dodgerblue4'), values = c(0, 0.5, 1)) +
  geom_hline(yintercept = 0, color = 'navy')

dev.copy(png,filename="./cs2Figures/scatter5_density_women.png", width=400, height=400);
dev.off ();

## Section 5: Exploring the run time for all runners (men and women)

# get normalized value
men_df_99 <- men_df %>% 
  filter(!is.na(age), age > 19, age < 81, year=='1999')
men_df_12 <- men_df %>% 
  filter(!is.na(age), age > 19, age < 81, year=='2012')
women_df_99 <- women_df %>% 
  filter(!is.na(age), age > 19, age < 81, year=='1999')
women_df_12 <- women_df %>% 
  filter(!is.na(age), age > 19, age < 81, year=='2012')
# find fastest runner each year
getmin<-function(time){
  time/min(time)
}
m_fastest_99 <- tapply(men_df_99$time, men_df_99$age, getmin)
m_fastest_12 <- tapply(men_df_12$time, men_df_12$age, getmin)
w_fastest_99 <- tapply(women_df_99$time, women_df_99$age, getmin)
w_fastest_12 <- tapply(women_df_12$time, women_df_12$age, getmin)

# men normalized values
toappend <- list()
i <- 1
for (j in m_fastest_99) {
  age <- names(m_fastest_99[i])
  val <- replicate(length(j), age)
  toappend <- append(toappend, val)
  i = i+1
}
agedf <- as.data.frame(matrix(unlist(toappend)))
timedf <- as.data.frame(matrix(unlist(m_fastest_99)))
men_99_norm <- cbind(agedf, timedf)
names(men_99_norm) <- c("Age", "Time")

toappend <- list()
i <- 1
for (j in m_fastest_12) {
  age <- names(m_fastest_12[i])
  val <- replicate(length(j), age)
  toappend <- append(toappend, val)
  i = i+1
}
agedf <- as.data.frame(matrix(unlist(toappend)))
timedf <- as.data.frame(matrix(unlist(m_fastest_12)))
men_12_norm <- cbind(agedf, timedf)
names(men_12_norm) <- c("Age", "Time")

# women normalized values
toappend <- list()
i <- 1
for (j in w_fastest_99) {
  age <- names(w_fastest_99[i])
  val <- replicate(length(j), age)
  toappend <- append(toappend, val)
  i = i+1
}
agedf <- as.data.frame(matrix(unlist(toappend)))
timedf <- as.data.frame(matrix(unlist(w_fastest_99)))
women_99_norm <- cbind(agedf, timedf)
names(women_99_norm) <- c("Age", "Time")

toappend <- list()
i <- 1
for (j in w_fastest_12) {
  age <- names(w_fastest_12[i])
  val <- replicate(length(j), age)
  toappend <- append(toappend, val)
  i = i+1
}
agedf <- as.data.frame(matrix(unlist(toappend)))
timedf <- as.data.frame(matrix(unlist(w_fastest_12)))
women_12_norm <- cbind(agedf, timedf)
names(women_12_norm) <- c("Age", "Time")


# Get age categories
men_df_agecat <- men_df %>% 
  filter(time > 30, !is.na(age), age > 15) %>% 
  mutate(age_cat = cut(age, breaks = c(seq(15, 75, 10), 90)))
table(men_df_agecat$age_cat)

## Plot of the Number of Male Runners by Year. 
## This plot shows that the number of male runners in the Cherry Blossom 10-mile race has more than doubled from 1999 to 2012
men_df_agecat %>%
  ggplot(aes(factor(year))) +
  geom_bar(width = 0.6, fill='steelblue', color='steelblue') +
  labs(x = 'Year', y = 'Number of Male Runners')

dev.copy(png,filename="./cs2Figures/barplot_men.png", width=400, height=400);
dev.off ();

## The distribution of performance for men 1999 races
men_df_agecat %>% 
  filter(year == 1999) %>% 
  select(time) %>% 
  summary()

## The distribution of performance for men 2012 races
men_df_agecat %>% 
  filter(year == 2012) %>% 
  select(time) %>% 
  summary()

## Density curves for the age of male runners in 1999 and 2012
men_df_agecat %>% 
  filter(year %in% c(1999, 2012)) %>% 
  ggplot(aes(age, color = factor(year))) +
  geom_line(stat = 'density') +
  scale_color_few(name = 'Year') +
  labs(x = 'Age', y = 'Density')

dev.copy(png,filename="./cs2Figures/density_men_1999_2012.png", width=400, height=400)
dev.off ();

# normalized

plot(density(men_99_norm$Time), col="purple", lwd = 3, main=" ")
lines(density(men_12_norm$Time), col="green", lwd = 3, lty = 3, )
legend("topright", col = c("green", "purple"), lty = 3:1, lwd = 3, legend = c("2012m", "1999m"))

dev.copy(png,filename="./cs2Figures/density_men_1999_2012_normalized.png", width=400, height=400)
dev.off ();

# normalized
plot(density(women_99_norm$Time), col="purple", lwd = 3, main=" ")
lines(density(women_12_norm$Time), col ="green", lwd = 3, lty = 3)
legend("topright", col = c("green", "purple"), lty = 3:1, lwd = 3, legend = c("2012w", "1999w"))

dev.copy(png,filename="./cs2Figures/density_women_1999_2012_normalized.png", width=400, height=400);
dev.off ();

## Compare these two distributions with a quantile-quantile plot
pts <- qqplot(men_df_agecat$age[men_df_agecat$year == 1999],
              men_df_agecat$age[men_df_agecat$year == 2012],
              plot.it = FALSE) %>% as_data_frame()
pts %>% 
  ggplot(aes(x, y)) +
  geom_point(fill='steelblue', color='steelblue') +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = 'Age in 1999', y = 'Age in 2012')

dev.copy(png,filename="./cs2Figures/qqplot_men_1999_2012.png",width=400, height=400); 
dev.off ();

men_df_agecat <- men_df %>% 
  filter(time > 30, !is.na(age), age > 15) %>% 
  mutate(age_cat = cut(age, breaks = c(seq(15, 75, 10), 90)))
table(men_df_agecat$age_cat)

men_df_agecat %>% 
  ggplot(aes(age_cat, time)) +
  geom_boxplot(fill='white', color='steelblue') +
  labs(x = 'Age (years)', y = 'Run time (minutes)')

dev.copy(png,filename="./cs2Figures/boxplot_men.png",width=400, height=400);
dev.off ();

# Normalized
pts <- qqplot(men_99_norm$Time,
              men_12_norm$Time,
              plot.it = FALSE) %>% as_data_frame()
pts %>% 
  ggplot(aes(x, y)) +
  geom_point(fill='steelblue', color='steelblue') +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = 'Time in 1999', y = 'Time in 2012')

dev.copy(png,filename="./cs2Figures/qqplot_men_1999_2012_normalized.png",width=400, height=400); 
dev.off ();

women_df_agecat <- women_df %>% 
  filter(time > 30, !is.na(age), age > 15) %>% 
  mutate(age_cat = cut(age, breaks = c(seq(15, 75, 10), 90)))
table(women_df_agecat$age_cat)

women_df_agecat %>% 
  ggplot(aes(age_cat, time)) +
  geom_boxplot(fill='white', color='#CC6666') +
  labs(x = 'Age (years)', y = 'Run time (minutes)')

dev.copy(png,filename="./cs2Figures/boxplot_women.png",width=400, height=400);
dev.off ();

# Normalized
pts <- qqplot(women_99_norm$Time,
              women_12_norm$Time,
              plot.it = FALSE) %>% as_data_frame()
pts %>% 
  ggplot(aes(x, y)) +
  geom_point(fill='steelblue', color='#CC6666') +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = 'Time in 1999', y = 'Time in 2012')

dev.copy(png,filename="./cs2Figures/qqplot_women_1999_2012_normalized.png",width=400, height=400); 
dev.off ();

women_df_agecat <- women_df %>% 
  filter(time > 30, !is.na(age), age > 15) %>% 
  mutate(age_cat = cut(age, breaks = c(seq(15, 75, 10), 90)))
table(women_df_agecat$age_cat)

women_df_agecat %>% 
  ggplot(aes(age_cat, time)) +
  geom_boxplot(fill='white', color='#CC6666') +
  labs(x = 'Age (years)', y = 'Run time (minutes)')

dev.copy(png,filename="./cs2Figures/boxplot_women_normalized.png",width=400, height=400);
dev.off ();


## Line plot of the number of female runners by year. 
## This plot shows that the number of female runners in the Cherry Blossom 10-mile race has more than 4-folds from 1999 to 2012
women_df_agecat %>%
  ggplot(aes(factor(year))) +
  geom_bar(width = 0.6, fill='#CC6666', color='#CC6666') +
  labs(x = 'Year', y = 'Number of Female Runners')

dev.copy(png,filename="./cs2Figures/barplot_women.png", width=400, height=400);
dev.off ();

## The distribution of performance for women 1999 races
women_df_agecat %>% 
  filter(year == 1999) %>% 
  select(time) %>% 
  summary()

## The distribution of performance for women 2012 races
women_df_agecat %>% 
  filter(year == 2012) %>% 
  select(time) %>% 
  summary()

## Density curves for the age of female runners in 1999 and 2012
women_df_agecat %>% 
  filter(year %in% c(1999, 2012)) %>% 
  ggplot(aes(age, color = factor(year))) +
  geom_line(stat = 'density') +
  scale_color_few(name = 'Year') +
  labs(x = 'Age', y = 'Density')

dev.copy(png,filename="./cs2Figures/density_women_1999_2012.png", width=400, height=400);
dev.off ();

## Compare these two distributions with a quantile-quantile plot
pts <- qqplot(women_df_agecat$age[women_df_agecat$year == 1999],
              women_df_agecat$age[women_df_agecat$year == 2012],
              plot.it = FALSE) %>% as_data_frame()
pts %>% 
  ggplot(aes(x, y)) +
  geom_point(fill='#CC6666', color='#CC6666') +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = 'Age in 1999', y = 'Age in 2012')

dev.copy(png,filename="./cs2Figures/qqplot_women_1999_2012.png", width=400, height=400);
dev.off ();

## lm_age for men
lm_age_m <- lm(time ~ age, data = men_df_agecat)
lm_age_m$coefficients
summary(lm_age_m)

## smoothScatter for men
smoothScatter(x = men_df_agecat$age, y = lm_age_m$residuals,xlab = "Age (years)", ylab = "Residuals")
abline(h = 0, col = "purple", lwd = 3)
 
m_resid.lo = loess(resids ~ age, 
                 data = data.frame(resids = residuals(lm_age_m),
                                   age = men_df_agecat$age))

age20to80 = 20:80

m_resid.lo.pr = predict(m_resid.lo, newdata = data.frame(age = age20to80))

lines(x = age20to80, y = m_resid.lo.pr, col = "green", lwd = 2)

dev.copy(png,filename="./cs2Figures/smoothScatter_men.png", width=400, height=400);
dev.off ();

## lm_age for women
lm_age_w <- lm(time ~ age, data = women_df_agecat)
lm_age_w$coefficients
summary(lm_age_w)

## smoothScatter for women
smoothScatter(x = women_df_agecat$age, y = lm_age_w$residuals,xlab = "Age (years)", ylab = "Residuals")
abline(h = 0, col = "purple", lwd = 3)
 
w_resid.lo = loess(resids ~ age, 
                 data = data.frame(resids = residuals(lm_age_w),
                                   age = women_df_agecat$age))

age20to80 = 20:80

w_resid.lo.pr = predict(w_resid.lo, newdata = data.frame(age = age20to80))

lines(x = age20to80, y = w_resid.lo.pr, col = "green", lwd = 2, lty = 3)

dev.copy(png,filename="./cs2Figures/smoothScatter_women.png", width=400, height=400);
dev.off ();

men_res_lo <- loess(time ~ age, data = men_df_agecat)
women_res_lo <- loess(time ~ age, data = women_df_agecat)

age20to80 <- 20:80
men_res_lo_pr <- predict(men_res_lo, data.frame(age = age20to80))
women_res_lo_pr <- predict(women_res_lo, data.frame(age = age20to80))

over50m <- pmax(0, men_df_agecat$age - 50)
over50w <- pmax(0, women_df_agecat$age - 50)

lm_over50m <- lm(time ~ age + over50m, data = men_df_agecat)
summary(lm_over50m)

lm_over50w <- lm(time ~ age + over50w, data = women_df_agecat)
summary(lm_over50w)

decades <- seq(30, 60, by = 10)
over_age_m <- map_dfc(decades, function(x) {
  name <- paste0('over', x)
  df <- data_frame(pmax(0, men_df_agecat$age - x))
  names(df) <- name
  df
})
tail(over_age_m)

decades <- seq(30, 60, by = 10)
over_age_w <- map_dfc(decades, function(x) {
  name <- paste0('over', x)
  df <- data_frame(pmax(0, women_df_agecat$age - x))
  names(df) <- name
  df
})
tail(over_age_w)

lm_piecewise_m <- men_df_agecat %>% 
  bind_cols(over_age_m) %>% 
  select(time, age, starts_with('over')) %>% 
  lm(time ~ ., data = .)
summary(lm_piecewise_m)

lm_piecewise_w <- women_df_agecat %>% 
  bind_cols(over_age_w) %>% 
  select(time, age, starts_with('over')) %>% 
  lm(time ~ ., data = .)
summary(lm_piecewise_w)

over_age_df <- map_dfc(decades, function(x) {
  name <- paste0('over', x)
  df <- data_frame(pmax(0, age20to80 - x))
  names(df) <- name
  df
}) %>% 
  bind_cols(age = age20to80, .)
tail(over_age_df)

predPiecewise_m = predict(lm_piecewise_m, over_age_df)

predPiecewise_w = predict(lm_piecewise_w, over_age_df)

plot(predPiecewise_m ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Run Time Prediction")

lines(x = age20to80, y = men_res_lo_pr,
      col = "green", lty = 2, lwd = 3)

legend("topleft", col = c("purple", "green"),
       lty = c(1, 2), lwd= 3,
       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")


dev.copy(png,filename="./cs2Figures/predPiecewise_men.png", width=400, height=400);
dev.off ();

plot(predPiecewise_w ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Run Time Prediction")

lines(x = age20to80, y = women_res_lo_pr,
      col = "green", lty = 2, lwd = 3)

legend("topleft", col = c("purple", "green"),
       lty = c(1, 2), lwd= 3,
       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")

dev.copy(png,filename="./cs2Figures/predPiecewise_women.png", width=400, height=400);
dev.off ();

men_df_agecat %>% 
  filter(year == 2012) %>% 
  select(time) %>% 
  summary()

women_df_agecat %>% 
  filter(year == 1999) %>% 
  select(time) %>% 
  summary()

age1999m = men_df_agecat[ men_df_agecat$year == 1999, "age" ]
age2012m = men_df_agecat[ men_df_agecat$year == 2012, "age" ]

age1999w = women_df_agecat[ men_df_agecat$year == 1999, "age" ]
age2012w = women_df_agecat[ men_df_agecat$year == 2012, "age" ]

plot(density(age1999m, na.rm = TRUE),
     ylim = c(0, 0.075), col = "purple",
     lwd = 3,  xlab = "Age (years)",  main = "")
lines(density(age2012m, na.rm = TRUE),
      lwd = 3, lty = 2, col="green")
legend("topleft", col = c("purple", "green"), lty= 1:2, lwd = 3,
       legend = c("1999m", "2012m"), bty = "n")

dev.copy(png,filename="./cs2Figures/density_men2_1999_2012.png", width=400, height=400);
dev.off ();

plot(density(age1999w, na.rm = TRUE),
     ylim = c(0, 0.075), col = "purple",
     lwd = 3,  xlab = "Age (years)",  main = "")
lines(density(age2012w, na.rm = TRUE),
      lwd = 3, lty = 2, col="green")
legend("topleft", col = c("purple", "green"), lty= 1:2, lwd = 3,
       legend = c("1999w", "2012w"), bty = "n")

dev.copy(png,filename="./cs2Figures/density_women2_1999_2012.png", width=400, height=400);
dev.off ();

## Loess curves fit to performance for 1999 and 2012 male runners
mR.lo99 = loess(time ~ age, men_df_agecat[ men_df_agecat$year == 1999,])
mR.lo.pr99 = predict(mR.lo99, data.frame(age = age20to80))
mR.lo12 = loess(time ~ age, men_df_agecat[ men_df_agecat$year == 2012,])
mR.lo.pr12 = predict(mR.lo12, data.frame(age = age20to80))
plot(mR.lo.pr99 ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Fitted Run Time (minutes)")
lines(x = age20to80, y = mR.lo.pr12,
      col = "green", lty = 2, lwd = 3)
legend("topleft", col = c("purple", "green"), lty = 1:2, lwd = 3,
       legend = c("1999m", "2012m"), bty = "n")

dev.copy(png,filename="./cs2Figures/loess_men_1999_2012.png", width=400, height=400);
dev.off ();

## Difference between Loess Curves
## This line plot shows the difference between the predicted run time for 2012 and 1999 male runners
lo_pr_99m <- predict(loess(time ~ age, 
                          data = men_df_agecat, 
                          subset = men_df_agecat$year == 1999),
                    data_frame(age = age20to80))
lo_pr_12m <- predict(loess(time ~ age, 
                          data = men_df_agecat, 
                          subset = men_df_agecat$year == 2012),
                    data_frame(age = age20to80))
data_frame(age = age20to80, diff = lo_pr_12m - lo_pr_99m) %>% 
  ggplot(aes(age, diff)) +
  geom_line(color ='#3366FF') +
  labs(x = 'Age', y = 'Difference in Fitted Curves (minutes)')

dev.copy(png,filename="./cs2Figures/loess_men_1999_2012.png", width=400, height=400);
dev.off ();

## Loess curves fit to performance for 1999 and 2012 female runners
wR.lo99 = loess(time ~ age, women_df_agecat[ women_df_agecat$year == 1999,])
wR.lo.pr99 = predict(wR.lo99, data.frame(age = age20to80))
wR.lo12 = loess(time ~ age, women_df_agecat[ women_df_agecat$year == 2012,])
wR.lo.pr12 = predict(wR.lo12, data.frame(age = age20to80))
plot(wR.lo.pr99 ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Fitted Run Time (minutes)")
lines(x = age20to80, y = wR.lo.pr12,
      col = "green", lty = 2, lwd = 3)
legend("topleft", col = c("purple", "green"), lty = 1:2, lwd = 3,
       legend = c("1999w", "2012w"), bty = "n")

dev.copy(png,filename="./cs2Figures/loess_women_1999_2012.png", width=400, height=400);
dev.off ();

men_res_lo <- loess(time ~ age, data = men_df_agecat)
men_res_lo_norm99 <- loess(Time ~ as.numeric(Age), data = men_99_norm)
men_res_lo_norm12 <- loess(Time ~ as.numeric(Age), data = men_12_norm)
women_res_lo <- loess(time ~ age, data = women_df_agecat)
women_res_lo_norm99 <- loess(Time ~ as.numeric(Age), data = women_99_norm)
women_res_lo_norm12 <- loess(Time ~ as.numeric(Age), data = women_12_norm)

age20to80 <- 20:80
men_res_lo_pr <- predict(men_res_lo, data.frame(age = age20to80))
men_res_lo_pr_norm99 <- predict(men_res_lo_norm99)
men_res_lo_pr_norm12 <- predict(men_res_lo_norm12)
women_res_lo_pr <- predict(women_res_lo, data.frame(age = age20to80))
women_res_lo_pr_norm99 <- predict(women_res_lo_norm99)
women_res_lo_pr_norm12 <- predict(women_res_lo_norm12)

summary(men_res_lo_pr_norm99)
summary(men_res_lo_pr_norm12)
summary(women_res_lo_pr_norm99)
summary(women_res_lo_pr_norm12)


# Normalized loess predictions
plot(x = men_12_norm$Age, y = men_res_lo_pr_norm12,
     col = "purple", lty = 2, lwd = 3,
     xlab = "Age (years)", ylab = "Normalized Run Time")
lines(x = men_99_norm$Age, y = men_res_lo_pr_norm99,
      col = "purple", lty = 2, lwd = 3)
legend("bottomleft", col = c("black", "purple"), lty = 1:2, lwd = 3,
       legend = c("2012w", "1999w"), bty = "n")

dev.copy(png,filename="./cs2Figures/loessDiff_men_1999_2012_normalized.png", width=400, height=400);
dev.off ();

# Normalized loess predictions
plot(x = women_12_norm$Age, y = women_res_lo_pr_norm12,
     col = "purple", lty = 2, lwd = 3,
     xlab = "Age (years)", ylab = "Normalized Run Time")
lines(x = women_99_norm$Age, y = women_res_lo_pr_norm99,
      col = "purple", lty = 2, lwd = 3)
legend("bottomleft", col = c("black", "purple"), lty = 1:3, lwd = 3,
       legend = c("2012w", "1999w"), bty = "n")

dev.copy(png,filename="./cs2Figures/loessDiff_women_1999_2012_normalized.png", width=400, height=400);
dev.off ();

## Difference between Loess Curves
## This line plot shows the difference between the predicted run time for 2012 and 1999 female runners
lo_pr_99w <- predict(loess(time ~ age, 
                          data = women_df_agecat, 
                          subset = women_df_agecat$year == 1999),
                    data_frame(age = age20to80))
lo_pr_12w <- predict(loess(time ~ age, 
                          data = women_df_agecat, 
                          subset = women_df_agecat$year == 2012),
                    data_frame(age = age20to80))
data_frame(age = age20to80, diff = lo_pr_12w - lo_pr_99w) %>% 
  ggplot(aes(age, diff)) +
  geom_line(color='#CC6666') +
  labs(x = 'Age', y = 'Difference in Fitted Curves (minutes)')

dev.copy(png,filename="./cs2Figures/loessDiff_women_1999_2012.png", width=400, height=400);
dev.off ();

## Section 6: Constructe a record for an individual runner across years 

## How many entrants are there over the 14 years?
## How many unique names are there among these entrants?
## How many names appear twice, 3 times, 4 times, etc. and what name occurs most often?
## How often does a name appear more than once in a year?

# How many entrants are there over the 14 races?
length(men_df_agecat$name)
length(women_df_agecat$name)

# How many unique names are there?
length(unique(men_df_agecat$name))
length(unique(women_df_agecat$name))

# How many names appear once, twice, etc.?
table(table(men_df_agecat$name))
table(table(women_df_agecat$name))

# Which name appears 30 times?
head(sort(table(men_df_agecat$name), decreasing = TRUE), 1)
head(sort(table(women_df_agecat$name), decreasing = TRUE), 1)

# Let's examine other information about these 33 Michael Smiths
msmith <- men_df_agecat %>% filter(name == 'Michael Smith')
jjohnson <- women_df_agecat %>% filter(name == 'Jennifer Johnson')

# The hometowns
msmith %>% count(home) %>% arrange(desc(n)) %>% head()
jjohnson %>% count(home) %>% arrange(desc(n)) %>% head()

## Remove punctuation such as a period after someone's middle initial and any stray commas
men_df_agecat <- men_df_agecat %>% 
  mutate(name = str_replace_all(name, '[,.]', ''))

women_df_agecat <- women_df_agecat %>% 
  mutate(name = str_replace_all(name, '[,.]', ''))

## Figure out how many times a name appears in the same year
tab_name_yr_m <- table(men_df_agecat$year, men_df_agecat$name)
tab_name_yr_w <- table(women_df_agecat$year, women_df_agecat$name)

## Call 'max()' to find the cell in the table with the greatest count
max(tab_name_yr_m)
max(tab_name_yr_w)

### Check with class(), mode(), and names()
class(tab_name_yr_m)
mode(tab_name_yr_m)
names(attributes(tab_name_yr_m))

class(tab_name_yr_w)
mode(tab_name_yr_w)
names(attributes(tab_name_yr_w))

## Call dim() and colnames() to find the implications of this data structure
dim(tab_name_yr_m)
head(colnames(tab_name_yr_m), 3)

dim(tab_name_yr_w)
head(colnames(tab_name_yr_w), 3)

## Use which() to find the row and column location and need to include the 'arr.ind' argument in the call
idx_max_m <- which(tab_name_yr_m == max(tab_name_yr_m), arr.ind = TRUE)
idx_max_m

idx_max_w <- which(tab_name_yr_w == max(tab_name_yr_w), arr.ind = TRUE)
idx_max_w

## Locate the names (it is Michael Brown, not the Michael Smith)
colnames(tab_name_yr_m)[idx_max_m[2]]
colnames(tab_name_yr_w)[idx_max_w[2]]

## Create yob() in the data frame to drive an approximation to year of birth
men_df_agecat <- men_df_agecat %>% 
  mutate(yob = year - age)

women_df_agecat <- women_df_agecat %>% 
  mutate(yob = year - age)

## Check the values for these new and cleaned variables for 'Michael Brown'
men_df_agecat %>% 
  filter(name == 'Michael Brown') %>% 
  arrange(yob) %>% 
  select(year, name, home, yob, time)

women_df_agecat %>% 
  filter(name == 'Aarti Shah') %>% 
  arrange(yob) %>% 
  select(year, name, home, yob, time)

## Paste together the cleaned name and the derived year of birth
men_df_agecat <- men_df_agecat %>% 
  mutate(id = paste(name, yob, sep = '_'))

## Determine how many times each 'id' appears in 'men_df_agecat' 
races <- men_df_agecat %>% count(id)

## Select those IDs that appear at least 8 times 
races8 <- races %>% filter(n >= 8) %>% .$id

## Subset 'men_df_agecat' to select the entries belonging to these identifiers 
men8 <- men_df_agecat %>% filter(id %in% races8)

## Organize the data frame so that entries with the same 'id' are contiguous
men8 <- men8 %>% arrange(id)

## Alternatively, create a liost to store elements for each ID in 'races8'
men8L <- men8 %>% group_by(id) %>% nest() %>% .$data
names(men8L) <- races8

## Check how many left IDs
length(unique(men8$id))

## Determine which satisfy 20 min constraint
gap_time <- map_lgl(men8L, ~ any(abs(diff(.$time)) > 20))

## How many of these runners have gaps of more than 20 minutes
sum(gap_time)

## Slightly reformatted displays of the first two of these athletes are
map(men8L[gap_time][1:2], ~ .[ ,c('year', 'home', 'name', 'yob', 'time')])

# gsub("[[:blank:]][a-z]{2}$", "", home)
# gsub("[[:blank:]][a-z]{2}$", "", home)

## Determine how many characters are in each value for home
home_len_m <- nchar(men_df_agecat$home)
home_len_w <- nchar(women_df_agecat$home)

## use it to extract the last two characters and add them back to our data frame 
men_df_agecat <- men_df_agecat %>% 
  mutate(state = str_sub(home, start = home_len_m - 1, end = home_len_m))

women_df_agecat <- women_df_agecat %>% 
  mutate(state = str_sub(home, start = home_len_w - 1, end = home_len_w))

## Set the 2006 values to `NA`:
men_df_agecat$state[men_df_agecat$year == 2006] <- NA
women_df_agecat$statewo[women_df_agecat$year == 2006] <- NA

## Recreate the new 'id' so that it includes 'state'
men_df_agecat <- men_df_agecat %>% 
  mutate(id = paste(name, yob, state, sep = '_'))

women_df_agecat <- women_df_agecat %>% 
  mutate(id = paste(name, yob, state, sep = '_'))

## Again select those 'id's that occur at least 8 times 
races_m <- men_df_agecat %>% count(id)
races8_m <- races_m %>% filter(n >= 8) %>% .$id
men8 <- men_df_agecat %>% filter(id %in% races8_m) %>% arrange(id)
men8L <- men8 %>% group_by(id) %>% nest() %>% .$data
names(men8L) <- races8_m

races_w <- women_df_agecat %>% count(id)
races8_w <- races_w %>% filter(n >= 8) %>% .$id
women8 <- women_df_agecat %>% filter(id %in% races8_w) %>% arrange(id)
women8L <- women8 %>% group_by(id) %>% nest() %>% .$data
names(women8L) <- races8_w

## length() for getting the number of runners who havea completed 8 races
length(races8_m)
length(races8_w)

## Section 7: Modelling

## Divide the runners into 9 groups to make 9 plots in a 3-by-3 grid
## Assign roughly the same number of runners to each group
men8 <- men8 %>%
  mutate(group = group_indices(., id) %% 9 + 1)

women8 <- women8 %>%
  mutate(group = group_indices(., id) %% 9 + 1)

## Plot it with one call to 'ggplot()' for men runners
ggplot(men8, aes(age, time, color = id)) +
  geom_line(show.legend = FALSE) +
  facet_wrap( ~ group)

dev.copy(png,filename="./cs2Figures/group9_men_1999_2012.png", width=400, height=400);
dev.off ();


## Plot it with one call to 'ggplot()' for women runners
ggplot(women8, aes(age, time, color = id)) +
  geom_line(show.legend = FALSE) +
  facet_wrap( ~ group)

dev.copy(png,filename="./cs2Figures/group9_women_1999_2012.png", width=400, height=400);
dev.off ();

## Draw a plot with the fitted lines to capture each runner's performance in group 9
men8 %>% 
  filter(group == 9) %>% 
ggplot(aes(age, time, color = id)) +
  geom_line(show.legend = FALSE) +
  geom_smooth(aes(group = id), 
              method = 'lm', 
              se = FALSE, 
              color = 'grey50', 
              size = 0.5,
              linetype = 'dashed',
              show.legend = FALSE) +
  facet_wrap( ~ group)

dev.copy(png,filename="./cs2Figures/group9th_men_1999_2012.png", width=400, height=400);
dev.off ();

## Draw a plot with the fitted lines to capture each runner's performance in group 9
women8 %>% 
  filter(group == 9) %>% 
ggplot(aes(age, time, color = id)) +
  geom_line(show.legend = FALSE) +
  geom_smooth(aes(group = id), 
              method = 'lm', 
              se = FALSE, 
              color = 'grey50', 
              size = 0.5,
              linetype = 'dashed',
              show.legend = FALSE) +
  facet_wrap( ~ group)

dev.copy(png,filename="./cs2Figures/group9th_women_1999_2012.png", width=400, height=400);
dev.off ();

## Examine the runner-to-runner variability, fit lines to all 306 athletes

lm_coefs_m <- men8 %>%
  group_by(id) %>% 
  do(fit = lm(time ~ age, data = .)) %>% 
  tidy(fit)

long_coefs_m <- men8 %>% 
  group_by(id) %>% 
  summarise(med_age = median(age)) %>% 
  mutate(coef = lm_coefs_m$estimate[lm_coefs_m$term == 'age'])

## Coefficients from Longitudinal Analysis of Athletes
## Scatter plot displays the slope of the fitted line to each of the 300+ runners who competed in at least 8 Cherry Blossom road races 
## A negative coefficient indicates the runner is getting faster as he ages
## The plot includes a least squares fitted line and a loess fitted curve
ggplot(long_coefs_m, aes(med_age, coef)) +
  geom_point(size = 0.7) +
  geom_smooth(method = 'lm', size = 0.7, color = 'darkgreen', se = FALSE) +
  geom_smooth(method = 'loess', size = 0.7, linetype = 'dashed', se = FALSE) +
  geom_hline(yintercept = 0, color = 'grey70', size = 0.3) +
  labs(x = 'Median age', y = 'Coefficient (minutes per race / year)')

dev.copy(png,filename="./cs2Figures/coef_variability_men.png", width=400, height=400);
dev.off ();

## How these coefficients vary with age?
long_fit_m <- lm(coef ~ med_age, data = long_coefs_m)
summary(long_fit_m)

## Examine the runner-to-runner variability, fit lines to all 306 athletes

lm_coefs_w <- women8 %>%
  group_by(id) %>% 
  do(fit = lm(time ~ age, data = .)) %>% 
  tidy(fit)

long_coefs_w <- women8 %>% 
  group_by(id) %>% 
  summarise(med_age = median(age)) %>% 
  mutate(coef = lm_coefs_w$estimate[lm_coefs_w$term == 'age'])

## Coefficients from Longitudinal Analysis of Athletes
## Scatter plot displays the slope of the fitted line to each of the 300+ runners who competed in at least 8 Cherry Blossom road races 
## A negative coefficient indicates the runner is getting faster as he ages
## The plot includes a least squares fitted line and a loess fitted curve
ggplot(long_coefs_w, aes(med_age, coef)) +
  geom_point(size = 0.7) +
  geom_smooth(method = 'lm', size = 0.7, color = 'darkgreen', se = FALSE) +
  geom_smooth(method = 'loess', size = 0.7, color = '#CC6666', linetype = 'dashed', se = FALSE) +
  geom_hline(yintercept = 0, color = 'grey70', size = 0.3) +
  labs(x = 'Median age', y = 'Coefficient (minutes per race / year)')

dev.copy(png,filename="./cs2Figures/coef_variability_women.png", width=400, height=400);
dev.off ();

## How these coefficients vary with age?
long_fit_w <- lm(coef ~ med_age, data = long_coefs_w)
summary(long_fit_w)

## -- The end of codes for case study 2 --
