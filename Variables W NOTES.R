#+ Wrangling: tidy variables with NOTES
#+ Jan 29, 2025

# Setup ----------------
# Packages
  library(tidyverse)
  library(knitr)

# Custom function
  fctr = function(...) {
    args = rlang::list2(...)
    rhs = map(args, rlang::f_rhs)
    cases = case_when( !!!args )
    exec(fct_relevel, cases, !!!rhs)
  }  
  
# Data
  df <- read_csv('water.csv')
  
  
# Get the data you want SELECTING VARIABLES ----------
# Identifying two ways to get what you want

  df2 <-
    df%>%
    select(
      hh_id, age, 
      hv201:hv215,#This colon says that in this sequence of variables, pull those variables
      -hv213 #Sometimes we want all the data minus 1 or 2, just add a (-)
    )
#The dataset will follow the sequence in order so hh_id will be the first variable

  
#Select cases (to keep) SELECTING OBSERVATION

df3 <-
  df %>%
  filter(
    sex_binary == "male", # these conditions could work together or separate (& = AND) (| = OR)
    age <=10 #be under the age of 10
  )

rm(df2, df3) #get rid of objects in Environment

#Question: What is the difference between df3 = df and df3 <- df?? ****
  

# mutate basics ------------------  
#Mutate adds new variables that you want


# Create columns (asking for three new columns) AND SHIFTING THE PLACEMENT OF COLOUMS
  df2 <-
    df %>%
    mutate(
      rural = if_else(hv025 == 'rural', 1, 0), # new coloumn using variable and changing categorical to                                                   numerical
      age_months = age * 12,
      age_dev = age - median(age, na.rm = T),
      .after = 1, # You can see that the new columns were added, but to the back of the dataset... doing                     .before/.after allows us to change the place where the variable is!
      .keep = 'unused' #We have lots of variables, lets get rid of everything BUT the ones we created                            here (3 variables) you can do .keep = 'none' BUT also unused, used, all.
      
    )

# Decide what to keep ***?? Question: What does this explain
  df2 <-
    df %>%
    mutate(
      r_id = row_number(),
      age_yrs = age * 12,
      .keep = 'none' # alts: 'used', 'unused'
    )

  
# NAs and Strings ---------------- 
# unwanted strings

#if there is any character variable in a column it reads all values as characters so we must get rid of them
#water_mins shows how long it takes the respondent to walk to their water source but its has values that are read as characters such as "on premises" and "don't know" so we have to decide whether we keep them or change them. Now we will change them to numerical values.
  
   df2 <-
    df %>%
    mutate(
      wat1 = as.numeric(water_mins), # changes strings to NA
      waterFetchMins = case_when(
        water_mins == 'on premises' ~ 0,
        water_mins %in% c('999', "don't know") ~ NA, 
        TRUE ~ as.numeric(water_mins) 
      ),
      .keep = 'used'
    )  # review the Warning messages; check: 
  
  summary(df2$water_mins)

#****JUMPED TO FACTOR AND LABELS

# Combine or split strings
  df2 <-
    df %>%
    mutate(
      r_id = row_number(),
      region_x_rid = paste(region_id, r_id, sep = '_'),
      region2 = str_extract(region_x_rid, "[^_]+"),
      rid2 = str_extract(region_x_rid, "[^_]*$"),
      .keep = 'used'
    )

# Change values to/from missing
  df2 <-
    df %>%
    mutate(
      attend1 = na_if(hv121, '9'),
      attend2 = if_else(hv121 == '9', NA, hv121),
      attend3 = if_else(is.na(attend2), 'dk/nr', attend2),
      .keep = 'used'
    )
  
  
  
  
#*****CONTINUED WORK HERE


# Factors and labels -------------  

  #Not just a character but a factor, where the order appeared matters.
count(df, hh_income)
  
  
  # Relevel (Change order)
  df2 <-
    df %>%
    mutate(
      inc_quint = fct_relevel(
        hh_income, # source variable
        'poorest','poorer', 'middle','richer','richest'
      )
    )
  
  
#OR WE CAN ALSO.....
  
# create ordered categories
  
  df2 <-
    df %>%
    mutate(age_grp = case_when(
      age <= 9 ~ 'youngest',
      age %in% 10:12 - 'middle',
      age >= 13 ~ 'oldest'
    ),
    age_fctr = fctr(
      age <= 9 ~ 'youngest',
      age %in% 10:12 - 'middle',
      age >= 13 ~ 'oldest'
    )
             )
  
count(df2, age_fctr) #***Question: Please help fix the code above to show imapct of factor
  
#factor can allow to use as a numeric grouping! And we can find mean
  
  
  df2 <-
    df %>%
    mutate(
      ageFact = fctr(
        age <= 9 ~ '6-9 years',
        age %in% 10:12 ~ '10-12',
        age >= 13 ~ '13-15' # vs TRUE
      ),
      .keep = 'used'
    )
  
  #REPEAT FUNCTIONS-----------------
  
# Repeat across columns ----------  
# Mutate across
  
   
  df2 <-
    df %>%
    mutate(
      across(where(is.character), ~na_if(.x, '9'))
    )
  
  
## SKIPPED THIS PART BELOW ***
  
# Grouping -----------------------
# Aggregate: group and summarize
  df2 <-
    df %>%
    group_by(hh_income) %>%
    summarize(
      mean_edu = mean(eduyrs, na.rm = T)
    )
  
# Group level stats: group and mutate
  df2 <-
    df %>%
    group_by(hh_income) %>%
    mutate(
      mean_edu = mean(eduyrs, na.rm = T)
    ) %>%
    ungroup()
  