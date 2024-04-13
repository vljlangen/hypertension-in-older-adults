
###########################################################################
###########################################################################
###                                                                     ###
###                               TABLE 1                               ###
###                                                                     ###
###########################################################################
###########################################################################

library(pacman)
p_load(dplyr, tidyverse, bannerCommenter,
       lubridate, janitor, tableone, psych,
       flextable)


###########################################################################
###########################################################################
###                                                                     ###
###              EXCLUDE PARTICIPANTS WITH MISSING BP DATA              ###
###                                                                     ###
###########################################################################
###########################################################################

# TUVA1
tuva_elig <- tuva %>% drop_na(SRR_70_istuen, SRRD_70_istuen)
tuva_n <- tuva_elig %>% nrow()

# Store the initial survey year into a vector






# UTUVA1

utuva_elig <- utuva %>% drop_na(B8_3_A7isys_U70, B8_3_A7idia_U70)
utuva_n <- utuva_elig %>% nrow()


############################################################################
############################################################################
###                                                                      ###
###                     STORE INITIAL YEAR OF SURVEY                     ###
###                                                                      ###
############################################################################
############################################################################


# TUVA1

tuva_elig$PAIVAMA_70_PVM %>% head

initial_survey_year_tuva <- paste0(
  
  year(min(tuva_elig$PAIVAMA_70_PVM, na.rm = T)),
  "\u2013",
  year(max(tuva_elig$PAIVAMA_70_PVM, na.rm = T))
)

# Confirm
initial_survey_year_tuva




# UTUVA2

# Convert exam date to date format
utuva_elig$B8_3_PVM_U70 <- as.Date(utuva_elig$B8_3_PVM_U70, format = "%d.%m.%Y")


initial_survey_year_utuva <- paste0(
  
  year(min(utuva_elig$B8_3_PVM_U70, na.rm = T)),
  "\u2013",
  year(max(utuva_elig$B8_3_PVM_U70, na.rm = T))
)

# Confirm
initial_survey_year_utuva


 
###########################################################################
###########################################################################
###                                                                     ###
###                       PREPARE VARIABLES, TUVA                       ###
###                                                                     ###
###########################################################################
###########################################################################

# Mutate a new variable for pathological RR values
tuva_elig <- tuva_elig %>% 
  mutate(`Hypertensive blood pressure reading` = ifelse(
            SRR_70_istuen >= 140 | SRRD_70_istuen >= 90, 1, 0))


# Retain only necessary variables
tuva_elig <- tuva_elig %>% select(IKA_tutkpvm_70, SUKUPUOLI, SRR_70_istuen,
                                  SRRD_70_istuen, `Hypertensive blood pressure reading`,
                                  hypertension, antihypertensive_med)

# Convert categorical variables into factor
tuva_elig <- tuva_elig %>% mutate(SUKUPUOLI =
                                    factor(SUKUPUOLI, levels = c(1, 2), labels = c("Men", "Women")))

tuva_elig <- tuva_elig %>% mutate(hypertension =
                                    factor(hypertension, levels = c(1, 0), labels = c("Yes", "No")))


tuva_elig <- tuva_elig %>% mutate(antihypertensive_med =
                                    factor(antihypertensive_med, levels = c(1, 0), labels = c("Yes", "No")))

tuva_elig <- tuva_elig %>% mutate(`Hypertensive blood pressure reading` =
                                    factor(`Hypertensive blood pressure reading`,
                                           levels = c(0, 1), labels = c("No", "Yes")))


# Rename with nice names
tuva_elig <- tuva_elig %>% rename(`Age (years)` = IKA_tutkpvm_70,
                                  `Sex` = SUKUPUOLI,
                                  `Systolic blood pressure (mmHg)` = SRR_70_istuen,
                                  `Diastolic blood pressure (mmHg)` = SRRD_70_istuen,
                                  `Prevalent hypertension` = hypertension,
                                  `Use of any antihypertensive medication` = antihypertensive_med)




## Get variables names
dput(names(tuva_elig))

# Copy from the result of the above command below:

## Vector of variables to summarize
myVars <- c("Age (years)", "Sex", "Systolic blood pressure (mmHg)", "Diastolic blood pressure (mmHg)", 
            "Hypertensive blood pressure reading",
            "Prevalent hypertension", "Use of any antihypertensive medication")

## Vector of categorical variables that need transformation
catVars <- c("Sex", "Hypertensive blood pressure reading",
             "Prevalent hypertension", "Use of any antihypertensive medication")

# Create a strata variable
tuva_elig$cohort <- "1920-born TUVA cohort"


###########################################################################
###########################################################################
###                                                                     ###
###                       PREPARE VARIABLES, UTUVA                      ###
###                                                                     ###
###########################################################################
###########################################################################

#################################################################
##                        Calculate age                        ##
#################################################################

# First, convert dates to date format

# (utuva$SyntPVM already is in date format)
is.Date(utuva_elig$SyntPVM)

# # Convert exam date to date format
# utuva_elig$B8_3_PVM_U70 <- as.Date(utuva_elig$B8_3_PVM_U70, format = "%d.%m.%Y")

# Confirm
is.Date(utuva_elig$B8_3_PVM_U70)

# Calculate age
utuva_elig$age <- as.numeric(difftime(utuva_elig$B8_3_PVM_U70, utuva_elig$SyntPVM, units = "days") / 365.25)

# Turns out that one age has erroneous data (age = 61.4 years)
min(utuva_elig$age)

# Identify that person
utuva_elig %>% select(ID1991, ID2011, SyntPVM, B8_3_PVM_U70, age) %>%  filter(age < 65 )

# (The person has for ID2011 the value 665)


# The exam date year is wrong. When we sort the data, the correct year is with almost
# 100% surity "2011":
sort(utuva_elig$B8_3_PVM_U70) %>% head()
max(utuva_elig$B8_3_PVM_U70)

# Correct that cell of data with Base R
#
#Find the row index where ID matches the erroneous_ID
#
row_index <- which(utuva_elig$ID2011 == 665)

# Identify the column index
column_index <- which(colnames(utuva_elig) == "B8_3_PVM_U70")

# Confirm
utuva_elig[row_index, column_index]

# Correct the erroneous value
correct_date <- as.Date("2011-11-18", format = "%Y-%m-%d")
is.Date(correct_date)

utuva_elig[row_index, column_index] <- correct_date

# Confirm
utuva_elig[row_index, column_index]
is.Date(utuva_elig$B8_3_PVM_U70)

# Recalculate ages
utuva_elig$age <- as.numeric(difftime(utuva_elig$B8_3_PVM_U70, utuva_elig$SyntPVM, units = "days") / 365.25)



 
#################################################################
##                      Prepare variables                      ##
#################################################################

# Mutate a new variable for pathological RR values
utuva_elig <- utuva_elig %>% 
  mutate(`Hypertensive blood pressure reading` = ifelse(
    B8_3_A7isys_U70 >= 140 | B8_3_A7idia_U70 >= 90, 1, 0))


# Retain only necessary variables
utuva_elig <- utuva_elig %>% select(age, SUKUPUOLI, B8_3_A7isys_U70,
                                    B8_3_A7idia_U70, `Hypertensive blood pressure reading`,
                                    hypertension, antihypertensive_med)



# Convert categorical variables into factor
utuva_elig <- utuva_elig %>% mutate(SUKUPUOLI =
                                    factor(SUKUPUOLI, levels = c(1, 2), labels = c("Men", "Women")))

utuva_elig <- utuva_elig %>% mutate(hypertension =
                                    factor(hypertension, levels = c(1, 0), labels = c("Yes", "No")))


utuva_elig <- utuva_elig %>% mutate(antihypertensive_med =
                                    factor(antihypertensive_med, levels = c(1, 0), labels = c("Yes", "No")))

utuva_elig <- utuva_elig %>% mutate(`Hypertensive blood pressure reading` =
                                    factor(`Hypertensive blood pressure reading`,
                                           levels = c(0, 1), labels = c("No", "Yes")))


# Rename with nicer names
utuva_elig <- utuva_elig %>% rename(`Age (years)` = age,
                                  `Sex` = SUKUPUOLI,
                                  `Systolic blood pressure (mmHg)` = B8_3_A7isys_U70,
                                  `Diastolic blood pressure (mmHg)` = B8_3_A7idia_U70,
                                  `Prevalent hypertension` = hypertension,
                                  `Use of any antihypertensive medication` = antihypertensive_med)


 



# Create a strata variable
utuva_elig$cohort <- "1940-born UTUVA cohort"


# utuva_age <- mean(utuva_elig$age, na.rm = T)
# utuva_age_sd <- sd(utuva_elig$age, na.rm = T)



############################################################################
############################################################################
###                                                                      ###
###                       JOIN TUVA AND UTUVA DATA                       ###
###                                                                      ###
############################################################################
############################################################################


tuva_utuva_elig <- rbind(tuva_elig, utuva_elig)



###########################################################################
###########################################################################
###                                                                     ###
###         RUN PSYCH PACKAGE ANALYSIS FOR CONTINUOUS VARIABLES         ###
###                                                                     ###
###########################################################################
###########################################################################

tuva_utuva_elig %>% select(`Age (years)`, `Systolic blood pressure (mmHg)`,
                           `Diastolic blood pressure (mmHg)`) %>% 
  multi.hist(global = FALSE)


############################################################################
############################################################################
###                                                                      ###
###                        CREATE TABLEONE OBJECT                        ###
###                                                                      ###
############################################################################
############################################################################

# Run relevel for `Prevalent hypertension` and `Use of any antihypertensive medication`,
# otherwise the table will show the percentage of those with NOT these qualities
 
# Relevel Prevalent hypertension
tuva_utuva_elig$`Prevalent hypertension` <- relevel(tuva_utuva_elig$`Prevalent hypertension`, "No")

# Confirm
tuva_utuva_elig$`Prevalent hypertension`

# Relevel Use of any antihypertensive medication
tuva_utuva_elig$`Use of any antihypertensive medication` <- relevel(tuva_utuva_elig$`Use of any antihypertensive medication`, "No")

# Confirm
tuva_utuva_elig$`Use of any antihypertensive medication`




## Create a TableOne object
tableone_obj <- CreateTableOne(data = tuva_utuva_elig, vars = myVars, factorVars = catVars,
                               addOverall = F,
                               test = T,
                               strata = "cohort")

# Display tableone object
tableone_obj

# Print the object (to get rid of the special structure of tableone)
tableone_obj_print <- print(tableone_obj,
                  quote = F,
                  noSpaces = T,
                  #smd = T,
                  # missing = T,
                  contDigits = 1,
                  printToggle = F,
                  dropEqual = T,
                  explain = F,
                  #nonnormal = c(put your nonnormal variables here),
                  #formatOptions = list(big.mark = ",")
                  )

#Display the printed object
tableone_obj_print

# Convert to data frame
ft_df <- as.data.frame(tableone_obj_print)

# Move the rownames to column index 1
ft_df <- ft_df %>%
  rownames_to_column(var = "Characteristic")

# Define the sex as women (as seen above, the figures are for women)
ft_df[3,1] <- "Women"

# Delete the "test" column
ft_df <- ft_df %>% select(- test)

#Display data frame
ft_df

#Add the survey years of each cohort



ft_df <- ft_df %>% add_row(Characteristic="Initial survey year",
                           `1920-born TUVA cohort`= initial_survey_year_tuva,
                           `1940-born UTUVA cohort`= initial_survey_year_utuva,
                           p="",
                          .before=2)






############################################################################
############################################################################
###                                                                      ###
###                           CREATE FLEXTABLE                           ###
###                                                                      ###
############################################################################
############################################################################


#get_flextable_defaults()


set_flextable_defaults(
  border.color = "black",

  border.width = 1.0)

#init_flextable_defaults()

ft <- ft_df %>% flextable()

ft <- fontsize(ft, size = 12, part = "all")

ft <- ft %>% autofit()

# Add theme (this cannot be done later as it messes up with the align commands later on)
ft <- theme_booktabs(ft)

# Align first column to left
ft <- align(ft, j = 1, align = "left", part = "all")

# Align the rest to center
ft <- align(ft, j = 2:4, align = "center", part = "all")

#Change the header to bold
ft <- ft %>%
  bold(part = "header", bold = TRUE)

# Add a header
ft <- ft %>% add_header_lines("Table 1. Baseline characteristics of the study cohorts.")

# Add a footer
ft <- ft %>% add_footer_lines(
             as_paragraph(
  "Values are means (and standard deviations) for continuous data and numbers (and percentages) for categorical data. Continuous variables were analyzed with t-tests and categorical variables with chi-square tests to identify significant differences. ",
  
  as_sup("a"), "Hypertensive blood pressure reading was defined as either having a systolic blood pressure ≥140 mmHg or diastolic blood pressure ≥90 mmHg. ",
  
  as_sup("b"), "Hypertension was defined as either having a hypertensive blood pressure reading or confirmed use of antihypertensive medication."))

# Edit footer font size
ft <- fontsize(ft, size = 12, part = "footer")

# Change to Arial font
ft <- font(
  ft,
  i = NULL,
  j = NULL,
  "Arial",
  part = "all")
  
# Add superscripts to body
ft <- compose(ft,
              i = 7,
              j = 1,
              part = "body",
              value = as_paragraph(
                "Hypertensive blood pressure reading",
                as_sup("a")
              )
)

# Add superscripts to body
ft <- compose(ft,
              i = 8,
              j = 1,
              part = "body",
              value = as_paragraph(
                "Prevalent hypertension",
                as_sup("b")
              )
)

# Display ft
ft




##################################################################
##                        Export as docx                        ##
##################################################################


## Save the resulting table as .docx (and .png)

# Save as word .docx
save_as_docx(ft, path = "article1/table1_flextable.docx",
             pr_section =
               prop_section(page_size = page_size(orient = "landscape"),
                            type = "continuous"))


