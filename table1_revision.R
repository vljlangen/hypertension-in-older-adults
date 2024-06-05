
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
       flextable, officer)

# Import data

tuva <- readRDS("data/article1tuva_bp_meds_final.rds")
utuva <- readRDS("data/article1utuva_bp_meds_final.rds")



###########################################################################
###########################################################################
###                                                                     ###
###              EXCLUDE PARTICIPANTS WITH MISSING BP DATA              ###
###                                                                     ###
###########################################################################
###########################################################################


# TUVA1
tuva_elig <- tuva %>% drop_na(tuva1sbp, tuva1dbp)
tuva_n <- tuva_elig %>% nrow()
tuva_n


# UTUVA1

utuva_elig <- utuva %>% drop_na(utuva1sbp, utuva1dbp)
utuva_n <- utuva_elig %>% nrow()
utuva_n


# Store the initial survey year into a vector

 





############################################################################
############################################################################
###                                                                      ###
###                     STORE INITIAL YEAR OF SURVEY                     ###
###                                                                      ###
############################################################################
############################################################################


# TUVA1

tuva_elig$tuva1_date %>% head

initial_survey_year_tuva <- paste0(
  
  year(min(tuva_elig$tuva1_date, na.rm = T)),
  "\u2013",
  year(max(tuva_elig$tuva1_date, na.rm = T))
)

# Confirm
initial_survey_year_tuva




# UTUVA1



# # Convert exam date to date format
# utuva_elig$B8_3_PVM_U70 <- as.Date(utuva_elig$B8_3_PVM_U70, format = "%d.%m.%Y")
# 
# 
# 
# # There is one erroneous date:
# head(sort(year(utuva_elig$B8_3_PVM_U70)))


utuva_elig$utuva1_date %>% head

initial_survey_year_utuva <- paste0(
  
  year(min(utuva_elig$utuva1_date, na.rm = T)),
  "\u2013",
  year(max(utuva_elig$utuva1_date, na.rm = T))
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
    tuva1sbp >= 140 | tuva1dbp >= 90, 1, 0))


# Retain only necessary variables
tuva_elig <- tuva_elig %>% select(IKA_tutkpvm_70, SUKUPUOLI, tuva1sbp,
                                  tuva1dbp, `Hypertensive blood pressure reading`,
                                  hypertension_tuva1, antihypertensive_med)

# Convert categorical variables into factor
tuva_elig <- tuva_elig %>% mutate(SUKUPUOLI =
                                    factor(SUKUPUOLI, levels = c(1, 2), labels = c("Men", "Women")))

tuva_elig <- tuva_elig %>% mutate(hypertension_tuva1 =
                                    factor(hypertension_tuva1, levels = c(1, 0), labels = c("Yes", "No")))


tuva_elig <- tuva_elig %>% mutate(antihypertensive_med =
                                    factor(antihypertensive_med, levels = c(1, 0), labels = c("Yes", "No")))

tuva_elig <- tuva_elig %>% mutate(`Hypertensive blood pressure reading` =
                                    factor(`Hypertensive blood pressure reading`,
                                           levels = c(0, 1), labels = c("No", "Yes")))


# Rename with nice names
tuva_elig <- tuva_elig %>% rename(`Age (years)` = IKA_tutkpvm_70,
                                  `Sex` = SUKUPUOLI,
                                  `Systolic blood pressure (mmHg)` = tuva1sbp,
                                  `Diastolic blood pressure (mmHg)` = tuva1dbp,
                                  `Prevalent hypertension` = hypertension_tuva1,
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

# Confirm that study date is in Date format
is.Date(utuva_elig$utuva1_date)

# Calculate age
utuva_elig$age <- as.numeric(difftime(utuva_elig$utuva1_date, utuva_elig$SyntPVM, units = "days") / 365.25)

# Corfirm that age looks OK
utuva_elig$age %>% psych::describe()


initial_survey_year_utuva <- paste0(
  
  year(min(utuva_elig$utuva1_date, na.rm = T)),
  "\u2013",
  year(max(utuva_elig$utuva1_date, na.rm = T))
)

# Confirm
initial_survey_year_utuva


 
#################################################################
##                      Prepare variables                      ##
#################################################################

# Mutate a new variable for pathological RR values
utuva_elig <- utuva_elig %>% 
  mutate(`Hypertensive blood pressure reading` = ifelse(
    utuva1sbp >= 140 | utuva1dbp >= 90, 1, 0))


# Retain only necessary variables
utuva_elig <- utuva_elig %>% select(age, SUKUPUOLI, utuva1sbp,
                                    utuva1dbp, `Hypertensive blood pressure reading`,
                                    hypertension_utuva1, antihypertensive_med)



# Convert categorical variables into factor
utuva_elig <- utuva_elig %>% mutate(SUKUPUOLI =
                                    factor(SUKUPUOLI, levels = c(1, 2), labels = c("Men", "Women")))

utuva_elig <- utuva_elig %>% mutate(hypertension_utuva1 =
                                    factor(hypertension_utuva1, levels = c(1, 0), labels = c("Yes", "No")))


utuva_elig <- utuva_elig %>% mutate(antihypertensive_med =
                                    factor(antihypertensive_med, levels = c(1, 0), labels = c("Yes", "No")))

utuva_elig <- utuva_elig %>% mutate(`Hypertensive blood pressure reading` =
                                    factor(`Hypertensive blood pressure reading`,
                                           levels = c(0, 1), labels = c("No", "Yes")))


# Rename with nicer names
utuva_elig <- utuva_elig %>% rename(`Age (years)` = age,
                                  `Sex` = SUKUPUOLI,
                                  `Systolic blood pressure (mmHg)` = utuva1sbp,
                                  `Diastolic blood pressure (mmHg)` = utuva1dbp,
                                  `Prevalent hypertension` = hypertension_utuva1,
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
  
  as_sup("b"), "Hypertension was defined as either having uncontrolled BP, self-reported hypertension, or confirmed use of antihypertensive medication without indications other than hypertension."))

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


