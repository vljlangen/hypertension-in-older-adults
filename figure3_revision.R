library(pacman)
p_load(dplyr, tidyr, ggplot2, ggthemes, ggsignif, cowplot, here, showtext, magick)
 

# Tell the "here" package where you are right now - i.e. tell the location of the file you are running right now
here::i_am("article1/README.R")

# After that, you can check where the project root is by running "here()"
here()

#  Load the datasets using {here} package
article1tuva_file <- here("data", "article1tuva_bp_meds_final.rds")
article1utuva_file <- here("data", "article1utuva_bp_meds_final.rds")

tuva <- readRDS(article1tuva_file)
utuva <- readRDS(article1utuva_file)

# Clean up variables
rm(article1tuva_file, article1utuva_file)
 


############################################################################
############################################################################
###                                                                      ###
###                EXCLUDE PARTICIPANTS WITH MISSING DATA                ###
###                                                                      ###
############################################################################
############################################################################




# TUVA1
tuva <- tuva %>% drop_na(tuva1sbp, tuva1dbp)

# Check how many remained
tuva %>% nrow()


# UTUVA1

utuva <- utuva %>% drop_na(utuva1sbp, utuva1dbp)

# Check how many remained
utuva %>% nrow()

 


############################################################################
############################################################################
###                                                                      ###
###                         MUTATE NEW VARIABLES                         ###
###                                                                      ###
############################################################################
############################################################################




# Mutate a new variable for pathological RR values
tuva <- tuva %>% 
  mutate(hypertensive_bp = ifelse(
    tuva1sbp >= 140 | tuva1dbp >= 90, 1, 0))


# Mutate a new variable for pathological RR values
utuva <- utuva %>% 
  mutate(hypertensive_bp = ifelse(
    utuva1sbp >= 140 | utuva1dbp >= 90, 1, 0))





###########################################################################
###########################################################################
###                                                                     ###
###                               PLOT                                  ###
###                                                                     ###
###########################################################################
###########################################################################



# Select only binary drug data
tuva_meds <- tuva %>% select(ID1991,
                             hypertensive_bp,
                             hypertension_tuva1,
                             antihypertensive_med,
                             ACEi,
                             ARB,
                             BB,
                             CCB,
                             diuretic,
                             other_bp_medicine)

tuva_meds <- tuva_meds %>% rename(hypertension = hypertension_tuva1)

# Name the cohort
tuva_meds$cohort <- "TUVA"

# Reorder
tuva_meds <- tuva_meds %>%  select(ID1991, cohort, everything())



# Select only binary drug data
utuva_meds <- utuva %>% select(ID1991,
                               hypertensive_bp,
                               hypertension_utuva1,
                               antihypertensive_med,
                               ACEi,
                               ARB,
                               BB,
                               CCB,
                               diuretic,
                               other_bp_medicine)

utuva_meds <- utuva_meds %>% rename(hypertension = hypertension_utuva1)

# Name the cohort
utuva_meds$cohort <- "UTUVA"

# Reorder
utuva_meds <- utuva_meds %>%  select(ID1991, cohort, everything())


# # Turn all NAs into zeros in UTUVA (already OK in TUVA)
cols_to_zeros <- c("ACEi", "ARB", "BB", "CCB", "diuretic", "other_bp_medicine")

utuva_meds[cols_to_zeros][is.na(utuva_meds[cols_to_zeros])] <- 0


# Combine TUVA and UTUVA
tuva_utuva_meds <- rbind(tuva_meds, utuva_meds)


##################################################################
##                    Convert to long format                    ##
##################################################################



# Run chisq.tests to compare the categorical variables has_ace:has_arb between cohorts "A" and "B"

# First, turn the data to long format
tuva_utuva_meds_long <- tuva_utuva_meds %>%
  pivot_longer(cols = 3:11, names_to = "binary_var", values_to = "value")



# Run chi-square tests

test_results <- tuva_utuva_meds_long %>%
  group_by(binary_var) %>%
  summarize(p_value = chisq.test(table(cohort, value))$p.value)

# Display the table
test_results 


# Embellish
test_results <- test_results %>%
  mutate(p_value = case_when(
    p_value < 0.001 ~ paste0("P<0.001"),
    p_value < 0.01 ~ paste0("P<0.01"),
    TRUE ~ paste0("P=", format(round(as.numeric(p_value), 2), nsmall = 2))
  ))


# View the updated tibble
test_results

# Reorder to the following order:

# Hypertensive\nBP reading
# Hypertension
# Any\nantihypertensive\nmedication
# ACEi
# ARB
# BB
# CCB
# Diuretic
# Other\nantihypertensive\nmedication

hypertensive_bp <- test_results %>% slice(8)
hypertension <- test_results %>% slice(7)
antihypertensive_med <- test_results %>% slice(5)
test_results <- test_results %>% slice(1:4,6,9)

test_results <- rbind(hypertensive_bp,
                      hypertension,
                      antihypertensive_med,
                      test_results)

# Confirm the order
test_results

 
# For plotting the bar chart, create a probability table
drug_table <- tuva_utuva_meds %>%
  group_by(cohort) %>%
  summarise(hypertensive_bp_pct = mean(hypertensive_bp, na.rm = TRUE) * 100,
            hypertension_pct = mean(hypertension, na.rm = TRUE) * 100,
            antihypertensive_med_pct = mean(antihypertensive_med, na.rm = TRUE) * 100,
            ACEi_pct = mean(ACEi, na.rm = TRUE) * 100,
            ARB_pct = mean(ARB, na.rm = TRUE) * 100,
            BB_pct = mean(BB, na.rm = TRUE) * 100,
            CCB_pct = mean(CCB, na.rm = TRUE) * 100,
            Diuretic_pct = mean(diuretic, na.rm = TRUE) * 100,
            Other_pct = mean(other_bp_medicine, na.rm = TRUE) * 100)


# Display the table
drug_table 

# Rename variables for plotting
drug_table <- drug_table %>% rename(`Uncontrolled\nBP` = hypertensive_bp_pct,
                                    `Hyper-\ntension` = hypertension_pct,
                                    `Any BP-\nlowering\ndrug` = antihypertensive_med_pct,
                                    ACEi = ACEi_pct,
                                    ARB = ARB_pct,
                                    BB = BB_pct,
                                    CCB = CCB_pct,
                                    Diuretic = Diuretic_pct,
                                    `Other BP-\nlowering\ndrug` = Other_pct)

# Display the table
drug_table 




# For plotting the bar chart, turn data into long format

# Convert data to long format
drug_long <- pivot_longer(drug_table,
                          cols = 2:10,
                          names_to = "binary_var",
                          values_to = "Percentage")


drug_long <- drug_long %>% rename(Cohort = cohort)


# Display
drug_long


# To get the tick labels, use the following logic
#drug_long$binary_var %>% unique() %>% dput()

# Order positions of the bars in the bar chart
c("Hypertensive\nBP reading", "Hypertension",
  "Any anti-\nhypertensive",
  "ACEi", "ARB", "BB", "CCB",
  "Diuretic", "Other anti-\nhypertensive"
)


drug_long$binary_var <- factor(drug_long$binary_var, levels = c("Uncontrolled\nBP", "Hyper-\ntension", "Any BP-\nlowering\ndrug", 
                                                                "ACEi", "ARB", "BB", "CCB", "Diuretic", "Other BP-\nlowering\ndrug"
))


# Assuming your data frame is named df
drug_long <- drug_long %>% 
  mutate(Cohort = if_else(Cohort == "TUVA", "1920-born TUVA cohort", Cohort))

# Assuming your data frame is named df
drug_long <- drug_long %>% 
  mutate(Cohort = if_else(Cohort == "UTUVA", "1940-born UTUVA cohort", Cohort))


# Define your custom colors
#custom_colors <- c("TUVA" = "#46BAC5", "UTUVA" = "#FAAA48")
#custom_colors <- c("TUVA" = "#44B2BC", "UTUVA" = "#EEA244")

custom_colors <- c("1920-born TUVA cohort" = "#3EA4AD", "1940-born UTUVA cohort" = "#EEA244")


# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")

# Invoke showtext
showtext_auto()




# Create the plot

fig3 <-   ggplot(drug_long, aes(fill = Cohort, x = binary_var, y = Percentage)) +
 
  theme_classic(base_size = 15, base_family = "rosario") +  
  
  theme(axis.line = element_line(linewidth = 0.3)) +
  
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +

  scale_fill_manual(values = custom_colors) +  # Manually set fill colors
  
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 100, 25),
                     limits=c(0, 100),
                     labels = paste0(seq(0, 100, by = 25), "%")) +
  
  labs(x = "Hypertension prevalence and use of antihypertensive medication") +

    geom_signif(
    textsize=5,
    y_position = c(88, 95.5, 60, 21, 23, 33.5, 21, 21, 7), xmin = 1:9-0.2, xmax = 1:9+0.2,
    annotation = test_results$p_value, tip_length = 0.01,
    family = "rosario") +

  
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 23)) +

  theme(
    legend.position = c(.8, .96),
    legend.text = element_text(size = 16),
    legend.title=element_blank()
    ) +
  
  
  theme(axis.title.x = element_text(face = "bold",
                                    margin = margin(t = 24, unit = "pt")),  # Adjust margin for x-axis title
        axis.title.y = element_text(face = "bold",
                                    margin = margin(r = 15, unit = "pt"))) +  # Adjust margin for y-axis title

  theme(plot.margin = margin(20, 20, 20, 20))


  
fig3

#  Save the PNG and PDF files using {here} package
png_file <- here("article1", "Fig3_dpi600.png")
pdf_file <- here("article1", "Fig3_dpi600.pdf")
  


# Save as PDF with dpi specified
ggsave(pdf_file, fig3, dpi = 600, width = 14, height = 6)


# Load the PDF back with {here} and {magick} packages
pdf_image <- magick::image_read_pdf(pdf_file, density = 600)

# Save it as PNG
image_write(pdf_image,
            path = png_file,
            format = "png",
            density = 600)






