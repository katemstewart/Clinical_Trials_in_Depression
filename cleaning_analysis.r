# ------------------------------------------------------------
# Script Name:  Processing and descriptive analysis steps for Clinical Trials 
#               In Depression collection, available at: 
# Github: 
# Author: Kate Stewart
# Date: October 2025
# Description:  Processes clinical trial data to identify trials involving drugs
#               relevant to treatment of depression. Categorises sponsors of 
#               clinical trials into type and summarizes sponsor distribution 
#               across datasets. Performs descriptive statistics on trial data
#               including source, intervention types, trial status, trial phase, 
#               drug and drug class, and available demographics data in trials. 
#               Visualizes the distribution of vector scores across query groups
#               drug-related, gene-related, and other) using violin plots.
#               Calculates Precision at 20 score for selected vector queries 
#               (biospecimens, genetics, proteomics, SSRI, antidepressant 
#               response) using manual relevance assessments of top and bottom
#               20 trials. Includes visualisations. 
# ================================================================

# === Load Required Libraries ===
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(flextable)
library(forcats)
library(patchwork)
library(openxlsx)

# === Read in Datasets ===
trials_summary <- read_excel("clinical_trials_raw_data.xlsx",
                             sheet = "trials_summary")
detailed <- read_excel("interim_withconditions.xlsx")
interventions <- read_excel("clinical_trials_raw_data.xlsx",
                            sheet = "interventions_detailed")
interventions_summary <- read_excel("clinical_trials_raw_data.xlsx",
                                    sheet = "interventions_summary")
pico_view <- read_excel("clinical_trials_raw_data.xlsx",
                        sheet = "PICO")
term_search <- read_excel("clinical_trials_raw_data.xlsx",
                          sheet = "term_search")
demographics <- read_excel("clinical_trials_raw_data.xlsx",
                           sheet = "demographics_summary")
demographics_detailed <- read_excel("clinical_trials_raw_data.xlsx",
                                    sheet = "demographics_detailed")
publications <- read_excel("clinical_trials_raw_data.xlsx",
                           sheet = "publications")

# === Filter Trials with Drug Interventions ===
# Select trials that include a drug intervention
drug_int_dataset <- detailed |>  
  filter(`Includes drug intervention` == "Yes")
# Select interventions classified as 'Drug'
drug_interventions <- interventions |>  
  filter(intervention_type == "Drug")

# === Clean and Standardise Drug Names ===
# Convert drug names to lowercase, remove special characters, and trim whitespace
drug_interventions$drug_name_clean <- tolower(drug_interventions$drug)
drug_interventions$drug_name_clean <- gsub("[^a-zA-Z\\s]", "", drug_interventions$drug_name_clean)
drug_interventions$drug_name_clean <- trimws(drug_interventions$drug_name_clean)

# If drug name is missing, use intervention name instead
drug_interventions <- drug_interventions |>
  mutate(drug_name_clean = ifelse(is.na(drug_name_clean), intervention, drug_name_clean))

# Reapply cleaning to updated drug name
drug_interventions$drug_name_clean <- tolower(drug_interventions$drug_name_clean)
drug_interventions$drug_name_clean <- gsub("[^a-zA-Z\\s]", "", drug_interventions$drug_name_clean)
drug_interventions$drug_name_clean <- trimws(drug_interventions$drug_name_clean)

# Export cleaned drug interventions for manual review
write.csv(drug_interventions, "AD_drug_interventions.csv", row.names = FALSE)

# === Join with Antidepressant Lookup Table ===
AD_all_lookup <- read_excel("AD_All_Lookup.xlsx")
AD_all_lookup <- AD_all_lookup |>
  rename(drug_name_generic = Drug_name_clean)

# Join and standardize drug names using lookup
cleaned_drug_interventions <- drug_interventions |> 
  left_join(AD_all_lookup, by = c("drug_name_clean" = "Drug_name"), relationship = "many-to-many") |> 
  mutate(drug_name_clean = ifelse(is.na(drug_name_generic), drug_name_clean, drug_name_generic)) |> 
  select(-drug_name_clean)

# Filter trials that include antidepressants
AD_interventions_only <- cleaned_drug_interventions |> 
  filter(drug_name_generic != "NA") |> 
  mutate(drug_name_generic = gsub("_", " ", drug_name_generic)) |> 
  mutate(Drug_class = gsub("_", " ", Drug_class))

# === Join with Other Drug Lookup Table ===
Other_Drug_Lookup <- read_excel("Other_Drug_Lookup.xlsx")
Other_Drug_Lookup <- Other_Drug_Lookup |> 
  rename(drug_name_generic = Drug_name_clean)

# Join and standardize other drug names
cleaned_other_drug_interventions <- drug_interventions |> 
  left_join(Other_Drug_Lookup, by = c("drug_name_clean" = "Drug_name"), relationship = "many-to-many") |> 
  mutate(drug_name_clean = ifelse(is.na(drug_name_generic), drug_name_clean, drug_name_generic)) |> 
  select(-drug_name_clean)

# Filter trials that include NMDA antagonists, cannabinoids, or psychedelics
Other_drug_interventions_only <- cleaned_other_drug_interventions |> 
  filter(drug_name_generic != "NA") |> 
  mutate(drug_name_generic = gsub("_", " ", drug_name_generic)) |> 
  mutate(Drug_class = gsub("_", " ", Drug_class))

# === Combine All Drug Classes for Depression Treatment ===
All_Drug_Lookup <- bind_rows(AD_all_lookup, Other_Drug_Lookup)

# Join all drug interventions with combined lookup
all_drug_depression <- drug_interventions |> 
  left_join(All_Drug_Lookup, by = c("drug_name_clean" = "Drug_name"), relationship = "many-to-many") |> 
  mutate(drug_name_clean = ifelse(is.na(drug_name_generic), drug_name_clean, drug_name_generic)) |> 
  select(-drug_name_clean) |> 
  filter(drug_name_generic != "NA") |> 
  mutate(drug_name_generic = gsub("_", " ", drug_name_generic)) |> 
  mutate(Drug_class = gsub("_", " ", Drug_class))

# === Summary of trials by category ===
# Count unique trials for each drug category
unique_trials_AD_only <- AD_interventions_only |> 
  distinct(trial_id) |> 
  nrow()
unique_trials_other_drugs <- Other_drug_interventions_only |> 
  distinct(trial_id) |> 
  nrow()
unique_trials_all_drug <- all_drug_depression |> 
  distinct(trial_id) |> 
  nrow()

# Print summary
cat("Unique trials with antidepressants:", unique_trials_AD_only, "\n")
cat("Unique trials with other drugs:", unique_trials_other_drugs, "\n")
cat("Total unique trials with any depression-related drug:", unique_trials_all_drug, "\n")


# === Extract Sponsor Information ===
# Create a sponsor list with trial ID, title, and sponsor name
sponsor_list <- detailed |> 
  select(`Trial ID`, Title, Sponsor)

# === Categorize Sponsors by Type ===
# Use pattern matching to assign sponsor_type based on known keywords
sponsor_list <- sponsor_list |> 
  mutate(sponsor_type = case_when(
    is.na(Sponsor) ~ NA_character_,
    grepl("talkspace|Oregon Research Behavioral Intervention Strategies, Inc|digital for mental health|meru health|hucircadian|silver cloud|ybrain|cota inc|pacifica|adaptive health|appliedvr|oxford VR|benten tech|rethink wellbeing|akili interactive|Ask Rose|7 Generation Games|Academic Web Pages|actualize therapy|appa health|big health|blueskeye|clinical tools, inc|eleos health|ginger.io|happify|healthrhythms|hedonia|hollo |limbix|mindlight|mindapps|mynd|mystrength|posit science|prevail health|pro-change|proofpilot|taliaz|tangent data|tao connect|Oregon Center for Applied Science, Inc.|woebot", Sponsor, ignore.case = TRUE) ~ "Digital health",
    grepl("LLC|company|menarini|ZYMOGENETICS, INC.|Labopharm Inc|Upsher-Smith Laboratories, Inc.|alkermes|NEMA Research, Inc.|Clinirx Tangent Research|CPS Research|ancora|AnthroTronix|assurex|bausch|biolite|braincells|ceregene|cervel|cyberonics|Daiichi Sankyo|dermira|ehave inc|Electromedical Products|Emalex|Chenland Nutritionals Inc.|Capital Clinical Research Associates, LLC|Euthymics|Exelixis|Gate Neurosciences|Nu-Life Solutions|genvec|gilead|Human Genome Sciences, Inc.|Intra-Cellular Therapies, Inc.|Kalaco Scientific, Inc.|Krystal Biotech, Inc.|Regado Biosciences, Inc|Tal Medical, Inc.|Targacept Inc.|XTL Development Inc.|Sunovion|MSI Methylation Sciences, Inc.|mylan|Neuralstem Inc.|Neurocentria, Inc.|NEUROCHEM INC.|NeuroQore Inc.|NeuroRx, Inc.|Nuron Biotech, Inc.|OptiNose US|alector|SyneuRx|Suisselle|Actinogen Medical|Soterix Medical|Servier Research and Development Limited; Institut de Recherches Internationales Servier|Forest Research Institute Inc., a wholly owned subsidiary of Forest Laboratories, LLC|ELI LILLY & COMPANY, LILLY CORPORATE CENTER|Chelsea Therapeutics Inc.|Takeda Global Research & Development Centre|shire|noscira|organon|serodus|som biotech|suiselle|soundmind|shionogi|schering|iomedico|praxis precision|prima cbd|radicle|neurophyxia|pantarhei|perception neuro|philips|pierre|neurosearch|nubiyota|Nutrición Medica|nutrilinea|magstim|mapreg|s.p.a|meditop|Zylorion|umecrine|Neurocrine Biosciences|bial|rafa|Omni C&S|neuraxpharm|wave neuro|viome|livanova|hoffman|Catarinense|cosmos|krka|khondrion|italfarmaco|ipsen|Healthcare Innovation Technology Lab|GeNeuro|green medicine|Genzyme|gaia|fisher|flow neuro|bioib|avillion|compass pathways|cortendo|camurus|cassella|catalysis|benevolentai|colibris|a. carlsson|biotics|ab science|argenx|acraf|scientiam|adwin|aifred|bioprojet|pharma|astra|wyeth|glaxo|pfizer|therapeutics|forest|sanofi|novartis|cephalon|lundbeck|bayer|lilly|procter|neurocode|janssen|takeda|ltd|corporation|gmbh|medtronic|biogen|squibb|sandoz|neuronetic|rand|northstar|abbvie|limited|respirerx|merck|abbott|allergan|altheadx|alto neuro|amgen|corp.|brainsway|boehringer|diagnostics|farmaceut|servier|galderma|plc|i4health|minerva|nurturely|probiotical|reunion|roxane|theranexus|UCB|trudiagnostic", Sponsor, ignore.case = TRUE) ~ "Healthcare industry",
    grepl("psychiatric research unit|american cancer society|possible|premier health|Royal Holloway University|mind mental health|propersona|mathematica|slso|national society|Agence nationale de recherches|National Heart, Lung, and Blood|Health Resources in Action,|Agence nationale de recherches sur le sida et les hÃ©patites virales ANRS|NBML|Sunstone Medical|National Jewish Health|Valley Medical Research|National Bureau of Economic Research, Inc.|Hadassah Medical Organization|college|univer|Serveis de Salut|sanatorium|alexander mcintyre|jail education|sonder behav|lakeland|stichting apotheek|Papastergiou|stichting toegepast|duran|sidra med|tuscaloosa|Società Italian|seton healthcare family|sangath|satellite health|providence|uniwers|inrca|northwell|possible |prisma health|Psychiatrische Dienste|psychiatry roskilde|Pirkanmaan|parnassia|Maryland Oncology|modum|noggo|max planck|mdrc|Medizinische Einrichtungen|Národní ústav duševního zdraví|ggz|Motricité|oberwaid|rambam|enseignement en douleur|revival|orygen|umcg|ucd|taub|nepal|umc|yolanda|koushede|vail health|unicancer|wellspan|Boltzmann|LUMC|Östergötland|Velling Magnussen|irmandade|Pesquisa em Lazer|leuven|Kronikgune|traumacentrum|irccs|sevilla|je bouge|Hotel-Dieu|intersector|inserm|clinic|Albacete|cctu|helse|Hersenstichting|Essentia Health|fonds|hungry|fraser|fuerza|field trip|European|kliniken|Service de Santé|eksote|christiana|chu |Ciusss|clarigent|Club rTMS|cerepp|changping|hospi|billings clinic|bayside|bdh-klinik|health of the palm beaches|anuradha|arcagy|institu|azienda|kaiser|umc utrecht|advent|advocate|affective neuro|uconn health|uhtoulouse|sykehuset|servicio|stockholm|trials unit, ucl|parc de salut|seniorlife|erasmus|ospedaliero|clergy|chu de|amiens|chru|chu of|klinikum|nyu langone|onkologie|department|school|agency|centre|center|zonmw|foundation|alliance|va office|veteran|zentrum|publique|council|nhs|the alfred|authority|system|nacional|mayo clinic|academy|fundaci|canadian|mental health cent|consortium|region|network|maine|psiquiatria|poverty|welfare|ministry|toronto|amsterdam|irccs|sykhuset|district|danish|ospedaliera|hamilton|basque|vinatier|charit|marseille|fondazione|trust|alberta|sygehus|associa|centro|consorci|consorzi|dipartimento|fondation|pital|collaboration|istituto|centar|north holland|melbourne|ospedale|st pat|st john|ziekenhuis|ÜNİVERSİTESİ", Sponsor, ignore.case = TRUE) ~ "Academic & public sector",
    TRUE ~ "Unknown"
  ))

# === Count Sponsors Across All Trials ===
sponsor_count_tbl <- sponsor_list |>
  count(sponsor_type) |>
  arrange(desc(n))
print(sponsor_count_tbl)

# === Add Sponsor Categories to Drug Intervention Dataset ===
# Create a distinct sponsor-type mapping
sponsor_cat_df <- sponsor_list |> 
  select(Sponsor, sponsor_type) |> 
  distinct(Sponsor, sponsor_type)

# Join sponsor type to drug intervention dataset
drug_int_dataset_sponsors <- left_join(drug_int_dataset, sponsor_cat_df, by = "Sponsor") 

# Count sponsor types in drug intervention trials
drug_int_sponsor_count <- drug_int_dataset_sponsors |> 
  count(sponsor_type) |> 
  arrange(desc(n))
print(drug_int_sponsor_count)

# === Add Sponsor Categories to Depression Drug Dataset ===
# Create mapping of trial ID to sponsor type
sponsor_cat_id <- sponsor_list |> 
  select(`Trial ID`, Sponsor, sponsor_type) |> 
  distinct(`Trial ID`, Sponsor, sponsor_type) |> 
  rename("trial_id" = `Trial ID`)

# Join sponsor type to depression drug dataset
all_drug_depress_sponsor <- left_join(all_drug_depression, sponsor_cat_id, by = "trial_id")

# Get unique sponsor-trial combinations
all_drug_depress_sponsor_unique <- all_drug_depress_sponsor |> 
  distinct(trial_id, Sponsor, sponsor_type)

# Count sponsor types in depression drug trials
depress_drug_sponsor_count <- all_drug_depress_sponsor_unique |> 
  count(sponsor_type) |> 
  arrange(desc(n))
print(depress_drug_sponsor_count) 

# === Combine Sponsor Counts Across Datasets ===
sponsor_category_table <- sponsor_count_tbl |> 
  left_join(drug_int_sponsor_count, by = "sponsor_type") |> 
  left_join(depress_drug_sponsor_count, by = "sponsor_type") |> 
  rename("All trials" = n.x,
         "Inc. drug intervention" = n.y, 
         "Inc. drugs to treat depression" = n)
print(sponsor_category_table)

# Export sponsor category summary table to Word document
sponsor_category_table_doc <- flextable(sponsor_category_table) |> 
  save_as_docx(path = "sponsor_category_table.docx")

# === Identify Top Sponsors by Frequency ===
# Top 10 sponsors across all trials
sponsor_freq <- sponsor_cat_id |> 
  distinct(trial_id, Sponsor, sponsor_type) |> 
  count(Sponsor, name = "count") |> 
  filter(!is.na(Sponsor)) |> 
  arrange(desc(count)) |> 
  slice(1:10)
print(sponsor_freq)

# Export top sponsor table to Word document
sponsor_table <- flextable(sponsor_freq) |> 
  save_as_docx(path = "sponsor_table.docx")

# Top 10 sponsors across trials with drug interventions
drug_sponsor_freq <-  drug_int_dataset_sponsors|> 
  distinct(`Trial ID`, Sponsor, sponsor_type) |> 
  count(Sponsor, name = "count") |> 
  filter(!is.na(Sponsor)) |> 
  arrange(desc(count)) |> 
  slice(1:10)
print(drug_sponsor_freq)

# Export table to Word document
drug_sponsor_table <- flextable(drug_sponsor_freq) |> 
  save_as_docx(path = "drug_sponsor_table.docx")

# Top 10 sponsors across trials with drug interventions to treat depression
depress_drug_sponsor_freq <- all_drug_depress_sponsor_unique |> 
  count(Sponsor, name = "count") |> 
  filter(!is.na(Sponsor)) |> 
  arrange(desc(count)) |> 
  slice(1:10)
print(depress_drug_sponsor_freq)

# Export table to Word document
depress_drug_sponsor_table <- flextable(depress_drug_sponsor_freq) |> 
  save_as_docx(path = "depress_drug_sponsor_table.docx")

# === Source Analysis ===
# Count number of trials by source (e.g., registry)
source_count <- detailed |> 
  count(Source)
print(source_count)

# Calculate percentage of trials by source
percent_source <- detailed |> 
  group_by(Source) |> 
  summarise(Percentage = n() / nrow(detailed)*100)
print(percent_source)


# === Intervention Type Analysis ===
# Count number of interventions by type
interventions_count <- interventions |> 
  count(intervention_type)
print(interventions_count)

# Plot intervention type
intervention_type <- ggplot(interventions, aes(fct_infreq(intervention_type), fill = intervention_type)) +
  geom_bar() +
  labs(x = NULL) +
  theme_minimal() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "none")
intervention_type
ggsave("intervention_type.png", intervention_type, width = 8, height = 8)
ggsave("intervention_type.eps", intervention_type, width = 8, height = 8)


# === Drug Intervention Analysis ===
# Count trials including drug interventions
includes_drug_int_count <- detailed |> 
  count(`Includes drug intervention`)
print(includes_drug_int_count)

# Count drug intervention trials by source
drug_int_source <- drug_int_dataset |> 
  count(Source)
print(drug_int_source)

# Plot number of trials including drug interventions
includes_drug_int <- ggplot(detailed, aes(x = `Includes drug intervention`)) +
  geom_bar() +
  labs(title = "Clinical trials which include a drug intervention") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("include_drug_int.png", includes_drug_int, width = 5, height = 5)
ggsave("include_drug_int.eps", includes_drug_int, width = 5, height = 5)


# === Drug Trials by Source ===
# Prepare source-trial mapping
source <- detailed |> 
  select(`Trial ID`, Source) |> 
  rename(trial_id  = `Trial ID`)

# Count antidepressant trials by sourc
AD_interventions_source <- source |> 
  left_join(AD_interventions_only, by = "trial_id", relationship = "many-to-many") |> 
  filter(intervention != "NA") |> 
  distinct(trial_id, .keep_all = TRUE) |> 
  count(Source)
print(AD_interventions_source)

# Count NMDA/cannabinoid/psychedelic trials by source
other_drug_source <- source |> 
  left_join(Other_drug_interventions_only, by = "trial_id", relationship = "many-to-many") |> 
  filter(intervention != "NA") |> 
  distinct(trial_id, .keep_all = TRUE) |> 
  count(Source)
print(other_drug_source)

# Count all depression drug trials by source
all_drug_depression_source <- source |> 
  left_join(all_drug_depression, by = "trial_id", relationship = "many-to-many") |> 
  filter(intervention != "NA") |> 
  distinct(trial_id, .keep_all = TRUE) |> 
  count(Source)
print(all_drug_depression_source)

# === Unique Trials by Intervention Type ===
# Count unique trials for each intervention type
unique_trials_behavioural <- interventions |> 
  filter(intervention_type == "Behavioral") |> 
  distinct(trial_id) |> 
  count()
unique_trials_procedure <- interventions |> 
  filter(intervention_type == "Procedure") |> 
  distinct(trial_id) |> 
  count()
unique_trials_device <- interventions |> 
  filter(intervention_type == "Device") |> 
  distinct(trial_id) |> 
  count()
unique_trials_other_int <- interventions |> 
  filter(intervention_type == "Other") |> 
  distinct(trial_id) |> 
  count()
unique_trials_dietary <- interventions |> 
  filter(intervention_type == "Dietary Supplement") |> 
  distinct(trial_id) |> 
  count()
unique_trials_drug_int <- interventions |> 
  filter(intervention_type == "Drug") |> 
  distinct(trial_id) |> 
  count()
unique_trials_radiation <- interventions |> 
  filter(intervention_type == "Radiation") |> 
  distinct(trial_id) |> 
  count()
unique_trials_combo <- interventions |> 
  filter(intervention_type == "Combination Product") |> 
  distinct(trial_id) |> 
  count()
unique_trials_diagnostic <- interventions |> 
  filter(intervention_type == "Diagnostic Test") |> 
  distinct(trial_id) |> 
  count()
unique_trials_biological <- interventions |> 
  filter(intervention_type == "Biological") |> 
  distinct(trial_id) |> 
  count()
unique_trials_genetic <- interventions |> 
  filter(intervention_type == "Genetic") |> 
  distinct(trial_id) |> 
  count()

# Combine into summary table
unique_trials_intervention_table <- bind_rows(
  tibble(Name = "Behavioural", Count = unique_trials_behavioural$n),
  tibble(Name = "Biological", Count = unique_trials_biological$n),
  tibble(Name = "Combination", Count = unique_trials_combo$n),
  tibble(Name = "Device", Count = unique_trials_device$n),
  tibble(Name = "Diagnostic", Count = unique_trials_diagnostic$n),
  tibble(Name = "Dietary", Count = unique_trials_dietary$n),
  tibble(Name = "Drug", Count = unique_trials_drug_int$n),
  tibble(Name = "Genetic", Count = unique_trials_genetic$n),
  tibble(Name = "Other", Count = unique_trials_other_int$n),
  tibble(Name = "Procedure", Count = unique_trials_procedure$n),
  tibble(Name = "Radiation", Count = unique_trials_radiation$n)
)
# Export intervention summary table
unique_intervention_table <- flextable(unique_trials_intervention_table) |> 
  save_as_docx(path = "unique_intervention_table.docx")

# === Trial Status Analysis: All Trials ===
Status_count_all <- detailed |> 
  count(Status)
print(Status_count_all)

percent_status_all <- detailed |> 
  group_by(Status) |> 
  summarise(Percentage = n() / nrow(detailed)*100)

# Plot trial status
Trial_Status_all <- ggplot(detailed, aes(x = Status)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Trial status - all clinical trials")
Trial_Status_all
ggsave("trial_status_all.png", Trial_Status_all, width = 5, height = 5)

# === Trial Status by Registry Source ===
Status_count_by_register <- detailed |> 
  group_by(Source, Status) |> 
  summarise(num_trials = n(), .groups = "drop") |> 
  pivot_wider(
    names_from = Source,
    values_from = num_trials
  )
print(Status_count_by_register)

# Export status by registry table
status_register_table <- flextable(Status_count_by_register) |> 
  save_as_docx(path = "status_register_table.docx")

# === Trial Status: Drug Intervention Trials ===
Status_count_drug <- drug_int_dataset |> 
  count(Status)
print(Status_count_drug)

percent_status_drug <- drug_int_dataset |> 
  group_by(Status) |> 
  summarise(Percentage = n() / nrow(drug_int_dataset)*100)
print(percent_status_drug)

# Plot trial status for drug intervention trials
Trial_Status_drug_int <- ggplot(drug_int_dataset, aes(x = Status)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Trial status - clinical trials including a drug intervention")
Trial_Status_drug_int
ggsave("trial_status_drug_int.png", Trial_Status_drug_int, width = 5, height = 5)

# === Trial Status: Depression Drug Trials ===
# Add phase and status to depression drug trial
phase_status <- detailed |>
  select(`Trial ID`, Phase, Status) |> 
  rename(trial_id = `Trial ID`)

all_drug_depression_phase_status <- phase_status |> 
  left_join(all_drug_depression, by = "trial_id", relationship = "many-to-many") |> 
  filter(intervention != "NA") |> 
  distinct(trial_id, .keep_all = TRUE)

# Count and percentage of trial status
Status_count_depress_drug <- all_drug_depression_phase_status |> 
  count(Status)
print(Status_count_depress_drug)

percent_status_depress_drug <- all_drug_depression_phase_status |> 
  group_by(Status) |> 
  summarise(Percentage = n() / nrow(all_drug_depression_phase_status)*100)
print(percent_status_depress_drug)

# === Trial Phase Analysis: All Trials ===
Phase_count_all <- detailed |> 
  count(Phase)
print(Phase_count_all)

percent_phase_all <- detailed |> 
  group_by(Phase) |> 
  summarise(Percentage = n() / nrow(detailed)*100)
print(percent_phase_all)

Trial_Phase_all <- ggplot(detailed, aes(x = Phase)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Trial phase - all clinical trials")
ggsave("trial_phase_all.png", Trial_Phase_all,  width = 5, height = 5)

# === Trial Phase Analysis: Drug Intervention Trials ===
Phase_count_drug_int <- drug_int_dataset |> 
  count(Phase)
print(Phase_count_drug_int)

percent_phase_drug_int <- drug_int_dataset |> 
  group_by(Phase) |> 
  summarise(Percentage = n() / nrow(drug_int_dataset)*100)
print(percent_phase_drug_int)

Trial_Phase_drug_int <- ggplot(drug_int_dataset, aes(x = Phase)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Trial phase - clinical trials including a drug intervention")
ggsave("trial_phase_drug_int.png", Trial_Phase_drug_int,  width = 5, height = 5)

# === Trial Phase Analysis: Depression Drug Trials ===
Phase_count_depress_drug <- all_drug_depression_phase_status |> 
  count(Phase)
print(Phase_count_depress_drug)

percent_phase_depress_drug <- all_drug_depression_phase_status |> 
  group_by(Phase) |> 
  summarise(Percentage = n() / nrow(all_drug_depression_phase_status)*100)
print(percent_phase_depress_drug)

# === Antidepressant Drug Class Analysis ===
AD_class_count_tbl <- AD_interventions_only |> 
  count(Drug_class) 
print(AD_class_count_tbl)

AD_drug_class <- ggplot(AD_interventions_only, aes(fct_infreq(Drug_class), fill = Drug_class)) +
  geom_bar() +
  labs(title = NULL, x = NULL, y = "instances of drug class in trials") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none", text = element_text(size=16)) +
  scale_fill_manual(values = c(
    "MAOI" = "#E41A1C",
    "NDRI" = "#377EB8",
    "NRI" = "#4DAF4A",
    "other" = "#984EA3", 
    "SARI" = "#FF7F00",
    "SNRI" = "#FFFF33",
    "SSRI" = "#9590FF",
    "TCA" = "#A65628",
    "tetracyclic" = "#F781BF",
    "unspecified" = "#999999",
    "NMDA_receptor_antagonist" = "#66C2A5",
    "serotenergic_psychedelic" = "#FC8D62",
    "cannabinoid" = "#8DA0CB"))
ggsave("AD_drug_class.png", AD_drug_class, width = 8, height = 5)
ggsave("AD_drug_class.eps", AD_drug_class, width = 8, height = 5)

# === Other Drug Class Analysis ===
other_drug_class_count_tbl <- Other_drug_interventions_only |> 
  count(Drug_class) 
print(other_drug_class_count_tbl)

Other_drug_class <- ggplot(Other_drug_interventions_only, aes(fct_infreq(Drug_class), fill = Drug_class)) +
  geom_bar() +
  labs(title = NULL, x = NULL, y = "instances of drug class in trial") +
  theme_minimal() +
  theme(text = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_manual(values = c(
    "MAOI" = "#E41A1C",
    "NDRI" = "#377EB8",
    "NRI" = "#4DAF4A",
    "other" = "#984EA3", 
    "SARI" = "#FF7F00",
    "SNRI" = "#FFFF33",
    "SSRI" = "#9590FF",
    "TCA" = "#A65628",
    "tetracyclic" = "#F781BF",
    "unspecified" = "#999999",
    "NMDA receptor antagonist" = "#66C2A5",
    "serotenergic psychedelic" = "#FC8D62",
    "cannabinoid" = "#8DA0CB"))
ggsave("other_drug_class_count.png", Other_drug_class, width = 8, height = 8)
ggsave("other_drug_class_count.eps", Other_drug_class, width = 8, height = 8)

# === Antidepressant Drug Frequency Analysis ===
AD_count_tbl <- AD_interventions_only |> 
  count(drug_name_generic) |> 
  arrange(desc(n))
print(AD_count_tbl)

AD_count <- ggplot(AD_interventions_only, aes(fct_infreq(drug_name_generic), fill = Drug_class, color = Drug_class)) +
  geom_bar() +
  labs(title = "Antidepressants included in clinical trials", x = NULL, y = "instances of drug included in trials") +
  theme_minimal() +
  theme(text = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("AD_count.png", AD_count, width = 15, height = 10)
ggsave("AD_count.eps", AD_count, width = 15, height = 10)

# === Antidepressants with ≥25 Instances ===
AD_count_min25 <- AD_count_tbl |> 
  filter(n >=25)

AD_interventions_min25 <- AD_interventions_only |> 
  group_by(drug_name_generic) |> 
  filter(n() >= 25) |> 
  ungroup()

AD_plot_min25 <- ggplot(AD_interventions_min25, aes(fct_infreq(drug_name_generic), fill = Drug_class)) +
  geom_bar() +
  labs(title = NULL, x = NULL, y = "instances of drug in trials", fill = "Drug class") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_fill_manual(values = c(
    "MAOI" = "#E41A1C",
    "NDRI" = "#377EB8",
    "NRI" = "#4DAF4A",
    "other" = "#984EA3", 
    "SARI" = "#FF7F00",
    "SNRI" = "#FFFF33",
    "SSRI" = "#9590FF",
    "TCA" = "#A65628",
    "tetracyclic" = "#F781BF",
    "unspecified" = "#999999",
    "NMDA receptor antagonist" = "#66C2A5",
    "serotenergic psychedelic" = "#FC8D62",
    "cannabinoid" = "#8DA0CB"
  ))
ggsave("AD_plot_min25.png", AD_plot_min25, width = 15, height = 10)
ggsave("AD_plot_min25.eps", AD_plot_min25, width = 15, height = 10)

# === Combine AD Class and AD ≥25 Plots ===
AD_two_panel <- AD_drug_class /
  AD_plot_min25
ggsave("AD_two_panel.png", AD_two_panel, width = 8, height = 8)
ggsave("AD_two_panel.eps", AD_two_panel, width = 8, height = 8)

# === Other Drug Frequency Analysis ===
Other_count_tbl <- Other_drug_interventions_only |> 
  count(drug_name_generic) |> 
  arrange(desc(n))
print(Other_count_tbl)

Other_drug_count <- ggplot(Other_drug_interventions_only, aes(fct_infreq(drug_name_generic), fill = Drug_class)) +
  geom_bar() +
  labs(
    title = NULL,
    x = NULL,
    y = "instances of drug in trials",
    fill = "Drug class"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(
    values = c(
      "MAOI" = "#E41A1C",
      "NDRI" = "#377EB8",
      "NRI" = "#4DAF4A",
      "other" = "#984EA3", 
      "SARI" = "#FF7F00",
      "SNRI" = "#FFFF33",
      "SSRI" = "#9590FF",
      "TCA" = "#A65628",
      "tetracyclic" = "#F781BF",
      "unspecified" = "#999999",
      "NMDA receptor antagonist" = "#66C2A5",
      "serotenergic psychedelic" = "#FC8D62",
      "cannabinoid" = "#8DA0CB" 
    ))
ggsave("Other_drug_count.png", Other_drug_count, width = 10, height = 15)

# === Combine Other Drug Class and Count Plots ===
other_drug_two_panel <- Other_drug_class /
  Other_drug_count
ggsave("other_drug_two_panel.png", other_drug_two_panel, width = 10, height = 12)
ggsave("other_drug_two_panel.eps", other_drug_two_panel, width = 10, height = 12)

# === Unique Trials Including Ketamine ===
unique_trials_ketamine <- Other_drug_interventions_only |> 
  filter(drug_name_generic == "ketamine") |> 
  distinct(trial_id)

# === Distribution of Drugs per Trial (Unique Drugs Only) ===
summary_all_drug_freq <- all_drug_depression |>
  distinct(trial_id, drug_name_generic) |>
  group_by(trial_id) |>
  summarise(
    num_drugs = n(),
    inc_drug = paste(sort(unique(drug_name_generic)), collapse = ", ")) |>
  ungroup()

# Plot distribution of number of unique drugs per trial
all_drug_freq_distribution_plot <- ggplot(summary_all_drug_freq, aes(num_drugs)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
  theme_minimal()
ggsave("drug_freq_distrib.png", all_drug_freq_distribution_plot, width=15, height = 10)

# Calculate percentage of trials by number of unique drugs
all_drug_freq_percent <- summary_all_drug_freq |> 
  group_by(num_drugs) |> 
  summarise(Percentage = n() / nrow(summary_all_drug_freq)*100)

# === Distribution Including Duplicate Drugs (e.g., Different Dosages) ===
all_drug_freq_duplicates <- all_drug_depression |>
  group_by(trial_id) |>
  summarise(
    num_drugs = n(),
    inc_drug = paste(drug_name_generic, collapse = ", ")) |>
  ungroup()

# Plot distribution including duplicates
all_drug_freq_duplicates_plot <- ggplot(all_drug_freq_duplicates, aes(num_drugs)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
  theme_minimal()
ggsave("drug_freq_distrib_dup.png", all_drug_freq_distribution_plot, width=15, height = 10)

# Calculate percentage including duplicates
all_drug_freq_percent_dup <- all_drug_freq_duplicates |> 
  group_by(num_drugs) |> 
  summarise(Percentage = n() / nrow(all_drug_freq_duplicates)*100)
print(all_drug_freq_percent_dup)

# === Trial Start Year Analysis ===
# Extract start year from trial start date
detailed$start_year <- strftime(detailed$`Start date`, format = "%Y")
Start_year_df <- detailed |>  select(`Trial ID`, start_year)
Start_year_df <- Start_year_df |> 
  rename("trial_id" = `Trial ID`)

# Join start year to antidepressant trials
AD_trials_start_year <- left_join(AD_interventions_only, Start_year_df, by = "trial_id")

#Count trials by start year and drug class
trial_count <- AD_trials_start_year |>
  filter(start_year != "NA") |> 
  mutate(start_year = as.numeric(start_year)) |> 
  group_by(start_year, Drug_class) |>
  summarise(num_trials = n(), .groups = "drop")

# Plot antidepressant drug class over time
AD_class_by_year <- ggplot(trial_count, aes(x = start_year, y = num_trials, color = Drug_class, group = Drug_class)) +
  geom_line(linewidth = 1) +
  labs(title = "Trials over time by drug class",
       x = "Start Year",
       y = "count drug class included in trial",
       color = "Drug Class") +
  theme_minimal() +
  theme(axis.text = element_text(size = 18)) +
  theme(axis.title = element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 18)) +
  theme(title = element_text(size = 20))
ggsave("AD_class_by_year.png", AD_class_by_year, width = 16, height = 8)

# === All Depression Drug Trials Over Time ===
all_drug_depression_start_year <- left_join(all_drug_depression, Start_year_df, by = "trial_id")
all_drug_depress_start_year_count <- all_drug_depression_start_year |> 
  filter(start_year != "NA") |> 
  mutate(start_year = as.numeric(start_year)) |> 
  group_by(start_year, Drug_class) |> 
  summarise(num_trials = n(), .groups = "drop")

All_drug_class_by_year <- ggplot(all_drug_depress_start_year_count, aes(x = start_year, y = num_trials, color = Drug_class, group = Drug_class)) +
  geom_line(linewidth = 1) +
  labs(title = NULL,
       x = "Trial start date",
       y = "Count of drug class",
       color = "Drug Class") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12) 
  ) +
  scale_colour_manual(values = c(
    "MAOI" = "#E41A1C",
    "NDRI" = "#377EB8",
    "NRI" = "#4DAF4A",
    "other" = "#984EA3", 
    "SARI" = "#FF7F00",
    "SNRI" = "#FFFF33",
    "SSRI" = "#9590FF",
    "TCA" = "#A65628",
    "tetracyclic" = "#F781BF",
    "unspecified" = "#999999",
    "NMDA receptor antagonist" = "#66C2A5",
    "serotenergic psychedelic" = "#FC8D62",
    "cannabinoid" = "#8DA0CB"
  ))

ggsave("All_drug_class_by_year.png", All_drug_class_by_year, width = 8, height = 4)

# === Filter Timeline to 1987–2024 (Exclude Incomplete Years) ===
count_trials_w_start_year <- detailed |> 
  count(start_year) |> 
  print(n=40)

Start_year_df_87_24 <- Start_year_df |> 
  filter(start_year != "2025") |> 
  filter(start_year != "2026")

all_drug_depression_start_87_24 <- left_join(all_drug_depression, Start_year_df_87_24, by = "trial_id")
all_drug_depress_start_year_count_87_24 <- all_drug_depression_start_87_24 |> 
  filter(start_year != "NA") |> 
  mutate(start_year = as.numeric(start_year)) |> 
  group_by(start_year, Drug_class) |> 
  summarise(num_trials = n(), .groups = "drop")

All_drug_class_by_year_87_24 <- ggplot(all_drug_depress_start_year_count_87_24, aes(x = start_year, y = num_trials, color = Drug_class, group = Drug_class)) +
  geom_line(linewidth = 1) +
  labs(title = NULL,
       x = "Trial start date",
       y = "Count of drug class",
       color = "Drug Class") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 16), 
    legend.text = element_text(size = 16), 
    legend.title = element_text(size = 16) 
  ) +
  scale_colour_manual(values = c(
    "MAOI" = "#E41A1C",
    "NDRI" = "#377EB8",
    "NRI" = "#4DAF4A",
    "other" = "#984EA3", 
    "SARI" = "#FF7F00",
    "SNRI" = "#FFFF33",
    "SSRI" = "#9590FF",
    "TCA" = "#A65628",
    "tetracyclic" = "#F781BF",
    "unspecified" = "#999999",
    "NMDA receptor antagonist" = "#66C2A5",
    "serotenergic psychedelic" = "#FC8D62",
    "cannabinoid" = "#8DA0CB"
  ))
ggsave("all_drug_class_by_year_87_24.png", All_drug_class_by_year_87_24, width = 10, height = 5)
ggsave("all_drug_class_by_year_87_24.eps", All_drug_class_by_year_87_24, width = 10, height = 5)

# === Demographics Availability Summary ===
available_demographics <- demographics |>
  summarise(across(everything(), ~ sum(!is.na(.)))) |>
  pivot_longer(cols = everything(), names_to = "variable", values_to = "no. trials with available data") |> 
  mutate(variable = gsub("_", " ", variable))

# Export full demographics availability table
available_demographics_full_table <- flextable(available_demographics) |> 
  save_as_docx(path = "available_demographics_full.docx")

# Export selected subset of demographics variables
available_demographics_mode <- available_demographics |>
  slice(c(1, 2, 6, 10, 14, 18, 30, 34, 38, 39, 43, 47, 51, 55, 59, 63, 67))

available_demographics_table <- flextable(available_demographics_mode) |> 
  save_as_docx(path = "available_demographics.docx")

# === Demographics Missingness Analysis ===
missing_demo_data <- data.frame(
  variable = names(demographics),
  missing_count = colSums(is.na(demographics))
)|>
  filter(if_any(contains("mean"), ~ !is.na(.))) |> 
  slice(c(2, 3, 6, 7, 10, 11, 14, 15, 18, 19, 30, 31, 34, 35, 38, 39, 40, 43, 44, 47, 48, 51, 52, 55, 56, 59, 60, 63, 64, 67,68)) |> 
  mutate(variable = gsub("_", " ", variable))

# Calculate missing percentage
missing_demo_data$missing_percent <- (missing_demo_data$missing_count / 8853) * 100

# Plot missingness
demographics_missingness <- ggplot(missing_demo_data, aes(x = reorder(variable, -missing_count), y = missing_count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Missing Values", x = "Variable", y = "Count of Missing Values") +
  geom_text(aes(label = paste0(round(missing_percent, 2), "%")), hjust = 1.1,  size = 3.5, colour = "white") +
  theme_minimal()
ggsave("demographics_missingness.png",demographics_missingness, width = 5, height = 5)
ggsave("demographics_missingness.eps",demographics_missingness, width = 5, height = 5)


# === Sample Size Analysis ===

# Filter trials with available sample size data
sample_size_available <- detailed |> 
  filter(`Sample size (estimated)` != "NA") 
count(sample_size_available)

# Filter trials with sample size ≥ 100
sample_100_plus <- detailed |> 
  filter(`Sample size (estimated)` >=100) 
print(sample_100_plus)

# Calculate percentage of trials with sample size ≥ 100
sample_100_plus_percent <- sample_100_plus |> 
  summarise(Percentage = n() / nrow(sample_size_available)*100)
print(sample_100_plus_percent)

# Filter trials with sample size ≥ 1000
sample_1000_plus <- detailed |> 
  filter(`Sample size (estimated)` >=1000) 
print(sample_1000_plus)

# Calculate percentage of trials with sample size ≥ 1000
sample_1000_plus_percent <- sample_1000_plus |> 
  summarise(Percentage = n() / nrow(sample_size_available)*100)
print(sample_1000_plus_percent)

# === Demographics Summary ===
summary(demographics) 

# Standard deviation for age minimum (mean)
sd(demographics$age_minimum_mean, na.rm = TRUE)

# Standard deviation for age maximum (mean)
sd(demographics$age_maximum_mean, na.rm = TRUE)

# Standard deviation for age mean (mean)
sd(demographics$age_mean_mean, na.rm = TRUE)

# === Summarize Range of Scores Across All Queries ===
# Select relevant vector score columns from the detailed dataset
vector_score_filter <- detailed |>
  select(`SSRI score`, `SNRI score`, `TCA score`, `NARI score`, `MAOI score`, `Adverse Events/Side Effects score`, `Antidepressant Response score`, `Antidepressant Mechanisms score`, `Blood Sample score`, `Biospecimens score`, `Genetics score`, `Genome score`, `Epigenomics score`, `Pharmacogenetics score`, `Cytochrome P450 score`, `Single Nucleotide Polymorphism score`, `DNA score`, `DNA Methylation score`, `Proteomics score`, `Causal Inference score`, `Electronic Health Records score`, `Lived Experience score`, `Patient Involvement score`)

# Summarize min and max for each query score
vector_score_summary <- vector_score_filter |>
  summarise(across(everything(),
                   list(min = ~min(., na.rm = TRUE), max = ~max(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}")) |>
  pivot_longer(cols = everything(),
               names_to = c("Query group", "Min/Max"),
               names_sep = "_",
               values_to = "Value")


# === Violin Plot: All Query Scores ===
# Reshape data for plotting
all_vector_scores_longer <- pivot_longer(vector_score_filter, cols = everything(), names_to = "Query", values_to = "Vector score")

# Create violin plot with boxplot overlay
all_vector_plot <- ggplot(all_vector_scores_longer, aes(Query, `Vector score`)) +
  geom_violin(trim = FALSE) +
  theme_minimal() +
  labs(title = NULL,
       x = "Query",
       y = "Vector score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_boxplot(width = 0.16, outlier.shape = NA, coef = 0)

# Save the plot
ggsave("all_vector_plot.png", all_vector_plot, width = 10, height = 8)
ggsave("all_vector_plot.eps", all_vector_plot, width = 10, height = 8)


# === Violin Plot: Gene-Related Queries ===
vector_score_gene <- detailed |> 
  select(`Genetics score`, `Genome score`, `Pharmacogenetics score`, `Proteomics score`, `DNA score`, `DNA Methylation score`, `Cytochrome P450 score`, `Epigenomics score`, `Single Nucleotide Polymorphism score`)
vector_score_gene_long <- pivot_longer(vector_score_gene, cols = everything(), names_to = "Query", values_to = "Vector score")
vector_plot_gene <- ggplot(vector_score_gene_long, aes(Query, `Vector score`)) +
  geom_violin(trim = FALSE) + 
  ylim(0,1) +
  theme_minimal() +
  labs(title = NULL,
       x = "Query",
       y = "Vector score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_boxplot(width = 0.16, outlier.shape = NA, coef = 0) +
  scale_x_discrete(labels = c("Cytochrome\nP450 score", "DNA\nMethylation\nscore", "DNA score", "Epigenomics\nscore", "Genetics score", "Genome score", "Pharmacogenetics\nscore", "Proteomics score", "Single Nucleotide\nPolymorphism score"))
vector_plot_gene
ggsave("vector_plot_gene.png", vector_plot_gene, width = 8, height = 5)
ggsave("vector_plot_gene.eps", vector_plot_gene, width = 8, height = 5)

# === Violin Plot: Other Queries ===
vector_score_other <- detailed |>
  select(`Blood Sample score`, `Biospecimens score`, `Causal Inference score`, `Electronic Health Records score`, `Lived Experience score`, `Patient Involvement score`)
vector_score_other_long <- pivot_longer(vector_score_other, cols = everything(), names_to = "Query", values_to = "Vector score")
vector_plot_other <- ggplot(vector_score_other_long, aes(Query, `Vector score`)) +
  geom_violin(trim = FALSE) +
  ylim(0,1) +
  theme_minimal() +
  labs(title = NULL,
       x = "Query",
       y = "Vector score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_boxplot(width = 0.16, outlier.shape = NA, coef = 0) +
  scale_x_discrete(labels = c("Biospecimens\nscore", "Blood sample\nscore", "Causal Inference\nscore", "Electronic Health\nRecords score", "Lived Experience\nscore", "Patient Involvement\nscore")) 

vector_plot_other
ggsave("vector_plot_other.png", vector_plot_other, width = 8, height = 5)
ggsave("vector_plot_other.eps", vector_plot_other, width = 8, height = 5)

#violin plots for subset of vector queries - biospecimens, genetics, proteomics, SSRI, antidepressant response 
vector_score_subset <- detailed |> 
  select(`Biospecimens score`, `Genetics score`, `Proteomics score`, `SSRI score`, `Antidepressant Response score`)
vector_score_subset_long <- pivot_longer(vector_score_subset, cols = everything(), names_to = "Query", values_to = "Vector score")

# === Violin Plot: Selected Queries ===
vector_plot_subset <- ggplot(vector_score_subset_long, aes(Query, `Vector score`)) +
  geom_violin(trim = FALSE) +
  theme_minimal() +
  labs(title = NULL,
       x = "Query",
       y = "Vector score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_boxplot(width = 0.16, outlier.shape = NA, coef = 0) +
  scale_x_discrete(labels = c("Antidepressant\nResponse score", "Biospecimens score", "Genetics score", "Proteomics score", "SSRI score"))
vector_plot_subset


# === Biospecimens Query ===
# Select relevant columns
biospecimens_scores <- detailed |> 
  select(`Trial ID`, Title, `Biospecimens score`)

# Extract top and bottom 20 trials by score
biospec_top <- biospecimens_scores |>  top_n(20, `Biospecimens score`) |>  arrange(desc(`Biospecimens score`))
biospec_bottom <- biospecimens_scores |>  top_n(-20, `Biospecimens score`) |>  arrange(desc(`Biospecimens score`))

# Manually assign relevance scores (1 = relevant, 0 = not relevant, 0.5 = partially relevant)
biospec_top$relevance <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, .5, 1, 1, 1, 1, 1, 1, 1, 1, 1)
biospec_bottom$relevance <- c(0, 0, 0, 1, .5, 0, 1, 0, .5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

# Ensure only 20 trials are used for precision calculation
biospec_top <- slice_head(biospec_top, n = 20) 

# Calculate Precision@20
biospec_precision20 <- biospec_top |> 
  count(relevance) |>
  summarise(precision = sum(n[relevance == 1]) / sum(n))

biospec_precision20_bottom <- biospec_bottom |> 
  count(relevance) |>
  summarise(precision = sum(n[relevance == 1]) / sum(n))


# === Precision@20 Function ===
# General function to calculate precision from a dataframe with relevance scores
vector_precision_20 <- function(x)  #where x is the data frame of the top or bottom 20 with relevance assessment column
{
  precision_20 <- x |> 
    count(relevance) |> 
    summarise(precision = sum(n[relevance == 1]) / sum(n)) 
  return(precision_20)
}

# === Genetics Query ===
genetics_scores <- detailed |> 
  select(`Trial ID`, Title, `Genetics score`)

genetics_top <- genetics_scores |>  top_n(20, `Genetics score`) |>  arrange(desc(`Genetics score`))
genetics_bottom <- genetics_scores |>  top_n(-20, `Genetics score`) |>  arrange(desc(`Genetics score`))

genetics_top$relevance <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
genetics_bottom$relevance <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

genetics_precision20 <- vector_precision_20(genetics_top) 
genetics_precision20_bottom <- vector_precision_20(genetics_bottom) 

# === Proteomics Query ===
proteomics_scores <- detailed |> 
  select(`Trial ID`, Title, `Proteomics score`)

proteomics_top <- proteomics_scores |>  top_n(20, `Proteomics score`) |>  arrange(desc(`Proteomics score`))
proteomics_bottom <- proteomics_scores |>  top_n(-20, `Proteomics score`) |>  arrange(desc(`Proteomics score`))

proteomics_top$relevance <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
proteomics_bottom$relevance <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

proteomics_precision20 <- vector_precision_20(proteomics_top)
proteomics_precision20_bottom <- vector_precision_20(proteomics_bottom) 

# === SSRI Query ===
SSRI_scores <- detailed |> 
  select(`Trial ID`, Title, `SSRI score`)

SSRI_top <- SSRI_scores |>  top_n(20, `SSRI score`) |>  arrange(desc(`SSRI score`))
SSRI_bottom <- SSRI_scores |>  top_n(-20, `SSRI score`) |>  arrange(desc(`SSRI score`))

SSRI_top$relevance <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
SSRI_bottom$relevance <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5)
SSRI_top <- slice_head(SSRI_top, n = 20) #initial SSRI_top search gave 28 results, results with equal vector scores. At relevance assessment, all deemed relevant, so random selection from equal vector scores to make up rest of top 20 for precision calculations

SSRI_precision20 <- vector_precision_20(SSRI_top) 
SSRI_precision20_bottom <- vector_precision_20(SSRI_bottom) 

# === Antidepressant Response Query ===
ADR_scores <- detailed |> 
  select(`Trial ID`, Title, `Antidepressant Response score`)

ADR_top <- ADR_scores |>  top_n(20, `Antidepressant Response score`) |>  arrange(desc(`Antidepressant Response score`))
ADR_bottom <- ADR_scores |>  top_n(-20, `Antidepressant Response score`) |>  arrange(desc(`Antidepressant Response score`))

ADR_top$relevance <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0.5)
ADR_bottom$relevance <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

ADR_precision20 <- vector_precision_20(ADR_top) 
ADR_precision20_bottom <- vector_precision_20(ADR_bottom) 


# === Combine Precision@20 Results ===
# Create a list of query group labels
query_group_names <- c("biospec_precision", "biospec_precision_bottom", "genetics_precision", "genetics_precision_bottom", "proteomics_precision", "proteomics_precision_bottom", "SSRI_precision", "SSRI_precision_bottom", "ADR_precision", "ADR_precision_bottom")

# Combine all precision results into one dataframe
precision_df <- bind_rows(biospec_precision20, biospec_precision20_bottom, genetics_precision20, genetics_precision20_bottom, proteomics_precision20, proteomics_precision20_bottom, SSRI_precision20, SSRI_precision20_bottom, ADR_precision20, ADR_precision20_bottom) |> 
  mutate(query_group_names) |> 
  relocate(query_group_names) |> 
  rename(query_group = query_group_names) |> 
  rename("precision@20" = precision)

# Export precision table to Word document
precision_df_table <- flextable(precision_df) |> 
  save_as_docx(path = "precision_df_table.docx")

# Add precision labels manually to vector plot for subset of queries
precision_labels <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(1.1, 1.1, 1.1, 1.1, 1.1),
  label = c("Precision@20 =\n0.85 / 0", "Precision@20 =\n0.95 / 0.1", "Precision@20 =\n1 / 0.05", "Precision@20 =\n0.1 / 0", "Precision@20 =\n1 / 0")
)

vector_plot_subset <- vector_plot_subset +
  geom_label(data = precision_labels, aes(x = x, y = y, label = label))
ggsave("vector_plot_subset.png", vector_plot_subset, width = 8, height = 8)

# === Create Final Versions of Files ===

# Step 1: Clean and process intervention data
interventions_detailed <- interventions |>
  mutate(
    # Create a cleaned version of the intervention name, preferring 'drug' if available
    intervention_clean = if_else(is.na(drug), intervention, drug),
    # Convert to lowercase
    intervention_clean = tolower(intervention_clean),
    # Remove non-alphabetic characters
    intervention_clean = gsub("[^a-zA-Z\\s]", "", intervention_clean),
    # Trim leading/trailing whitespace
    intervention_clean = trimws(intervention_clean)
  ) |>
  # Join with drug lookup table to get generic drug names
  left_join(All_Drug_Lookup, by = c("intervention_clean" = "Drug_name"), relationship = "many-to-many") |>
  # Create a final processed intervention name, using generic name if available
  mutate(intervention_processed = if_else(is.na(drug_name_generic), intervention, drug_name_generic)) |>
  # Move the processed column next to trial_id for clarity
  relocate(intervention_processed, .after = trial_id)

# Step 2: Add processed intervention names to summary data
interventions_summary_processed <- interventions_summary |>
  left_join(
    interventions_detailed |> select(trial_id, intervention, intervention_type, intervention_processed),
    by = c("trial_id", "intervention", "intervention_type")
  )

# Step 3: Collapse multiple processed interventions per trial into a single string
collapsed <- interventions_detailed |>
  group_by(trial_id) |>
  summarise(
    intervention_processed = paste(unique(intervention_processed), collapse = "; "),
    .groups = "drop"
  )

# Step 4: Create detailed trial dataset with sponsor categories and processed interventions
trials_detailed <- detailed |>
  # Add sponsor category
  left_join(sponsor_cat_df, by = "Sponsor") |>
  # Add collapsed intervention data
  left_join(collapsed, by = c(`Trial ID` = "trial_id")) |>
  # Reorder columns for readability
  relocate(start_year, .before = `Start date`) |>
  relocate(sponsor_type, .after = Sponsor) |>
  relocate(cond_labels, .after = Conditions) |>
  relocate(intervention_processed, .after = Interventions) |>
  # Rename cleaned conditions column
  rename(conditions_cleaned = cond_labels)

# Step 5: Remove unnecessary columns from processed dataframes
interventions_detailed <- interventions_detailed |>
  select(-drug, -intervention, -intervention_clean, -drug_name_generic)

intervention_summary <- interventions_summary |>
  select(-intervention)

trials_detailed <- trials_detailed |>
  select(-Conditions, -is_long, -cleaned, -meta_labels, -bucket, -Interventions)

# Step 6: Define named subsets for export
drug_intervention_trials <- trials_detailed |>
  filter(`Includes drug intervention` == "Yes")

antidepressant_interventions <- AD_interventions_only
EDIT <- Other_drug_interventions_only
ADDT <- all_drug_depression

# Step 7: Create list of processed data frames to include in final spreadsheet
df_list <- list(
  trials_summary = trials_summary,
  trials_detailed = trials_detailed,
  pico_view = pico_view, 
  interventions_summary_processed = interventions_summary_processed,
  interventions_detailed = interventions_detailed,
  term_search = term_search, 
  demographics = demographics, 
  demographics_detailed = demographics_detailed, 
  publications = publications,
  drug_intervention_trials = drug_intervention_trials,
  antidepressant_interventions = antidepressant_interventions,
  EDIT = EDIT,
  ADDT = ADDT
)

# Step 8: Create and populate Excel workbook
wb <- createWorkbook()

# Add each dataframe to a separate sheet
for (sheet_name in names(df_list)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df_list[[sheet_name]])
}

# Step 9: Save the workbook to file
saveWorkbook(wb, file = "clinical_trials_processed.xlsx", overwrite = TRUE)
