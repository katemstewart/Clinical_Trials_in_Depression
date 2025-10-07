# Study Reference
**Stewart et al., 2025**
**DOI**: [Insert DOI here]

## Overview
This repository accompanies the data processing and descriptive analyses for the study by Stewart et al. (2025). It includes both final outputs and all intermediate files and scripts used during processing.

---

## Folder Structure
### Final_files/
Contains the final output files. These are ready-to-use and do not require running any processing scripts.

### For_processing/
Includes:
- Raw data files
- R and Python scripts
- Input and output files generated during processing

---

## Dependencies
This project uses both **Python** and **R** for data processing and analysis. Please ensure the following dependencies are installed before running the scripts.

### Python
- **Version**: Python 3.9.20
- Install required packages using:

pip install -r requirements.txt

### R
- **Version**: R 4.3.2
- Install required packages using:

install.packages(c(
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "forcats",
  "flextable",
  "patchwork", 
  "openxlsx"
))


## Data Processing Steps
### Step 1: Download the For_processing folder
Ensure the following files are included:
- clinical_trials_raw_data.xlsx
- condition_maps.py
- conditions_pipeline_gh.py
- AD_All_Lookup.xlsx
- Other_Drug_Lookup.xlsx


### Step 2: Run conditions_pipeline_gh.py
Inputs:
- clinical_trials_raw_data.xlsx
- condition_maps.py

Outputs:
- interim_withconditions.xlsx (used in Step 3)
- term_by_category.xlsx -> table of the top categories, the sub-categories, and their counts (Table 1)
- depression_coocurrence.png / tif -> Figure 5
- mental_behavioural_subcategory_counts.png / tif -> Figure 6


### Step 3: Run cleaning_analysis.r  
Inputs:
- clinical_trials_raw_data.xlsx
- interim_withconditions.xlsx
- AD_All_Lookup.xlsx
- Other_Drug_Lookup.xlsx

Outputs:
- clinical_trials_processed.xlsx
- status_register_table.docx -> Table 3
- sponsor_category_table.docx -> Table 4
- intervention_type.png / eps -> Figure 3
- AD_two_panel.png / eps -> Figure 4
- all_drug_class_by_year_87_24.png / eps -> Figure 7
- vector_plot_subset.png / eps -> Figure 8
- available_demographics_full.docx -> Supplementary table 2
- unique_intervention_table.docx -> Supplementary table 3
- sponsor_table.docx -> Supplementary table 4 (column 1)
- drug_sponsor_table.docx -> Supplementary table 4 (column 2)
- depress_drug_sponsor_table.docx -> Supplementary table 4 (column 3)
- demographics_missingness.png / eps -> Supplementary Figure 1
- other_drug_two_panel.png / eps -> Supplementary Figure 2
- vector_plot_drug.png / eps -> Supplementary Figure 3
- vector_plot_gene.png / eps -> Supplementary Figure 3
- vector_plot_other.png / eps -> Supplementary Figure 3
- AD_class_by_year.png
- AD_count.png / eps
- AD_drug_class.png / eps
- AD_drug_interventions.csv
- AD_plot_min25.png / eps
- all_drug_class_by_year.png
- available_demographics.docx
- drug_freq_distrib.png
- drug_freq_distrib_dup.png
- include_drug_int.png / eps
- other_drug_class_count.png / eps
- Other_drug_count.png
- precision_df_table.docx
- trial_phase_all.png
- trial_status_all.png
- trial_status_drug_int.png
