#=========================================================================
# Purpose: Main R file for Preparing/Cleaning Data
# Author: Jacob Jameson 
#=========================================================================
rm(list = ls()) 

library(tidyverse)
library(stringr)
library(lfe)

#=========================================================================
# Determine test times
#=========================================================================

path <- '~/Dropbox/Batch vs sequential testing/Data/'

df <- read.csv(paste0(path, 'deidentified_FINAL.csv'))

test_columns = c("US_PERF", "NON_CON_CT_PERF", "CON_CT_PERF", 
                 "LAB_PERF", "XR_PERF")


colnames(df)[colnames(df) == "PLAIN_XRAY"] = "XR_PERF"
colnames(df)[colnames(df) == "US_ORDER_DTTM_REL"] ="US_ORDER_DTTM_REL"
colnames(df)[colnames(df) == "CT_WITHOUT_CONTR_ORDER_DTTM_REL"] = "NON_CON_CT_ORDER_REL"
colnames(df)[colnames(df) == "CT_WITH_CONTR_ORDER_DTTM_REL"] ="CON_CT_ORDER_REL"
colnames(df)[colnames(df) == "LAB_ORDER_DTTM_REL"] ="LAB_ORDER_REL"
colnames(df)[colnames(df) == "PLAIN_XRAY_ORDER_DTTM_REL"] ="XR_ORDER_REL"

df$CT_PERF = ifelse(df$NON_CON_CT_PERF=='Y' | df$CON_CT_PERF=='Y', 1, 0)

df$ESI = as.character(df$ESI)

for (i in test_columns){
  df[[i]] = ifelse(df[[i]] =='Y', 1, 0) 
}

df$nEDTests = rowSums(df[test_columns])

df[append(test_columns, "nEDTests")]


# Identify columns with *_REL suffix
rel_cols <- grep("_REL$", names(df), value = TRUE)

# Apply the transformation to each *_REL column
for (col in rel_cols) {
  df <- df %>%
    separate(col, c(paste0(col, "_hours"), paste0(col, "_minutes")), sep = ":") %>%
    mutate(!!col := as.numeric(get(paste0(col, "_hours"))) * 60 + 
             as.numeric(get(paste0(col, "_minutes"))))
}

df <- df %>%
  select(-matches("_hours$|_minutes$"))

#=========================================================================
# Determine batching
#   - criteria: ordered within 5 minutes of each other
#               first tests ordered were in a batch
#=========================================================================

# Determine which was the first test ordered -----------------------------
# Columns of interest

cols_of_interest <- c('US_ORDER_DTTM_REL', 'NON_CON_CT_ORDER_REL', 'CON_CT_ORDER_REL',
                      'LAB_ORDER_REL', 'XR_ORDER_REL')

# Function to determine batch
determine_batch <- function(row) {
  min_time <- min(row, na.rm = TRUE)
  tests <- names(row)
  
  batch <- tests[which(row <= min_time + 5 & !is.na(row))]
  
  return(paste(batch, collapse = ","))
}

# Apply function to each row for the columns of interest
df$batch <- apply(df[cols_of_interest], 1, determine_batch)

#=========================================================================
# Lab-Image Batch --------------------------------------------------------
#=========================================================================
# Function to determine if a batch meets the criteria
is_lab_image_batch <- function(batch) {
  lab_present <- grepl("LAB_ORDER_REL", batch)
  other_tests_present <- any(sapply(c('US_ORDER_DTTM_REL',
                                      'NON_CON_CT_ORDER_REL', 
                                      'CON_CT_ORDER_REL', 
                                      'XR_ORDER_REL'), function(test) grepl(test, batch)))
  
  return(as.integer(lab_present & other_tests_present))
}

df$lab_image_batch <- sapply(df$batch, is_lab_image_batch)

#=========================================================================
# Image-Image Batch ------------------------------------------------------
#=========================================================================
# Function to determine if a batch meets the criteria for image_image_batch
is_image_image_batch <- function(batch) {
  image_tests <- c('US_ORDER_DTTM_REL', 'NON_CON_CT_ORDER_REL', 
                   'CON_CT_ORDER_REL', 'XR_ORDER_REL')
  
  batch_tests <- unlist(strsplit(batch, ","))
  count_present <- sum(image_tests %in% batch_tests)
  
  return(as.integer(count_present >= 2))
}

df$image_image_batch <- sapply(df$batch, is_image_image_batch)

#=========================================================================
# Any Batch --------------------------------------------------------------
#=========================================================================

df$any.batch <- ifelse(
  df$lab_image_batch == 1 | df$image_image_batch == 1, 1, 0
)


# Function to determine the sequenced tests
get_sequenced <- function(batch, row) {
  all_tests <- c('US_ORDER_DTTM_REL', 'NON_CON_CT_ORDER_REL', 
                 'CON_CT_ORDER_REL', 'LAB_ORDER_REL', 'XR_ORDER_REL')
  
  batch_tests <- unlist(strsplit(batch, ","))
  
  # Identify tests not in the batch
  non_batch_tests <- setdiff(all_tests, batch_tests)
  
  # Check if any of these tests have a non-NA value in the current row
  sequenced_tests <- non_batch_tests[!is.na(row[non_batch_tests])]
  
  if (length(sequenced_tests) > 0) {
    return(paste(sequenced_tests, collapse = ","))
  } else {
    return(NA)
  }
}

df$sequenced <- apply(df, 1, function(row) get_sequenced(as.character(row["batch"]), row))

#=========================================================================
# Clean Vars -------------------------------------------------------------
#=========================================================================

df$PATIENT_RACE <- str_to_lower(df$PATIENT_RACE)

df <- df %>%
  mutate(race = case_when(
    grepl('black', PATIENT_RACE, fixed = TRUE) ~ "black",
    grepl('african', PATIENT_RACE, fixed = TRUE) ~ "black",
    grepl('asian', PATIENT_RACE, fixed = TRUE) ~ "asian",
    grepl('pacific islander', PATIENT_RACE, fixed = TRUE) ~ "asian",
    grepl('native', PATIENT_RACE, fixed = TRUE)~ "native",
    grepl('samoan', PATIENT_RACE, fixed = TRUE) ~ "other",
    grepl('guamanian or chamorro', PATIENT_RACE, fixed = TRUE) ~ "other",
    grepl('white', PATIENT_RACE, fixed = TRUE) ~ "white",
    grepl('unknown', PATIENT_RACE, fixed = TRUE) ~ "unknown",
    grepl('choose not to disclose', PATIENT_RACE, fixed = TRUE) ~ "unknown",
    grepl('unable to provide', PATIENT_RACE, fixed = TRUE) ~ "unknown",
    grepl('other', PATIENT_RACE, fixed = TRUE) ~ "other",
    grepl('', PATIENT_RACE, fixed = TRUE) ~ "unknown",
    TRUE ~ PATIENT_RACE))

df$ARRIVAL_AGE_DI <- ifelse(
  df$ARRIVAL_AGE_DI == '85+', '85', df$ARRIVAL_AGE_DI
)
df$ARRIVAL_AGE_DI <- as.numeric(df$ARRIVAL_AGE_DI)

#=========================================================================
# Clean Chief Complaint --------------------------------------------------
#=========================================================================

complaints <- list(
  "Abdominal Complaints" = 
    c('Abdominal Cramping', 'Abdominal Distention', 'Dyspepsia',
      'Abdominal Pain', 'Ascites', 'Hernia', 
      'Abdominal Aortic Aneurysm', 'Abdominal Injury', "Pancreatitis",
      'Umbilical Hernia'),
  'Abnormal Test Results' = 
    c('Abnormal Lab', 'Abnormal Potassium', 'Abnormal Calcium', 
      'ECG Changes', 'Abnormal ECG', 'Abnormal Test Result', 
      'Blood Infection', 'Acute Renal Failure', 'Hypocalcemia',
      'Chronic Renal Failure', 'Pulmonary Embolism', 'Abnormal X-ray', 
      'Hypoglycemic Unawareness', 'Elevated Blood Pressure', 
      'Abnormal Sodium', 'Hyperglycemia', 'Hyponatremia', 
      'Platelet Disorders', 'Anemia', 'Hypoglycemia', 'Hypertension', 
      'Hypotension', 'Abnormal Chest Imaging', 'Abnormal Oximetry', 
      'Abnormal Stress Test', 'Blood Sugar Problem',
      'Hypocalcemia', 'Hyponatremia'),
  'Allergic Reaction' = 
    c('Allergic Reaction', 'Anaphylaxis'),
  'Back or Flank Pain' = 
    c('Back Pain', 'Back Problem', 'Flank Pain', 
      'Sciatica', 'Back Injury', 'Disc Disorder'),
  'Breast Complaints' = 
    c('Breast Mass', 'Breast Pain', 'Breast Problem', 'Breast Discharge',
      'Breast Cancer', 'Breast Discharge', 'Breast Inflammation'),
  'Cardiac Arrhythmias' = 
    c('Atrial Fibrillation', 'Atrial Flutter', 'Cardiac Valve Problem',
      'Bradycardia', 'Irregular Heart Beat', 'Palpitations', 'POTS', 
      'Ventricular Tachycardia','Rapid Heart Rate', 'Heart Problem', 
      'Cardiac Arrest', 'Congestive Heart Failure', 'Circulatory Problem',  
      "Transient Ischemic Attack", 'Ventricular Tachycardia'),
  'Chest Pain' = 
    c('Chest Injury', 'Chest Pain', 'Chest Wall Pain', 'Angina',
      'Collarbone Injury', 'Rib Injury', 'Heart Pain'),
  'Dizziness/Lightheadedness/Syncope' = 
    c('Dizziness', 'Near Syncope', 'Syncope', 'Vertigo', 'Spells',
      'Hypotension', 'Paroxysmal Positional Vertigo', 
      'Paroxysmal Positional Vertig'),
  'Ear Complaints' = 
    c('Cerumen Impaction', 'Ear Drainage', 'Ear Fullness',
      'Ear Laceration', 'Ear Problem', 'Earache',
      'Hearing Problem', 'Tinnitus', 'Ear Injury', 'Hearing Loss',
      'Nasal Trauma'),
  'Epistaxis' = 
    c('Epistaxis', 'Epistaxis (Nose Bleed)', 'Nose Problem'),
  'Exposures, Bites, and Envenomations' =
    c('Animal Bite', 'Body Fluid Exposure', 'Chemical Exposure', 
      'Poisoning', 'Exposure to STD', 'Insect Bite', 'Smoke Inhalation', 
      'Radiation', 'Snake Bite', 'Toxic Inhalation'),
  'Extremity Complaints' = 
    c('Ankle Injury', 'Ankle Pain', 'Arm Injury', 'Arm Pain', 
      'Cold Extremity', 'Arm Swelling', 'Arthritis', 'Elbow Injury', 
      'Elbow Pain', 'Pseudogout', 'Extremity Pain', 'Extremity Weakness', 
      'Finger Injury', 'Hip Injury', 'Extremity Weakness', 
      'Finger Injury', 'Finger Pain', 'Dislocation',
      'Foot Infection', 'Foot Injury', 'Foot Numbness', 'Foot Pain',
      'Foot Swelling', 'Foot Ulcer', 'Foot Wound Check', 'Hand Injury',
      'Hand Pain', 'Hip Pain', 'Joint Pain', 'Joint Swelling', 
      'Knee Injury', 'Knee Pain', 'Knee Problem', 'Leg Injury', 
      'Leg Pain', 'Leg Swelling', 'Lower Extremity Issue', 
      'Pain in Limb', 'Shoulder Injury', 'Shoulder Pain', 'Toe Injury', 
      'Upper Extremity Issue', 'Wrist Injury',
      'Wrist Pain', 'Hand Problem', 'Leg Cramps', 'Arm Problem',
      'Foot Problem', 'Pain In Limb', 'Toe Pain', 'Hand Burn',
      'Foot Burn', 'Neck Injury', 'Leg Problem', 'Deep Vein Thrombosis',
      "Varicose Veins",  "Ingrown Toenail", 'Hip Injury', 'Wound Care', 
      'Venous Thromboembolic Diseas'),
  'Eye Complaints' = 
    c('Blurred Vision', 'Decreased Visual Acuity', 'Diplopia', 
      'Detached Retina', 'Eye Drainage', 'Eye Exposure', 'Eye Pain', 
      'Eye Problem', 'Eye Swelling', 'Eye Trauma', 'Foreign Body Eye',
      'Flashes/Light', 'Loss of Vision', 'Red Eye', 'Visual Field Change', 
      'Eyelid Problem', 'Itchy Eye', 'Eye Exam', 'Burning Eyes', 
      'Eye Twitching', "Eyelid/brow Lift Evaluation",
      'Strabismus', 'Glaucoma',  "Spots/Floaters"),
  'Falls, Motor Vehicle Crashes, Assaults, and Trauma' =
    c('Assault Victim', 'Concussion', 'Facial Injury', 'Fall', 
      'Nasal Trauma', 'Head Injury', 'Head Laceration', 
      'Motor Vehicle Crash', 'Puncture Wound', 
      'Sexual Assault', 'Trauma', 'Domestic Violence', 'Gun Shot Wound',
      'Work Related Injury', 'Motorcycle Crash', 'Injury', 
      'Bicycle Accident', 'Near Drowning', 'Lip Laceration'),
  'Fatigue and Weakness' =
    c('Difficulty Walking', 'Fatigue', 'Gait Problem', 
      'Weakness-Generalized',
      'Chronic Fatique', 'Weakness - Generalized'),
  'Fevers, Sweats or Chills' =
    c('Chills', 'Diaphoresis', 'Fever', 'Night Sweats', 'Diaphoretic', 
      'Diapohresis', 'Hoarseness', 'Laryngitis'),
  'Foreign Body' =
    c('Food Bolus', 'Foreign Body', 'Foreign Body in Ear',
      'Foreign Body in Skin', 'Foreign Body in Vagina',
      'Swallowed Foreign Body', 'Foreign Body in Nose', 
      'Foreign Body', 'FB eye', 'Foreign Body in Rectum'),
  'Gastrointestinal Issues' =
    c('Anal Fissure', 'Black or Bloody Stool', 'Constipation', 'GERD', 
      'Anal Fistula', 'Diarrhea', 'Dysphagia', 'Fecal Impaction',
      'Fistula Follow Up', 'GIbleeding', 'GI Problem', 'Hemorrhoids', 
      'Morning Sickness', 'Nausea', 'Ostomy Care', 'Rectal Bleeding',
      'Rectal Pain', 'Vomiting', 'Vomiting Blood', 
      'Vomiting During Pregnancy',
      'GI Bleeding', 'Fecal Incontinence',
      'Bloated', 'Hematochezia', 'Urine Leakage', 'Heartburn',
      'Rectal Discharge', 'Urolithiasis', "Ulcerative Colitis",
      "Irritable Bowel Syndrome", "Rectal Prolapse","Fistula Evaluation",
      "Rectal Problems", 'Perianal Abscess', 'Fisula Evaluation', 
      'Stoma Dysfunction'),
  'Genital Complaints' =
    c('Groin Burn', 'Groin Pain', 'Groin Swelling', 'Inguinal Hernia', 
      'Menstrual Problem', 'Pelvic Pain', 'Penis Pain', 'Priapism', 
      'Testicle Pain', 'Menorrhagia',
      'Vaginal Bleed', 'Vaginal Bleeding', 'Vaginal Itching', 
      "Bartholin's Cyst", 'Genital Warts', 'Groin Injury', 
      'Vaginal Bleeding - Pregnant', 
      'Vag Bleed Pregnant', 'Female Genital Issue', 'Penis Injury',
      'Vaginal Discharge', 'Vaginal Pain', 'Erectile Dysfunction',
      'Vaginal Prolapse', 'Urethral Stricture', 'Penile Discharge',
      'Menorrhagia', "Gynecologic Exam", "Menstrual Problem", 
      "Vaginitis/Bacterial Vaginosis",
      'Ovarian Cyst', 'Vaginitis/Bacterial Vaginosi'),
  'Medical Device or Treatment Issue' =
    c('Cast Problem', 'Device Check', 'Dressing Change', 'Feeding Tube', 
      'AICD Problem', 'Insulin Pump Visit', 'Gastrostomy Tube Change', 
      'Medication Reaction', 'Shunt', 'Appliance Removal', 'Tube Problem', 
      'Urinary Catheter Change', 'Vascular Access Problem', 
      'Enteral Nutrition Evaluation', 'Device Malfunction', 
      'Pacemaker Problem', 
      'Removal / Exchange Catheter', 'Drain Removal', 
      'Outpatient Infusion', 
      'Treatment', 'Heart Assist Device', 'Stoma Dysfunction', 
      'Tracheostomy Tube Change',
      'Ureteral Stent Exchange'),
  'Medication Request' =
    c('Immunizations', 'Infusion/Injection Administration', 
      'IV Medication',  'Infusion/ Injection Administ', 'Med Refill',
      'Medication Visit', 
      'Pain Management', 'Blood Product Administration', 'Labs Only', 
      'Tetanus (Td & Tdap)', 'Wound Care'),
  'Neurological Issue' =
    c('Altered Mental Status', 'Cognitive Concerns', 'Facial Droop',
      'Pre Syncope', 'Focal Weakness', 'Headache', 'Memory Loss', 
      'Migraine', 'Dementia', 'Dysphasia',
      'Neuro Problem', 'Numbness', 'Paralysis', 'Seizures',
      'Slurred Speech', 'Spasms', 'Stroke Like Symptoms', 'Tingling',
      'Tremors', 'Trigeminal Neuralgia', 'Unable to Speak', 
      'Seizure Disorder', 'Insomnia', "Parkinson's Disease",
      'Loss of Consciousness', 'Neuropathy', 'Ataxia', 'Unable to speak',
      'Peripheral Neuropathy',
      'Stroke', 'Cerebrovascular Accident', 'Speech Problem', 
      'Acute Neurological Problem',
      'Flashes, Light', 'Unresponsive', "Multiple Sclerosis", 
      "Parkinson's Disease",
      "Febrile Seizure", 'Paresthesia', 'Peripheral Neuropathy', 
      'Hydrocephalus',
      'Spasticity', 'Neuroendocrine Tumor'),
  'Other' =
    c('Dehydration', 'Fisula Evaluation', 'Follow-Up', 'Illness', 
      'Letter for School/Work', 'Aneurysm',
      'Lung Eval', 'Error', 'Mass', 'Oral Swelling', 'Other', 
      'Advice Only', 'Deformity', 'Electric Shock',
      'Personal Problem', 'Shaking', 'Swelling', 'Swollen Glands', 
      'Adenopathy', 'Adrenal Problem',
      'Thrombophilia', 'Weight Gain', 'Weight Loss', 'Hiccups', '', 
      'Chemo Related Symptoms', 'Hot Flashes',
      'Follow-up', 'Non Healing Wound', '(Other)', 'Mouth Injury', 
      'Xerostomia', 'Prostate Check',
      'Suture / Staple Removal', 'Wellness', 'Voice Changes', 
      'Vital Sign Check', 'Coagulation Disorder',
      'Cold Exposure', 'Consult', 'Dental Problem', 
      'Tetanus (Td & Tdap)', "Infusion/ Injection Administ",
      "Tracheostomy Tube Change", 'Medical Information', 
      'Neutropenic Fever', 'Infection',
      'Leukemia',"Heat Exposure", "Poor Appetite", 'Gingivitis',
      "Pre-op Exam",
      'gingivitis', "Loss of appetite", "Failure To Thrive", 
      'Referral', 'Lymphoma',
      "Hot Flashes", 'Neutropenia', 'Radiation', 'Ingestion', 
      "TB Test", 'Fussy',
      'Lupus', 'Toxic Inhalation', 'Lung Screening', 
      'Leakage/Loss of Fluid',
      'Liver Eval', 'Hepatic Cancer', 'Lung Mass', 
      'Venous Thromboembolic Disease',
      'Insulin Pump Visit', 'Preventive Visit', 
      'Avulsion', 'Peripheral Edema',
      'Hypoglycemic Unawareness', 'Immobility', 
      'Giant Cell Arteritis', 'Polydipsia',
      'Platelet Disorders', 'Post-procedure', 'Lung Follow-up', 
      'Poisoning',
      'Injections', 'POTS', "Insulin Reaction", 
      'Liver Transplant', 'Labs Only'),
  'Other Pain' =
    c('Dental Pain', 'Facial Pain', 'Generalized Body Aches', 
      'Myalgia', 'Dental Injury',
      'Jaw Pain', 'Muscle Pain', 'Neck Pain', 'Pain', 
      'Sickle Cell Pain Crisis', 'Paresthesia',
      'Torticollis', 'Chronic Pain', 'Cancer Pain', 
      'Incisional Pain', 'Bone Pain',
      'Tailbone Pain', 'Gout', "Muscle pain/Weakness", 'Pseudogout'),
  'Post-Op Issue' =
    c('Post-Op', 'Post-Procedure', 'Post-Op Problem', 'Post-op',
      'Post-Op Issue', 'Wound Dehiscence', 'Post-op Problems',
      "Post-op Problem"),
  'Psychiatric Complaints' =
    c('Anxiety', 'Auditory Hallucinations', 'Depression', 'Panic Attack',
      'Homicidal', 'PTSD (Post-Traumatic Stress', 'Delusional', 'Fussy',
      'Paranoia', 'Suicide Attempt', 'Hallucinations', 'Manic Behavior',
      'Eating Disorder', 'Suicidal', 'Agitation', 'Psychiatric Evaluation',
      'Aggressive Behavior', 'Mental Health Problem', 
      'Inappropriate Words'),
  'Shortness of Breath' =
    c('Airway Obstruction', 'Aspiration', 'Pain With Breathing', 
      'Near Drowning',
      'Respiratory Distress', 'Shortness of Breath', 'Wheezing',
      'Increased Work Of Breathing',
      'Difficulty Breathing', 'Choking', "Oxygen Dependence",
      'Hyperventilating', 'Orthopnea'),
  'Skin Complaints' =
    c('Abrasion','Abscess', 'Bleeding/Bruising', 'Blister',
      'Angioedema', 'Lip Laceration',
      'Burn', 'Cellulitis', 'Cyst', 'Drainage from Incision', 
      'Disturb of Skin Sens',
      'Edema', 'Extremity Laceration', 'Facial Burn', 'Cyanosis',
      'Impetigo', 'Facial Laceration', 'Facial Swelling',
      'Finger Laceration', 'Leg Rash',
      'Herpes Zoster', 'Hives', 'Itching', 'Jaundice', 
      'Diabetic Ulcer', 'Diabetic Wound',
      'Laceration', 'Mouth Lesions', 'Non-Healing Wound', 'Rash', 
      'Recurrent Skin Infections', 'Skin Problem', 'Sore', 'Scabies',
      'Suture/Staple Removal', 'Wound Check', 'Wound Infection',
      'Lesion', 'Skin Check', 'Minor Skin Infection', 'Skin Ulcer',
      'Skin Discoloration', 'Sunburn', "Head Lice", 'Scabies',
      "Fungal Infection", "Leg Rash", 'Impetigo'),
  'Substance Abuse Issues' =
    c('Alcohol Intoxication', 'Alcohol Problem', 'Withdrawal',
      'Drug Overdose', 'Drug / Alcohol Dependency', 'Addiction Problem',
      'Addiction Assessment', 'Delirium Tremens (DTS)'),
  'Upper Respiratory Symptoms' =
    c('Congestion', 'Cough', 'Coughing Up Blood', 
      'Flu Symptoms', 'Enlarged Tonsils', 'Peritonsillar Abscess',
      'Nasal Congestion', 'Sinus Symptoms', 'Sinusitis',
      'Sore Throat', 'Hoarseness',
      'Throat Problem', 'Upper Respiratory Infection', 
      'Influenza', 'Laryngitis',
      'Respiratory Arrest', 'Pneumonia', 'Pleural Effusion', 'Asthma',
      'Croup', 'URI', 'Peritonsillar Abscess'),
  'Pregnancy Related' = 
    c("Pregnancy Problem", "Miscarriage", "Contractions", 
      'Ectopic Pregnancy',
      'Laboring',"Possible Pregnancy", "Pregnancy Related"), 
  'Renal' = 
    c('Av Fistula', 'Kidney Transplant', 'Elevated Serum Creatinine', 
      'End-Stage Liver Disease', "Hemodialysis Access", 'Nephritis',
      'Ureteral Stent Exchange'),
  'Urinary Complaints' =
    c('Bladder Problem', 'Blood in Urine', 'Cystitis',
      'Difficulty Urinating',
      'Dysuria', 'Gross Hematuria', 'Painful Urination',
      'Urinary Frequency', 'Urinary Symptom',
      'Urinary Incontinence', 'Urinary Problem', 
      'Urinary Retention', 'Slowing Urinary Stream',
      'Urinary Tract Infection', 'Urinary Urgency', 
      'Voiding Dysfunction',"Hesitancy Urinary")
)


for (i in seq(1,length(complaints))){
  name <- names(complaints[i])
  complaint <- complaints[[i]]
  
  df$CHIEF_COMPLAINT <- ifelse(
    df$CHIEF_COMPLAINT %in% complaint, name, df$CHIEF_COMPLAINT
  )
}

df <- df %>%
  mutate(complaint_esi  = paste(ESI, CHIEF_COMPLAINT),
         complaint_esi = factor(complaint_esi))

#=========================================================================
# Categorize Vital Signs -------------------------------------------------
#=========================================================================

df$tachycardic <- ifelse(
  is.na(df$TRIAGE_PULSE) == F & df$TRIAGE_PULSE > 100, 1, 0
)
df$tachypneic <- ifelse(
  is.na(df$TRIAGE_RR)  == F  & df$TRIAGE_RR > 20, 1, 0
)
df$febrile <- ifelse(
  is.na(df$TRIAGE_TEMP)  == F  & df$TRIAGE_TEMP > 38, 1, 0
)
df$hypotensive <- ifelse(
  is.na(df$TRIAGE_SBP)  == F  & df$TRIAGE_SBP < 90, 1, 0
)

#=========================================================================
# Create Time FE ---------------------------------------------------------
#=========================================================================

df <- df %>%
  separate(ARRIVAL_DTTM_REL, c("hours", "minutes"), sep = ":") %>%
  mutate(rel_minutes_arrival = as.numeric(hours)*60 + as.numeric(minutes),
         rel_minutes_depart = rel_minutes_arrival + ED_LOS)


df$rel_minutes_depart <- df$rel_minutes_depart - min(df$rel_minutes_arrival)
df$rel_minutes_arrival <- df$rel_minutes_arrival - min(df$rel_minutes_arrival)



# Calculate the number of patients in the hospital at the time of each patient's arrival
df$patients_in_hospital <- sapply(df$rel_minutes_arrival, function(arrival_time) {
  sum(df$rel_minutes_arrival <= arrival_time & df$rel_minutes_depart > arrival_time) - 1
})

# Calculate the time of the first test order for each patient
df$first_test_order_time <- apply(df[, c("US_ORDER_DTTM",
                                         "NON_CON_CT_ORDER_DTTM",
                                         "CON_CT_ORDER_DTTM",
                                         "LAB_ORDER_DTTM", 
                                         "XR_ORDER_DTTM")], 1, function(x) {
  min(x, na.rm = TRUE)
})

# Calculate the number of patients in the hospital at the time of each patient's first test order
df$patients_in_hospital_at_test <- sapply(df$first_test_order_time, function(test_time) {
  sum(df$rel_minutes_arrival <= test_time & df$rel_minutes_depart > test_time) - 1
})

# time FE
df$rel.hours <- as.numeric(df$hours)
df$rel.hours <- df$rel.hours - min(df$rel.hours)

# Create month of the year variable
df$month <- cut((df$rel.hours %/% (30*24)) %% 12 + 1,
                breaks = c(0, 1:12))

# Create day of the week variable
df$day_of_week <- cut((df$rel.hours %/% (24*7)) %% 7 + 1,
                      breaks = c(0, 1:7))

# Create hour of the day variable
df$hour <- cut(df$rel.hours %% 24 + 1,
               breaks = c(0, 1:24))

df$dayofweekt <- paste(df$day_of_week, df$hour)

df <- df %>%
  separate(TRIAGE_COMPLETED_REL, 
           c("triage_hours", "triage_minutes"), sep = ":") %>%
  mutate(rel_minutes_triage =
           (as.numeric(triage_hours)*60 + 
              as.numeric(triage_minutes)) - 
           rel_minutes_arrival)


#=========================================================================
# Create Final Dataset ---------------------------------------------------
#=========================================================================

df$ln_ED_LOS <- log(df$ED_LOS)

final <- df %>% 
  drop_na(ESI, CHIEF_COMPLAINT, ED_PROVIDER, ESI, nEDTests, ARRIVAL_AGE_DI) %>%
  select(ESI,  CHIEF_COMPLAINT, EXT_ID, US_PERF, NON_CON_CT_PERF, CON_CT_PERF, LAB_PERF,
         ARRIVAL_AGE_DI, ED_LOS, ln_ED_LOS, ED_PROVIDER, nEDTests, hours, 
         RTN_72_HR, RTN_72_HR_ADMIT, race, XR_PERF, GENDER, ED_DISPOSITION,
         tachycardic, tachypneic, febrile, hypotensive, rel.hours, any.batch,
         rel_minutes_triage, lab_image_batch, image_image_batch, imaging,
         complaint_esi, dayofweekt, month,
         patients_in_hospital, patients_in_hospital_at_test, first_test_order_time) %>%
  mutate(RTN_72_HR = ifelse(RTN_72_HR == 'Y', 1, 0),
         RTN_72_HR_ADMIT = ifelse(RTN_72_HR_ADMIT == 'Y', 1, 0)) 

final$age_groups <- cut(final$ARRIVAL_AGE_DI, 
                        c(-Inf, 20, 45, 65, Inf),
                        labels = c("<20", "20-45", "45-65", "65+"))

final$admit = ifelse(final$ED_DISPOSITION == 'Admit', 1, 0)
final$discharge = ifelse(final$ED_DISPOSITION == 'Discharge', 1, 0)
final$observation = ifelse(final$ED_DISPOSITION == 'Observation', 1, 0)

final <- final %>%
  mutate(dispo = case_when(
    admit == 1 ~ 'admit',
    discharge == 1 ~ 'discharge',
    observation == 1 ~ 'observeration',
    TRUE ~ 'other')) 

# Get Leave-one-out avg tests performed
final <- final %>%
  group_by(ED_PROVIDER) %>%
  mutate(total_tests = sum(nEDTests),
         n = n()) %>%
  ungroup() %>%
  mutate(avg_nEDTests = (total_tests - nEDTests)/ (n-1))

# Limit dataset to only physicians that had more than 520 encounters
provider_counts <- table(final$ED_PROVIDER)
providers_less_than_500 <- names(provider_counts[provider_counts < 520])
final <- final[!(final$ED_PROVIDER %in% providers_less_than_500), ]
final$complaint_esi <- paste(final$CHIEF_COMPLAINT, final$ESI)

# Limit to prevalent complaints only
final <- final %>%
  group_by(CHIEF_COMPLAINT) %>%
  filter(n() > 1000)


# write code to determine the standardized batching rate for each provider
batch_rates <- final %>% 
  group_by(ED_PROVIDER) %>%
  summarise(batch_rate = mean(any.batch, na.rm=T)) %>%
  ungroup() %>%
  mutate(z_batch_rate = as.vector(scale(batch_rate)),
         batcher = ifelse(z_batch_rate > 0, 'Batcher', 'Non-Batcher'))

final <- merge(final, batch_rates, on=c('ED_PROVIER'))

rm(list = setdiff(ls(), "final"))
#=========================================================================
##########################################################################
#=========================================================================
# IV Construction --------------------------------------------------------
#=========================================================================
##########################################################################

# Step 1: leave-out residualize at the ED encounter level
## conditional on shift-level variation, random assignment
## residual from regression represents physician tendency to batch
final$residual_batch <- resid(
  felm(any.batch ~ 0 | dayofweekt + month, data=final))

final$residual_batch_li <- resid(
  felm(lab_image_batch ~ 0 | dayofweekt + month, data=final))

final$residual_batch_ii <- resid(
  felm(image_image_batch ~ 0 | dayofweekt + month, data=final))

# Step 2: get batch tendency for each provider
final <- final %>%
  group_by(ED_PROVIDER) %>%
  mutate(Sum_Resid=sum(residual_batch, na.rm=T),
         batch.tendency = (Sum_Resid - residual_batch) / (n() - 1),
         
         Sum_Resid_li=sum(residual_batch_li, na.rm=T),
         batch.tendency_li = (Sum_Resid_li - residual_batch_li) / (n() - 1),
         
         Sum_Resid_ii=sum(residual_batch_ii, na.rm=T),
         batch.tendency_ii = (Sum_Resid_ii - residual_batch_ii) / (n() - 1)) %>% 
  ungroup()

write.csv(final, 'final.csv')
#=========================================================================