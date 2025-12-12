Overview
- prepare_data.R: Loads NHANES data, defines the hypertension cohort, and selects survey, demographic, clinical, medication, and outcome variables.
- import_transform_data.R: Imports the selected variables, converts categorical variables to ordered/unordered factors, creates the NHANES survey design object, generates a weighted BP control trend plot, and fits preliminary logistic regression models for testing.

Run or refer to *import_transform_data.R* before performing EDA or modeling.
