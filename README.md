# Introduction

Follow-up studies provide much more comprehensive, insightful understanding in the field of medical research than cross-sectional observations do. However, modeling longitudinal data can be demanding. Missing data occur rather often due to migration, death, or termination of practice for a variety of reasons. The second challenge arises from the temporal correlations among data points, which already violate the assumption of independence in most conventional statistical models.

We 

# Data Preparation

The dataset contains five entries for each of the $60$ patients who underwent protheses surgeries, and eight variables are observed over five years. Not surprisingly, there are $24$ missing observations that impede further analysis, and We start by substituting these NA values with estimations. Generally speaking, our best guess is the average of adjacent observations; in the scenarios that an adjacent observation is also missing, we fetch further until some value is available. If what is missing happens to be the last observation of a patient, only fetch backwards.[1](https://github.com/PawinData/Prostheses/blob/master/functions.R) It goes without saying that the first observation of a patient, marking his or her entry into this study, cannot be missing.

The two variables, `FU.Years` and `FU.Months` give exactly the same information, so we only retain one of them. Also, `Age` is apparently problematic as for some patients, it is even decreasing occasionally. We then decide to group $60$ patients by `Sex` and `Type` of prostheses, which are very unlikely to change during the study, and look into `nMTPM` and `BMI`.

# Exploratory Data Analysis

![EDA_MTPM](https://user-images.githubusercontent.com/59799041/80847545-629c5780-8c10-11ea-86cb-e437ce3f9e1f.png)
![EDA_BMI](https://user-images.githubusercontent.com/59799041/80847651-bc048680-8c10-11ea-8d59-510f86ad824b.png)

It seems that in the plot of how MTPM develops over time, red points are distributed above blue points, overall, and dashed lines above solid lines. Do female suffer from more serious displacement of prostheses or recover from the surgery slower? Are Type-1 prostheses betther than Type-2? These questions, brought up in exploratory data analysis, will be carefully investigated.

BMI, on the other hand, displays little of interest except that values of female BMI fluctuate more heavily than those of male. And we verify that the two females who have the lowest BMI are not the ones whose MTPM stand out on the top. Given that every line chart of BMI is fairly flat, we determine that `BMI` is not involved with the dynamics of `nMTPM` but could serves as the background of personal health. 

We also have an [interactive 3D visualization](https://github.com/PawinData/Prostheses/blob/master/3D.html) of the data.

