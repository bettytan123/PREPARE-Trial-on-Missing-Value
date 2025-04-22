/***********************************************
************************************************
Complete Case PREPARE Trial
************************************************
************************************************/
/*The follownig script is created to document the cleaning process and analysis process on 
the whodas_itt_oct_03.sas7bdat */

/* load the input data*/
libname dat 'C:\Users\mtaljaard\OneDrive - The Ottawa Hospital\Documents\Consulting\McIsaac Dan\Prepare\data from Dan';
/*libname dat 'C:\Users\yutchen\OneDrive - The Ottawa Hospital\Documents\PREPARE';*/

proc format;
    value discharge
        0 = 'Died in hospital'
        1 = 'Discharge to long-term care facility'
        2 = 'Discharge to rehabilitation / still in hospital'
        3 = 'Discharge home with support'
        4 = 'Discharge home independent'
        9 = 'Not applicable';
value cfs 4='Vulnerable'
          5='Mildly frail'
		  6='Moderately frail'
          7='Severely Frail'  
		  8='Very severely frail'
		  9='Terminally ill';
run;

 
/**************
1-DATA CLEANING
***************/

/*1.1 Data cleaning and variable creation*/
proc sort data=dat.whodas_itt_oct_03;
by pid;
proc sort data=dat.surgery_status_oct25;
by pid;
run;

data whodas_clean;
  merge dat.whodas_itt_oct_03 dat.surgery_status_oct25;
  by pid;
   /*handle 999 data issue*/
   array vars _numeric_; /* creates an array for all numeric variables */
   /* loop through each variable in the array and recode 999 to missing */
   do i = 1 to dim(vars);
      if vars[i] = 999 then vars[i] = .;
   end;
   drop i; /* drop the temporary index variable */

   /*creat variables for subgroup analysis*/
   *create binary indicator for age 75; 
    if age_num >= 75 then age_75 = 1; /*more than 75*/
    else age_75 = 0; /*less than75*/

   *create binary indicator for presence of depression;
    if PHQ_score >= 3 then depression = 1; /* Positive screen for depression */
    else depression = 0; /* Negative screen for depression */

	*create the new binary variable 'frailty_status';
    if cfs_score_base = 4 then frailty_status = 0;
    else if cfs_score_base >= 5 then frailty_status = 1;



label PHQ_score='Personal Health Questionnaire baseline'
        CHF='Congestive Heart Failure baseline'
        CTD='Connective Tissue Disease baseline'
        CVA='Stroke/mini-stroke baseline'
        MI='Heart attack baseline'
        PID='Patient id'
        PUD='Peptic ulcer disease baseline'
        PVD ='Peripheral Vascular disease baseline'
        any_complication = 'Co-primary outcome POMS composite'
        anyemerg_cat = 'gone to ED since enrolment'
        anyhead_cat='any head injuries since enrolment'
        anyhosp_cat='any hosp admission since enrolment'
        anyinjury_cat='any injury since enrolment'
        cfs_score_base = 'Frailty score baseline'
        dasi = 'Duke Activity Status Index baseline'
        days_pre='Days from enrolment to surgery'
        discharge_ord='Discharge location'
        eq_value_30d = 'Health related QoL 30 days'
        eq_vas_30d = 'Health related QoL Visual analog scale 30 days'
		frailty_status='CFS>=5';
format discharge_ord discharge. ; 
run;

proc contents data=whodas_clean;
run;  


/*1.2 load poms_mitt dataset for add varible pain_flag to whodas_itt*/
data poms_mitt;
  set dat.poms_mitt_oct_03;
run;

proc sql;
    create table whodas_with_pain_flag as
    select 
        a.*,                 /* Select all columns from table_bcd */
        b.pain_flag          /* Select the specific column from table_abc */
    from 
        whodas_clean as a
    left join 
        poms_mitt as b
    on 
        a.PID = b.PID;          /* Merge using the common column 'id' */
quit;

/*1.3 add randomization allocation to whodas_itt*/
/* load randomization data*/
proc import datafile="C:\Users\mtaljaard\OneDrive - The Ottawa Hospital\Documents\Consulting\McIsaac Dan\Prepare\data from Dan\PREPARE Allocation Data.xlsx"
    out=work.randomdata  /* Name of the SAS dataset */
    dbms=xlsx        /* Specify the Excel file type */
    replace;         /* Replace the dataset if it exists */
    sheet="Eligibility";  /* Name of the Excel sheet to import */
    getnames=yes;    /* Use the first row as variable names */
run;

/*proc import datafile="C:\Users\yutchen\OneDrive - The Ottawa Hospital\Documents\PREPARE\PREPARE Allocation Data.xlsx"
    out=work.randomdata  /* Name of the SAS dataset */
  *  dbms=xlsx        /* Specify the Excel file type */
  *  replace;         /* Replace the dataset if it exists */
  *  sheet="Eligibility";  /* Name of the Excel sheet to import */
  *  getnames=yes;    /* Use the first row as variable names */
/*run;*/

/* add treatment allocation to whodas_itt*/
proc sql;
    create table whodas_with_randomization as
    select 
        a.*,                 /* Select all columns from table_bcd */
        b.*          /* Select all column from table_abc */
    from 
        whodas_with_pain_flag as a
    left join 
        randomdata as b
    on 
        a.PID = b.RandomID;          /* Merge using the common column 'id' */
quit;




/***********************************************************************
2-DATA QUALITY CHECK: prevalence of missing data and out of range values; 
************************************************************************/

/*2.1 continuous variables*/
*Note: Avg_steps has many missing - infeasible to analyze as an outcome - DO NOT IMPUTE;
*********why does days_pre have only 13 missing;
proc means data=whodas_with_randomization N Nmiss ;
/*baseline variables*/
var  age_num dasi eq_value_base eq_vas_base  sts_time_base whodas_100_baseline  
/*outcomes*/
	avg_steps death_days eq_value_30d eq_vas_30d sts_time_DC whodas_100_30d  
/*surgery-related variables*/
    days_pre los;
title "Summary Statistics for All Continuous Variables";
run;


/* 2.11 Create histograms for all continuous variables using PROC UNIVARIATE */
*sts_time_base and sts_time_dc have odd distribution;
*death_days cannot be used for imputation - almost no variation; 
proc univariate data=whodas_with_randomization ;
   var age_num dasi eq_value_base eq_vas_base  sts_time_base whodas_100_baseline  
	avg_steps death_days eq_value_30d eq_vas_30d sts_time_DC whodas_100_30d  
    days_pre los;;
   histogram / normal;
   title "Histograms for All Continuous Variables";
run;

/*2.2 binary variables*/
proc freq data=whodas_with_randomization ;
*hemiplegia and HIV are  very sparse - ignore it for imputation;
tables 
/*demographics*/
   center
   Sex cfs_score_base CHF COPD CPAP CTD CVA Cancer Chemotherapy Diabetes /*HIV*/ Hearing  /*Hemiplegia*/  Insulin Kidney Liver MI Metastases  
         OSA PHQ_score PUD PVD  Radiation  Rheumatologic  Visual dementia malnutrition_risk smoker katz_base surgery_status 
/*surgery related variables*/
   ProcedureCategory any_complication poms_severity discharge_ord pain_flag 
      /*components of the POMS*/
      Pulmonary Infection Gastrointestinal Renal Cardiovascular Wounds Hematological Neurological 
/*secondary outcomes*/
   katz_dc readmission 
/*safety outcomes*/
   anyemerg_cat anyhead_cat anyhosp_cat anyinjury_cat    post_fall pre_fall 
/*censoring indicators*/
   censor_los  death_censor 
/missing;
 title "Frequency Distribution for Categorical Variables";
run;


*these variables are measured only for patients who had surgery - they should not be imputed;
/*any_complication poms_severity discharge_ord pain_flag  Pulmonary Infection Gastrointestinal Renal Cardiovascular Wounds Hematological Neurological*/ 
title;
proc print;
var pid days_pre surgery_status
/*surgery related variables*/
   ProcedureCategory any_complication poms_severity discharge_ord pain_flag 
      /*components of the POMS*/
      Pulmonary Infection Gastrointestinal Renal Cardiovascular Wounds Hematological Neurological 
/*secondary outcomes*/
   katz_dc readmission 
/*censoring indicators*/
   censor_los ; 
where surgery_status=0;
run;
proc freq;
tables surgery_status*censor_los;
run;
proc freq data=dat.myfinaldata;
tables surgery_status*(sts_time_dc discharge_ord readmission katz_dc)/missing;
run;

proc means data=dat.myfinaldata nmiss;
run;

proc means data=dat.myfinaldata n;
run;





/***************************
3-CREATE FINAL DATA : MYFINALDATA  
****************************/

/*3.1 create a permanent SAS datafile after recoding and cleaning;*/
*create Not applicable categories for variables only defined for those who had surgery;
*these variables should be treated as nominal in the imputation model;
data dat.myfinaldata;
    set whodas_with_randomization;
/*give 9 as patient who do not take surgery*/
if surgery_status=0 then do;
   any_complication =9;
   poms_severity =9;
   discharge_ord =9;
   pain_flag =9;
   los=0;
end;

*recode the allocation variable from text to binary;
if allocation='Prehab Program' then my_allocation =1;
else if allocation='Standard Care' then my_allocation=0;

*recode sex from text to binary;
if sex='Male' then mysex=0;
else if sex='Female' then mysex=1;

*create interaction terms for the multiple imputation model;
age_int=age_75*my_allocation;
depress_int=depression*my_allocation;
frail_int=frailty_status*my_allocation;
sex_int=mysex*my_allocation;
cancer_int=cancer*my_allocation;

run;

/*3.2 quick data check for what variable is missing*/
*now identify all variables with missing values that need to be imputed;
proc means data=dat.myfinaldata nmiss n;
   var _numeric_; /* Change _numeric_ to _all_ if you want to include both numeric and character variables */
   output out=miss_summary (drop=_type_ _freq_);
run;
*List of variables with missing values and to be imputed in multiple imputation model;
       /*PHQ_score              
       eq_vas_base            
       eq_value_base          
       sts_time_base          
       whodas_100_baseline    
       whodas_100_30d         
       eq_vas_30d             
       eq_value_30d           
       sts_time_DC            
       los                  
       discharge_ord        
       readmission          
       pre_fall             
       post_fall            
       anyinjury_cat        
       anyhosp_cat          
       anyemerg_cat         
       anyhead_cat          
       poms_severity                                                              
       days_pre*/               

*what is percentage of missingness on these variables;
proc freq data=dat.myfinaldata;
tables   PHQ_score eq_vas_base eq_value_base sts_time_base whodas_100_baseline whodas_100_30d eq_vas_30d eq_value_30d           
       sts_time_DC los discharge_ord readmission pre_fall post_fall anyinjury_cat anyhosp_cat anyemerg_cat anyhead_cat          
       poms_severity days_pre/missing;
run;
proc freq data=dat.myfinaldata; 
tables any_complication pain_flag/missing;
run;

*prevalence for the variables we were unable to impute;
proc freq data=dat.myfinaldata;
tables anyinjury_cat anyhosp_cat anyhead_cat;
run;
 
options orientation=landscape;
ods rtf file='C:\Users\mtaljaard\OneDrive - The Ottawa Hospital\Documents\Consulting\McIsaac Dan\Prepare\results\missingpattern.rtf' style=minimal;
/*ods rtf file='C:\Users\yutchen\OneDrive - The Ottawa Hospital\Documents\PREPARE\results\missingpattern.rtf' style=minimal;*/


/*3.3 misspattern table of the missing data patterns present in whodas*/
proc mi data=dat.myfinaldata nimpute=0 ;
var  whodas_100_30d  poms_severity eq_vas_30d eq_value_30d sts_time_DC los discharge_ord 
     readmission pre_fall post_fall anyinjury_cat anyhosp_cat anyemerg_cat anyhead_cat 
whodas_100_baseline eq_vas_base eq_value_base PHQ_score sts_time_base days_pre;
ods select misspattern;
run;
ods rtf close;

proc freq data=dat.myfinaldata;
tables poms_severity*surgery_status/missing;
run; 





/* *************************
4-PROC MI MODEL 
***************************/

/* 4.1 MI using fully conditional specificat*/
proc mi data= dat.myfinaldata nimpute=3 out=dat.mi_fcs11 seed =1000;
class 
    /*allocation*/ center
    age_75 /*Sex*/ ProcedureCategory
    katz_base cfs_score_base PHQ_score
    discharge_ord katz_dc any_complication poms_severity 
    pain_flag post_fall pre_fall readmission anyemerg_cat ;
/*fcs plots=trace(mean std); */
var 
   center my_allocation ProcedureCategory
   age_num age_75 mySex smoker
   COPD CPAP CVA Cancer Chemotherapy Diabetes Insulin Kidney Liver MI Metastases OSA PVD Radiation Rheumatologic dementia malnutrition_risk 
   dasi PHQ_score eq_value_base eq_vas_base whodas_100_baseline sts_time_base cfs_score_base katz_base
   days_pre death_days
   eq_value_30d eq_vas_30d whodas_100_30d los discharge_ord sts_time_DC katz_dc
   any_complication poms_severity
   death_censor pain_flag post_fall pre_fall readmission anyemerg_cat 
   depression frailty_status
   age_int depress_int frail_int sex_int cancer_int
;
fcs nbiter =40
/*binary or ordinal variables */
logistic (post_fall pre_fall readmission anyemerg_cat PHQ_score /link= logit likelihood= augment order= data details)
/*multinomial*/
logistic (discharge_ord poms_severity / likelihood= augment link = glogit details) 
/*linear*/
reg(eq_value_30d eq_value_base eq_vas_30d eq_vas_base whodas_100_30d whodas_100_baseline /details)
/*linear with predictive mean matching*/ 
regpmm (sts_time_DC sts_time_base los days_pre/details);
*fcs plots=trace(mean std);
run;




/* **************************************
5- CREATE MI DATA FOR Table 2 MI ANLAYSIS
*****************************************/

/*5.1 merge 30 impultation together into one file for mi analysis later*/

data dataset1;
    set dat.mi_fcs1;
	run;

proc contents data=dataset1;
run;


proc freq data=dataset1;
tables _Imputation_;
run;

data dataset2;
    set dat.mi_fcs2;
    _Imputation_ = _Imputation_ + 3;
run;

proc freq data=dataset2;
tables _Imputation_;
run;


data dataset3;
    set dat.mi_fcs3;
    _Imputation_ = _Imputation_ + 6;
run;


data dataset4;
    set dat.mi_fcs4;
    _Imputation_ = _Imputation_ + 9;
run;


data dataset5;
    set dat.mi_fcs5;
    _Imputation_ = _Imputation_ + 12;
run;


data dataset6;
    set dat.mi_fcs6;
    _Imputation_ = _Imputation_ + 15;
run;

data dataset7;
    set dat.mi_fcs7;
    _Imputation_ = _Imputation_ + 18;
run;

data dataset8;
    set dat.mi_fcs8;
    _Imputation_ = _Imputation_ + 21;
run;


data dataset9;
    set dat.mi_fcs9;
    _Imputation_ = _Imputation_ + 24;
run;

data dataset10;
    set dat.mi_fcs11;
    _Imputation_ = _Imputation_ + 27;
run;


proc freq data=dataset10;
    tables _Imputation_ ;
run;


/*5.2 save permanent in file for final table for mi analysis*/
data dat.myfinaldatami;
    set dataset1  
        dataset2
        dataset3
		dataset4
        dataset5
        dataset6
        dataset7
        dataset8
        dataset9
        dataset10;
	run;
        
   
proc contents data=dat.myfinaldatami;
run;


/* ***************************************
6- COMPLETE DATA ANALYSIS -- FULL ANALYSIS POPULATION
******************************************/

/* 6.1 Primary and secondary outcome analysis results using Complete Case analysis */
/* STATISTICS control mean */
data control_subset;
    set dat.myfinaldata; 
    if allocation = 'Standard Care';
run;


proc means data=control_subset mean std;
    var whodas_100_30d; 
run;

proc means data=control_subset mean std;
var sts_time_dc;
run;

proc freq data=control_subset; 
    tables poms_severity;
run;

/* intervention mean */
data intervention_subset;
    set dat.myfinaldata; 
    if allocation = 'Prehab Program';
run;

proc means data=intervention_subset mean std;
    var whodas_100_30d; 
run;

proc means data=intervention_subset mean std;
 var sts_time_dc;
 run;


proc freq data=intervention_subset; 
    tables poms_severity;
run;


/*6.2 continuous outcomes*/
*mean diff whodas;
proc mixed data=dat.myfinaldata method=REML;
class ProcedureCategory (ref='Urology-') center ;
model whodas_100_30d = my_allocation whodas_100_baseline ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution;
random intercept / subject= center;
ods output SolutionF=mytest;
estimate 'treatment effect' my_allocation 1 /cl alpha=0.025;
title 'Complete Case Analysis whodas';
run;
*eq_value;
proc mixed data=dat.myfinaldata method=REML;
class ProcedureCategory (ref='Urology-') center ;
model eq_value_30d = my_allocation eq_value_base ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution;
random intercept / subject= center;
ods output SolutionF=mytest;
estimate 'treatment effect' my_allocation 1 /cl alpha=0.05;
title 'Complete Case Analysis eq value';
run;

*eq_vas;
proc mixed data=dat.myfinaldata method=REML;
class ProcedureCategory (ref='Urology-') center ;
model eq_vas_30d = my_allocation eq_vas_base ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution;
random intercept / subject= center;
ods output SolutionF=mytest;
estimate 'treatment effect' my_allocation 1 /cl alpha=0.05;
title 'Complete Case Analysis eq vas';
run;
*sts;
proc mixed data=dat.myfinaldata method=REML;
class ProcedureCategory (ref='Urology-') center ;
model sts_time_dc = my_allocation sts_time_base ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution;
random intercept / subject= center;
ods output SolutionF=mytest;
estimate 'treatment effect' my_allocation 1 /cl alpha=0.05;
title 'Complete Case Analysis sts';
run;



/* 6.3 binary logistic varible: odds ratio*/
/* poms primary outcome */
proc freq data=control_subset; 
   where surgery_status=1;
   tables any_complication;
run;

proc freq data= intervention_subset;
where surgery_status=1;
table any_complication;
run;


proc glimmix data=dat.myfinaldata method=quad;
class ProcedureCategory (ref='Urology-') center ;
model any_complication(event='1') = my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=binary link=logit;
random intercept / subject= center;
estimate 'treatment effect' my_allocation 1 /exp cl alpha=0.025;
title 'Complete Case Analysis';
where surgery_status=1;
run;



/*readmission*/
proc freq data=control_subset; 
   where surgery_status=1;
   tables readmission;
run;

proc freq data= intervention_subset;
where surgery_status=1;
table readmission;
run;



proc glimmix data=dat.myfinaldata method=quad;
class ProcedureCategory (ref='Urology-') center ;
model readmission(event='1') = my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=binary link=logit;
random intercept / subject= center;
estimate 'treatment effect' my_allocation 1 /exp cl alpha=0.05;
title 'Complete Case Analysis';
where surgery_status=1;
run;


/* post falls*/
proc freq data=control_subset; 
where surgery_status=1;
tables post_fall;
run;

proc freq data= intervention_subset;
where surgery_status=1;
table post_fall;
run;

proc glimmix data=dat.myfinaldata method=quad;
class ProcedureCategory (ref='Urology-') center ;
model post_fall(event='1') = my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=binary link=logit;
random intercept / subject= center;
estimate 'treatment effect' my_allocation 1 /exp cl alpha=0.05;
title 'Complete Case Analysis';
where surgery_status=1;
run;




/*6.4 odd ratios for ordinal logisitc analysis */
/*katz*/
proc freq data=control_subset; 
   tables katz_dc;
run;

proc freq data= intervention_subset;
table katz_dc;
run;


proc glimmix data=dat.myfinaldata method=quad;
class ProcedureCategory (ref='Urology-') center ;
model katz_dc (order=internal)= my_allocation katz_base ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=multinomial link=clogit ;
random intercept / subject= center;
ods output SolutionF=mytest;
estimate 'treatment effect' my_allocation 1 /exp cl alpha=0.05;
title 'Complete Case Analysis 1';
where surgery_status=1;
run;


proc glimmix data=dat.myfinaldata method=quad;
class ProcedureCategory (ref='Urology-') center ;
model katz_dc (order=internal)= my_allocation katz_base ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=multinomial link=clogit ;
*random intercept / subject= center;
ods output SolutionF=mytest;
estimate 'treatment effect' my_allocation 1 /exp cl alpha=0.05;
title 'Complete Case Analysis 2';
where surgery_status=1;
run;


/*discharge disposition*/
proc freq data=control_subset; 
   where surgery_status=1;
   tables discharge_ord;
run;

proc freq data= intervention_subset;
where surgery_status=1;
table discharge_ord;
run;



/*Complication severity*/
proc freq data=control_subset; 
   where surgery_status=1;
   tables poms_severity;
run;

proc freq data= intervention_subset;
where surgery_status=1;
table poms_severity;
run;



/*lower grade in poms severity better result */
proc glimmix data=dat.myfinaldata /*method=REML*/ method=quad;
class ProcedureCategory (ref='Urology-') center ;
model poms_severity (descending order=internal)= my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=multinomial link=clogit ;
random intercept / subject= center;
ods output SolutionF=mytest;
estimate 'treatment effect' my_allocation 1 /exp cl alpha=0.05;
title 'Complete Case Analysis';
where surgery_status=1;
run;




/*6.5 hazaed ratio for death related variables*/
/*mortality death_censor*/
proc freq data=control_subset; 
    tables death_censor;
run;

proc freq data= intervention_subset;
 table death_censor;
run;



proc freq data=dat.myfinaldata;
   table my_allocation*death_censor;
run;

proc phreg data=dat.myfinaldata covs(aggregate);
   class  center ProcedureCategory (ref='Urology-');
   model death_days*death_censor(0)=my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer ;
   random center /solution;
   hazardratio 'Marginal Model Analysis' my_allocation;
   estimate 'treatment effect' my_allocation 1 / exp cl alpha=0.05;
run;




/* los*/
proc means data=control_subset mean std; 
    var los;
run;

proc means data= intervention_subset mean std;
 var los;
run;


proc phreg data=dat.myfinaldata plots(overlay=stratum)=cif;
class center ProcedureCategory (ref='Urology-');
model los*censor_los(0)= my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer  / eventcode(FG (BRESLOW))=1;
hazardratio 'Marginal Model Analysis' my_allocation;
estimate 'treatment effect' my_allocation 1 / exp cl alpha=0.05;
run;

