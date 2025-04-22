/***********************************************
************************************************
Per Protocol Analysis Case PREPARE Trial
************************************************
************************************************/
/*The following script is using the myfinaldatami dataset or myfinaldata dataset created earlier*/
*1. Depends on if the outcome we are interested in got missing value or not, we use proc mianalyze 
(for outcome got missing value) or simple proc mixed/glimmix for do not have missing */
*2. For this ITT ANALYSIS POPULATION, we limit the populations to:
-intervention group participants with adherence >75% and who has surgery
-control group participants who had surgery*/



/*load the libname */
/*libname dat 'C:\Users\mtaljaard\OneDrive - The Ottawa Hospital\Documents\Consulting\McIsaac Dan\Prepare\data from Dan';
libname dat 'C:\Users\yutchen\OneDrive - The Ottawa Hospital\Documents\PREPARE';

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


/*******************
1-ITT DATA ANAlYSIS
********************/

/* 1.1 load adherence data*/
proc import datafile="C:\Users\yutchen\OneDrive - The Ottawa Hospital\Documents\PREPARE\2024.07.17 - Overall Adherence.xlsx"
    out=work.adherencedata  /* Name of the SAS dataset */
    dbms=xlsx        /* Specify the Excel file type */
    replace;         /* Replace the dataset if it exists */
    sheet="Sheet1";  /* Name of the Excel sheet to import */
    getnames=yes;    /* Use the first row as variable names */
run;




/*1.2 <case 1> outcome with NO missing*/
/*1.21 Merge the adherence data from bcc into myfinaldata, setting adherence=0 for unmatched IDs */
proc sort data=work.adherencedata;
by PID;
run;
proc sort data=dat.myfinaldata;
by PID;
run;
data dat.merged_dataitt_withcompletecase;
   merge dat.myfinaldata(in=in_data)
         work.adherencedata (keep=PID adh4weeks);
   by PID;
   if not in_data then delete; /* Retain only records from datami */
   if missing(adh4weeks) then adh4weeks = 0; /* Set adherence to 0 if not found in adherencedata */
run;


/*data quality check*/
proc contents data=dat.merged_dataitt_withcompletecase;
run;
proc freq data=merged_dataitt_withcompletecase;
tables allocation my_allocation;
run;
proc freq data=merged_dataitt_withcompletecase;
tables allocation surgery_status adh4weeks;
run;

/*1.22 Add the per_protocol variable create and save the final set */
data dat.final_dataitt_withcompletecase;
   set dat.merged_dataitt_withcompletecase;
   adh4weeks_num = input(adh4weeks, best12.); /* Convert character to numeric */
   if surgery_status = 0 then per_protocol = 0;
   else if surgery_status = 1 then do;
      if my_allocation = 0 then per_protocol = 1; /* Control group with surgery */
      else if my_allocation = 1 and adh4weeks_num >= 75 then per_protocol = 1; /* Intervention group with adherence >= 0.75 */
      else per_protocol = 0; /* All other cases */
   end;
run;

/*need this sort for analysis */
proc sort data= dat.final_dataitt_withcompletecase;
by _imputation_;
run;

/*data quality check*/
proc freq data=dat.final_dataitt_withcompletecase;
tables surgery_status * adh4weeks_num;
run;

proc freq data =dat.final_dataitt_withcompletecase;
tables my_allocation surgery_status per_protocol adh4weeks adh4weeks_num;
run;



/*1.23 odd ratios for binary analysis*/
/*POMS */
Proc freq data=dat.final_dataitt_withcompletecase;
where per_protocol = 1;
table any_complication * my_allocation;
run;

proc glimmix data=dat.final_dataitt_withcompletecase method=quad;
class ProcedureCategory (ref='Urology-') center ;
model any_complication(event='1') = my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=binary link=logit;
random intercept / subject= center;
where per_protocol =1;
ods output ParameterEstimates=anycomp_glimmix;
estimate 'treatment effect' my_allocation 1 /cl exp alpha=0.025;
run;

proc freq data= dat.final_dataitt_withcompletecase;
tables any_complication;
run;


/*1.24 odd ratios for ordinal logisitc analysis*/
/*katz*/
Proc freq data=dat.final_dataitt_withcompletecase;
where per_protocol = 1;
table katz_dc * my_allocation;
Run;

proc glimmix data=dat.final_dataitt_withcompletecase method=quad;
class ProcedureCategory (ref='Urology-') center ;
model katz_dc(order=internal) = my_allocation ProcedureCategory katz_base mySex age_num frailty_status malnutrition_risk cancer/ solution dist=multinomial link=clogit;
random intercept / subject= center;
where per_protocol =1;
ods output ParameterEstimates=katz_glimmix;
estimate 'treatment effect' my_allocation 1 /cl exp alpha=0.05;
run;




/*1.25 hazaed ratio for death realted outcome*/
/*mortality death_censor*/
Proc freq data=dat.final_dataitt_withcompletecase;
where per_protocol = 1;
table death_censor * my_allocation;
Run;

/*no missing on death_censor and death_days-- use complete case*/
proc phreg data=dat.final_dataitt_withcompletecase covs(aggregate);
class ProcedureCategory (ref='Urology-') center ;
model death_days*death_censor(0)= my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer;
random center/ solution;
where per_protocol =1;
hazardratio 'Marginal Model Analysis' my_allocation;
ods output ParameterEstimates=death_phreg;
estimate 'treatment effect' my_allocation 1 /cl exp alpha=0.05;
run;


/*1.3 <case 2> outcome HAVE missing*/
/*1.31 Merge the adherence data into myfinaldatami, setting adherence=0 for unmatched IDs */
proc sort data=work.adherencedata;
by PID;
run;

data dat.merged_dataitt_withmicase;
   merge dat.myfinaldatami(in=in_data)
         work.adherencedata (keep=PID adh4weeks);
   by PID;
   if not in_data then delete; /* Retain only records from datami */
   if missing(adh4weeks) then adh4weeks = 0; /* Set adherence to 0 if not found in adherencedata */
run;


/*data quality check*/
proc contents data=dat.merged_dataitt_withmicase;
run;
proc freq data=merged_dataitt_withmicase;
tables allocation my_allocation;
run;
proc freq data=merged_dataitt_withmicase;
tables allocation surgery_status adh4weeks;
run;

/*1.32 Add the per_protocol variable create and save the final set */
data dat.final_dataitt_withmicase;
   set dat.merged_dataitt_withmicase;
   adh4weeks_num = input(adh4weeks, best12.); /* Convert character to numeric */
   if surgery_status = 0 then per_protocol = 0;
   else if surgery_status = 1 then do;
      if my_allocation = 0 then per_protocol = 1; /* Control group with surgery */
      else if my_allocation = 1 and adh4weeks_num >= 75 then per_protocol = 1; /* Intervention group with adherence >= 0.75 */
      else per_protocol = 0; /* All other cases */
   end;
run;

/*need this sort for analysis */
proc sort data= dat.final_dataitt_withmicase;
by _imputation_;
run;

/*data check*/
proc freq data=dat.final_dataitt_withmicase;
tables surgery_status * adh4weeks_num;
run;
proc freq data =dat.final_dataitt_withmicase;
tables my_allocation surgery_status per_protocol adh4weeks adh4weeks_num;
run;



/*1.33 continuous variables*/
/*whodas30*/
Proc means data=dat.final_dataitt_withmicase;
Class my_allocation;
Var whodas_100_30d;
where per_protocol = 1;
Run;


/*sts*/
Proc means data=dat.final_dataitt_withmicase;
Class my_allocation;
Var sts_time_dc;
where per_protocol = 1;
Run;


/*eq value*/
Proc means data=dat.final_dataitt_withmicase;
Class my_allocation;
Var eq_value_30d;
where per_protocol = 1;
Run;




/*eq vas*/
 Proc means data=dat.final_dataitt_withmicase;
Class my_allocation;
Var eq_vas_30d;
where per_protocol = 1;
Run;



/*1.331 continuous outcomes*/
*mean diff whodas;
proc mixed data=dat.final_dataitt_withmicase method=REML;
class ProcedureCategory (ref='Urology-') center ;
model whodas_100_30d = my_allocation whodas_100_baseline ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution;
random intercept / subject= center;
by _Imputation_;
where per_protocol =1; 
ods output SolutionF=whodas30_mixed;
estimate 'treatment effect' my_allocation 1 /cl alpha=0.025;
run;
proc print data=whodas30_mixed;
run;

proc mianalyze parms=whodas30_mixed alpha=0.025;
class ProcedureCategory cfs_score_base ;
modeleffects intercept my_allocation whodas_100_baseline ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer;
run;



*eq_value;
proc mixed data=dat.final_dataitt_withmicase method=REML;
class ProcedureCategory (ref='Urology-') center ;
model eq_value_30d = my_allocation eq_value_base ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution;
random intercept / subject= center;
by _Imputation_;
where per_protocol=1;
ods output SolutionF=eqvalue_mixed;
estimate 'treatment effect' my_allocation 1 /cl alpha=0.05;
run;
proc print data=eqvalue_mixed;
run;

proc mianalyze parms=eqvalue_mixed;
class ProcedureCategory cfs_score_base ;
modeleffects my_allocation ;
run;




*eq_vas;
proc mixed data=dat.final_dataitt_withmicase method=REML;
class ProcedureCategory (ref='Urology-') center ;
model eq_vas_30d = my_allocation eq_vas_base ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution;
random intercept / subject= center;
by _Imputation_;
where per_protocol = 1;
ods output SolutionF=eqvas_mixed;
estimate 'treatment effect' my_allocation 1 /cl alpha=0.05;
run;
proc print data=eqvas_mixed;
run;

proc mianalyze parms=eqvas_mixed;
class ProcedureCategory cfs_score_base ;
modeleffects my_allocation ;
run;




*sts;
proc mixed data=dat.final_dataitt_withmicase method=REML;
class ProcedureCategory (ref='Urology-') center ;
model sts_time_dc = my_allocation sts_time_base ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution;
random intercept / subject= center;
by _Imputation_;
where per_protocol=1;
ods output SolutionF=sts_mixed;
estimate 'treatment effect' my_allocation 1 /cl alpha=0.05;
run;
proc print data=sts_mixed;
run;

proc mianalyze parms=sts_mixed;
class ProcedureCategory cfs_score_base ;
modeleffects my_allocation ;
run;




/*1.34 odds ratio binary logistic varible*/
/*poms severity */
Proc freq data=dat.final_dataitt_withcompletecase;
where per_protocol = 1;
table any_complication * my_allocation;
Run;

/*becasue got no missing in using all obs in sas so we use compele case to generate the CI*/
proc glimmix data=dat.final_dataitt_withcompletecase method=quad;
class ProcedureCategory (ref='Urology-') center ;
model any_complication(event='1') = my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=binary link=logit;
random intercept / subject= center;
where per_protocol =1;
ods output ParameterEstimates=anycomp_glimmix;
estimate 'treatment effect' my_allocation 1 /cl exp alpha=0.025;
run;





/*readmission*/
Proc freq data=dat.final_dataitt_withmicase;
where per_protocol = 1;
table readmission * my_allocation;
Run;

proc glimmix data=dat.final_dataitt_withmicase method=quad;
class ProcedureCategory (ref='Urology-') center ;
model readmission(event='1') = my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=binary link=logit;
random intercept / subject= center;
where surgery_status=1 and per_protocol =1 ;
by _Imputation_;
ods output ParameterEstimates=readmission_glimmix;
estimate 'treatment effect' my_allocation 1 /cl exp alpha=0.05;
run;
proc print data=readmission_glimmix;
run;

/* need to exp the final reuslt to see odd ratio*/
proc mianalyze parms=readmission_glimmix;
class ProcedureCategory cfs_score_base ;
modeleffects my_allocation;
run;



/* post falls*/
Proc freq data=dat.final_dataitt_withmicase;
where per_protocol = 1;
table post_fall * my_allocation;
Run;


proc glimmix data=dat.final_dataitt_withmicase method=quad;
class ProcedureCategory (ref='Urology-') center ;
model post_fall(event='1') = my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=binary link=logit;
random intercept / subject= center;
where surgery_status=1 and per_protocol=1 ;
by _Imputation_;
ods output ParameterEstimates=postfall_glimmix;
estimate 'treatment effect' my_allocation 1 /cl exp alpha=0.05;
run;
proc print data=postfall_glimmix;
run;


/* need exp to get the final result */
proc mianalyze parms=postfall_glimmix;
class ProcedureCategory cfs_score_base ;
modeleffects my_allocation;
run;




/*discharge disposition*/
Proc freq data=dat.final_dataitt_withmicase;
where per_protocol = 1;
table discharge_ord * my_allocation;
Run;


proc glimmix data=dat.final_dataitt_withmicase method=laplace;
class ProcedureCategory (ref='Urology-') center ;
model discharge_ord( descending order=internal) = my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=multinomial link=clogit;
random intercept / subject= center;
where surgery_status=1 and per_protocol=1;
by _Imputation_;
ods output ParameterEstimates=discharge_glimmix;
estimate 'treatment effect' my_allocation 1 /cl exp alpha=0.05;
run;
proc print data=discharge_glimmix;
run;

proc mianalyze parms=discharge_glimmix;
class ProcedureCategory cfs_score_base ;
modeleffects my_allocation;
run;




/*Complication severity*/
Proc freq data=dat.final_dataitt_withmicase;
where per_protocol = 1;
table poms_severity * my_allocation;
Run;


/*lower grade in poms severity better result */
/*need exp the final reuslt for mianalyze */
proc glimmix data=dat.final_dataitt_withmicase method=quad;
class ProcedureCategory (ref='Urology-') center ;
model poms_severity( descending order=internal) = my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=multinomial link=clogit;
random intercept / subject= center;
where surgery_status=1 and per_protocol=1;
by _Imputation_;
ods output ParameterEstimates=poms_glimmix;
estimate 'treatment effect' my_allocation 1 /cl exp alpha=0.05;
run;
proc print data=poms_glimmix;
run;

proc mianalyze parms=poms_glimmix;
class ProcedureCategory cfs_score_base ;
modeleffects my_allocation;
run;


/*1.35 hazaed ratio for death related variables*/
/* los*/
Proc means data=dat.final_dataitt_withmicase;
class my_allocation;
where per_protocol=1;
var los;
run;


/*need exp result to get final result*/
proc phreg data=dat.final_dataitt_withmicase plots(overlay=stratum)=cif;
class ProcedureCategory (ref='Urology-') center ;
model los*censor_los(0)= my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer / eventcode (FG (BRESLOW)) =1;
by _Imputation_;
where per_protocol =1;
hazardratio 'Marginal Model Analysis' my_allocation;
ods output ParameterEstimates=los_phreg;
estimate 'treatment effect' my_allocation 1 /cl exp alpha=0.05;
run;
proc print data=los_phreg;
run;

proc mianalyze parms=los_phreg;
class ProcedureCategory cfs_score_base ;
modeleffects my_allocation;
run;
