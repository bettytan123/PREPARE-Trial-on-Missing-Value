/***********************************************
************************************************
MI Imputation Model Case PREPARE Trial
************************************************
************************************************/
/*The following script is using the myfinaldatami dataset created in Final_CompleteCaseAnalysis as begining*/

/* *************************
1- MI MODEL ANALYSIS
***************************/

/*1.1 load mi data*/
/*libname dat 'C:\Users\mtaljaard\OneDrive - The Ottawa Hospital\Documents\Consulting\McIsaac Dan\Prepare\data from Dan'*/
libname dat 'C:\Users\yutchen\OneDrive - The Ottawa Hospital\Documents\PREPARE';*/

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


data myfinaldatami;
set dat.myfinaldatami;
run;

proc contents data=myfinaldatami;
run;


proc sort data= dat.myfinaldatami;
by PID;
run;


/*1.2 Build control and intervention group to find statistics */

/*build control dataset*/
data control_subsetmi;
    set dat.myfinaldatami; 
    if allocation = 'Standard Care';
run;

/*build intervention dataset*/
data intervention_subsetmi;
    set dat.myfinaldatami; 
    if allocation = 'Prehab Program';
run;

/*statistics whodas30*/
proc means data=control_subsetmi mean std;
    var whodas_100_30d; 
run;

proc means data=intervention_subsetmi mean std;
    var whodas_100_30d; 
run;

/*sts*/
proc means data=control_subsetmi mean std;
var sts_time_dc;
run;

proc means data=intervention_subsetmi mean std;
 var sts_time_dc;
 run;


/*eq value*/

proc means data=control_subsetmi; 
    var eq_value_30d;
run;

proc means data=intervention_subsetmi; 
    var eq_value_30d;
run;


/*eq vas*/
proc means data=control_subsetmi; 
    var eq_vas_30d;
run;

proc means data=intervention_subsetmi; 
    var eq_vas_30d;
run;



/*1.3 continuous outcomes for mean difference to be reported*/

*mean diff whodas;
proc mixed data=dat.myfinaldatami method=REML;
class ProcedureCategory (ref='Urology-') center ;
model whodas_100_30d = my_allocation whodas_100_baseline ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution;
random intercept / subject= center;
by _Imputation_;
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
proc mixed data=dat.myfinaldatami method=REML;
class ProcedureCategory (ref='Urology-') center ;
model eq_value_30d = my_allocation eq_value_base ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution;
random intercept / subject= center;
by _Imputation_;
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
proc mixed data=dat.myfinaldatami method=REML;
class ProcedureCategory (ref='Urology-') center ;
model eq_vas_30d = my_allocation eq_vas_base ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution;
random intercept / subject= center;
by _Imputation_;
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
proc mixed data=dat.myfinaldatami method=REML;
class ProcedureCategory (ref='Urology-') center ;
model sts_time_dc = my_allocation sts_time_base ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution;
random intercept / subject= center;
by _Imputation_;
ods output SolutionF=sts_mixed;
estimate 'treatment effect' my_allocation 1 /cl alpha=0.05;
run;
proc print data=sts_mixed;
run;

proc mianalyze parms=sts_mixed;
class ProcedureCategory cfs_score_base ;
modeleffects my_allocation ;
run;



/* 1.4 odds ratio for binary logistic varible*/

/*poms severity 
-->becasue no missing in the first place we could use complete case result here*/



/*readmission*/
proc freq data=control_subsetmi; 
   where surgery_status=1;
   tables readmission;
run;

proc freq data= intervention_subsetmi;
where surgery_status=1;
table readmission;
run;

proc glimmix data=dat.myfinaldatami method=quad;
class ProcedureCategory (ref='Urology-') center ;
model readmission(event='1') = my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=binary link=logit;
random intercept / subject= center;
where surgery_status=1;
by _Imputation_;
ods output ParameterEstimates=readmission_glimmix;
estimate 'treatment effect' my_allocation 1 /cl exp alpha=0.05;
run;
proc print data=readmission_glimmix;
run;

proc mianalyze parms=readmission_glimmix;
class ProcedureCategory cfs_score_base ;
modeleffects my_allocation;
run;





/* post falls*/
proc freq data=control_subsetmi; 
where surgery_status=1;
tables post_fall;
run;

proc freq data= intervention_subsetmi;
where surgery_status=1;
table post_fall;
run;


proc glimmix data=dat.myfinaldatami method=quad;
class ProcedureCategory (ref='Urology-') center ;
model post_fall(event='1') = my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=binary link=logit;
random intercept / subject= center;
where surgery_status=1;
by _Imputation_;
ods output ParameterEstimates=postfall_glimmix;
estimate 'treatment effect' my_allocation 1 /cl exp alpha=0.05;
run;
proc print data=postfall_glimmix;
run;

proc mianalyze parms=postfall_glimmix;
class ProcedureCategory cfs_score_base ;
modeleffects my_allocation;
run;





/*1.5 odd ratios for ordinal logisitc analysis */

/*katz
-->becasue no missing in the first place we could use complete case result here*/




/*discharge disposition*/

proc freq data=control_subsetmi; 
   where surgery_status=1;
   tables discharge_ord;
run;

proc freq data= intervention_subsetmi;
where surgery_status=1;
table discharge_ord;
run;


proc glimmix data=dat.myfinaldatami method=quad;
class ProcedureCategory (ref='Urology-') center ;
model discharge_ord( descending order=internal) = my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=multinomial link=clogit;
random intercept / subject= center;
where surgery_status=1;
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

proc freq data=control_subsetmi; 
   where surgery_status=1;
   tables poms_severity;
run;

proc freq data= intervention_subsetmi;
where surgery_status=1;
table poms_severity;
run;



/*lower grade in poms severity better result */
proc glimmix data=dat.myfinaldatami method=quad;
class ProcedureCategory (ref='Urology-') center ;
model poms_severity( descending order=internal) = my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=multinomial link=clogit;
random intercept / subject= center;
where surgery_status=1;
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



/*1.6 hazaed ratio for death related variables*/
/*mortality death_censor*/
proc freq data=control_subsetmi; 
    tables death_censor;
run;

proc freq data= intervention_subsetmi;
 table death_censor;
run;



proc phreg data=dat.myfinaldatami covs(aggregate);
class ProcedureCategory (ref='Urology-') center ;
model death_days*death_censor(0)= my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer;
random center/ solution;
by _Imputation_;
hazardratio 'Marginal Model Analysis' my_allocation;
ods output ParameterEstimates=death_phreg;
estimate 'treatment effect' my_allocation 1 /cl exp alpha=0.05;
run;
proc print data=death_phreg;
run;

proc mianalyze parms=death_phreg;
class ProcedureCategory cfs_score_base ;
modeleffects my_allocation;
run;




/* los*/
proc means data=control_subsetmi mean std; 
    var los;
run;

proc means data= intervention_subsetmi mean std;
 var los;
run;



proc phreg data=dat.myfinaldatami plots(overlay=stratum)=cif;
class ProcedureCategory (ref='Urology-') center ;
model los*censor_los(0)= my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer / eventcode (FG (BRESLOW)) =1;
by _Imputation_;
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




