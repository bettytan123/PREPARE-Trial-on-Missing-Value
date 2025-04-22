/*************************************************
************************************************
PREPARE Sensitivity Analysis
************************************************
************************************************/
/*the follownig script is created to document the cleaning process and analysis process on 
the dat.myfinaldata (which is based on whodas_itt_oct_03 in the complete case analysis to obtain any_complication (when no complication is people who do not have surgery and 
complciation when people who have surgery) and whodas_84 who already exist in whodas_itt_oct_03.


/***********************************
1- DATA CLEANING
************************************/

/* 1.1 load the input data*/
/*libname dat 'C:\Users\mtaljaard\OneDrive - The Ottawa Hospital\Documents\Consulting\McIsaac Dan\Prepare\data from Dan';*/
libname dat 'C:\Users\yutchen\OneDrive - The Ottawa Hospital\Documents\PREPARE';

/*load format incase face format error */
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

 
/*quick data check*/
proc contents data= dat.myfinaldata; 
run;

proc contents data=dat.whodas_itt_oct_03;
run; /*note already has whodas_100_30d_84 column so we can use this for whodas84 analysis*/

proc contents data=dat.poms_sens84_oct_03;
run;


/*1.2 create the myany_complication variable for individuals who do not have surgery are assigned a POMS complication status of ‘no complication’*/
proc freq data=dat.myfinaldata;
tables surgery_status any_complication;
run;

data nocomplication_data;
    set dat.myfinaldata;
    if any_complication = 9 then myany_complication = 0;
	else if any_complication= 0 then myany_complication=0; /* No complication */
    else myany_complication = 1; 
run;

/*data check mathced*/
proc freq data=nocomplication_data;
tables any_complication myany_complication;
run;



/***********************************
2- SENSITIVITY ANALYSIS
************************************/


/*2.1 model analysis on no complication*/
proc sort data=nocomplication_data;
by my_allocation;
run;
proc freq data=nocomplication_data;
tables myany_complication;
by my_allocation;
run;

/*becasue we intersts in no complication thus do not need to filter surgery status =1 case*/
proc glimmix data=nocomplication_data method=quad;
class ProcedureCategory (ref='Urology-') center ;
model myany_complication(event='1') = my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=binary link=logit;
random intercept / subject= center;
estimate 'treatment effect' my_allocation 1 /exp cl alpha=0.025;
title 'Complete Case Analysis';
run;






/*2.2 model analysis on WHODAS_84 and POMS_84*/
/*2.21 Primary outcome whodas84 since this varible already exist in myfinaldata(whodas_itt)*/
proc sort data=dat.myfinaldata;
by my_allocation;
run;
proc means data=dat.myfinaldata;
var whodas_100_30d_84;
class my_allocation;
run;


proc mixed data=dat.myfinaldata method=REML;
class ProcedureCategory (ref='Urology-') center ;
model whodas_100_30d_84 = my_allocation whodas_100_baseline ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution;
random intercept / subject= center;
ods output SolutionF=whodas;
estimate 'treatment effect' my_allocation 1 /cl alpha=0.025;
title 'Complete Case Analysis whodas';
run;






/*2.22 Primary outcome poms84 (using itt data thus using myfinaldata's myany_complication for this case with where surgery_status=1*/ 
proc sort data=nocomplication_data;
by my_allocation;
run;
proc freq data=nocomplication_data;;
tables myany_complication;
by my_allocation;
where surgery_status=1;
run;

/*we still interested in event when complication happen*/
proc glimmix data=nocomplication_data method=quad;
class ProcedureCategory (ref='Urology-') center ;
model myany_complication(event='1') = my_allocation ProcedureCategory mySex age_num frailty_status malnutrition_risk cancer/ solution dist=binary link=logit;
random intercept / subject= center;
estimate 'treatment effect' my_allocation 1 /exp cl alpha=0.025;
title 'Complete Case Analysis';
where surgery_status=1;
run;

