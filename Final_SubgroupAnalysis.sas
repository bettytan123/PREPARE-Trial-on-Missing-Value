/*************************************************
************************************************
PREPARE Subgroup Analysis 
************************************************
************************************************/
/*the follownig script is created to document the cleaning process and subgroup analysis process on 
the Prepare Data using myfinaldata created in complete case analysis. */
/*1. Note Preplanned subgroup analyses for the two primary outcomes will be conducted based on 
sex, age (<75 vs =75), presence of cancer, presence of depression, and frailty status (4 vs = 5). 
Subgroup analyses will be conducted using an effect modifier approach where the subgroup 
indicator will be tested as an interaction term with treatment allocation.
2. Remind on the variables we have created early:
 *create binary indicator for age subgroup; 
    if age_num >= 75 then age_75 = 1;
    else age_75 = 0;

*create binary indicator for presence of depression;
    if PHQ_score >= 3 then depression = 1; /* Positive screen for depression */
    *else depression = 0; /* Negative screen for depression */

* Create the new binary variable 'frailty_status' */
    *if cfs_score_base = 4 then frailty_status = 0;
   /*else if cfs_score_base >= 5 then frailty_status = 1;*/


/* load the input data*/
/*libname dat 'C:\Users\mtaljaard\OneDrive - The Ottawa Hospital\Documents\Consulting\McIsaac Dan\Prepare\data from Dan';*/
libname dat 'C:\Users\yutchen\OneDrive - The Ottawa Hospital\Documents\PREPARE';


/*data check for forest plot*/
proc freq data=dat.myfinaldata;
tables any_complication* my_allocation;
where surgery_status=1;
run;

/***********************************
1- Primary outcome whodas_100_30d
************************************/


/*sex*/
proc sort data=dat.myfinaldata;
by mysex;
proc means data=dat.myfinaldata;
var whodas_100_30d;
class my_allocation;
by mysex;
run;

*produce the frequency distribution of the primary outcome by sex;
proc sort data=dat.myfinaldata;
by mysex;


*subgroup analysis - get the significance of the subgroup treatmet effects;;
proc mixed data=dat.myfinaldata method=REML;
class mysex my_allocation (ref='0') center;
model whodas_100_30d = my_allocation mysex my_allocation*mysex / solution;
random intercept / subject= center;
ods output Diffs=comparisons;
lsmeans my_allocation*mysex/slice=mysex diff cl alpha=0.025;
title 'Complete Case Analysis whodas sex';
run;

proc print data=comparisons;
where mysex= _mysex and my_allocation NE _my_allocation;
run;




/*age (<75 vs =75)*/
proc sort data=dat.myfinaldata;
by age_75;
proc means data=dat.myfinaldata;
var whodas_100_30d;
class my_allocation;
by age_75;
run;



*produce the frequency distribution of the primary outcome by age75;
proc mixed data=dat.myfinaldata method=REML;
class age_75 my_allocation (ref='0') center;
model whodas_100_30d = my_allocation age_75 my_allocation*age_75 / solution;
random intercept / subject= center;
ods output Diffs=comparisons;
lsmeans my_allocation*age_75/slice=age_75 diff cl alpha=0.025;
title 'Complete Case Analysis whodas age_75';
run;

proc print data=comparisons;
where age_75= _age_75 and my_allocation NE _my_allocation;
run;





/*presence of cancer*/
proc sort data=dat.myfinaldata;
by cancer;
proc means data=dat.myfinaldata;
var whodas_100_30d;
class my_allocation;
by cancer;
run;


*produce the frequency distribution of the primary outcome by cancer;
proc mixed data=dat.myfinaldata method=REML;
class cancer my_allocation (ref='0') center;
model whodas_100_30d = my_allocation cancer my_allocation*cancer / solution;
random intercept / subject= center;
ods output Diffs=comparisons;
lsmeans my_allocation*cancer/slice=cancer diff cl alpha=0.025;
title 'Complete Case Analysis whodas cancer';
run;

proc print data=comparisons;
where cancer= _cancer and my_allocation NE _my_allocation;
run;







/*presence of depression*/
proc sort data=dat.myfinaldata;
by depression;
proc means data=dat.myfinaldata;
var whodas_100_30d;
class my_allocation;
by depression;
run;


*produce the frequency distribution of the primary outcome by depression;
proc mixed data=dat.myfinaldata method=REML;
class depression my_allocation (ref='0') center;
model whodas_100_30d = my_allocation depression my_allocation*depression / solution;
random intercept / subject= center;
ods output Diffs=comparisons;
lsmeans my_allocation*depression/slice=depression diff cl alpha=0.025;
title 'Complete Case Analysis whodas depression';
run;

proc print data=comparisons;
where depression= _depression and my_allocation NE _my_allocation;
run;






/*frailty status (4 vs = 5)*/
proc sort data=dat.myfinaldata;
by frailty_status;
proc means data=dat.myfinaldata;
var whodas_100_30d;
class my_allocation;
by frailty_status;
run;

*produce the frequency distribution of the primary outcome by frailty_status;
proc mixed data=dat.myfinaldata method=REML;
class frailty_status my_allocation (ref='0') center;
model whodas_100_30d = my_allocation frailty_status my_allocation*frailty_status / solution;
random intercept / subject= center;
ods output Diffs=comparisons;
lsmeans my_allocation*frailty_status/slice=frailty_status diff cl alpha=0.025;
title 'Complete Case Analysis whodas frailty_status';
run;

proc print data=comparisons;
where frailty_status= _frailty_status and my_allocation NE _my_allocation;
run;





/********************************************
/* 2- Primary outcome POMS any_complication*/
/********************************************


/*sex*/
/*male 0 395*/
proc sort data=dat.myfinaldata;
by mysex;
proc freq data=dat.myfinaldata; 
   where surgery_status=1;
   tables any_complication*my_allocation;
   by mysex;
run;


*subgroup analysis - get the significance of the subgroup treatmet effects;;
proc glimmix data=dat.myfinaldata method=quad;
   class mysex my_allocation (ref='0') center; /* Define categorical variables */
   model  any_complication(event='1') = mysex my_allocation my_allocation*mysex/  
                dist=binary link=logit solution ;
   random intercept / subject=center; /* Specify Site as a random effect */
   lsmeans my_allocation*mysex/slice=mysex diff cl OR alpha=0.025;
   ods output Diffs=comparisons;
   where surgery_status=1;
run;
proc print data=comparisons;
where mysex= _mysex and my_allocation NE _my_allocation;
run;






/*age (<75 vs =75)*/
proc sort data=dat.myfinaldata;
by age_75;
proc freq data=dat.myfinaldata; 
   where surgery_status=1;
   tables any_complication*my_allocation;
   by age_75;
run;


*produce the frequency distribution of the primary outcome by age75;
proc glimmix data=dat.myfinaldata method=quad;
   class age_75 my_allocation (ref='0') center; /* Define categorical variables */
   model  any_complication(event='1') = age_75 my_allocation my_allocation*age_75/  
                dist=binary link=logit solution ;
   random intercept / subject=center; /* Specify Site as a random effect */
   lsmeans my_allocation*age_75/slice=age_75 diff cl OR alpha=0.025;
   title 'Complete Case Analysis whodas age_75';
   ods output Diffs=comparisons;
   where surgery_status=1;
run;
proc print data=comparisons;
where age_75= _age_75 and my_allocation NE _my_allocation;
run;




/*presence of cancer*/
proc sort data=dat.myfinaldata;
by cancer;
proc freq data=dat.myfinaldata; 
   where surgery_status=1;
   tables any_complication*my_allocation;
   by cancer;
run;


*produce the frequency distribution of the primary outcome by cancer;
proc glimmix data=dat.myfinaldata method=quad;
   class cancer my_allocation (ref='0') center; /* Define categorical variables */
   model  any_complication(event='1') = cancer my_allocation my_allocation*cancer/  
                dist=binary link=logit solution ;
   random intercept / subject=center; /* Specify Site as a random effect */
   lsmeans my_allocation*cancer/slice=cancer diff cl OR alpha=0.025;
   title 'Complete Case Analysis whodas cancer';
   ods output Diffs=comparisons;
   where surgery_status=1;
run;
proc print data=comparisons;
where cancer= _cancer and my_allocation NE _my_allocation;
run;




/*presence of depression*/
proc sort data=dat.myfinaldata;
by depression;
proc freq data=dat.myfinaldata; 
   where surgery_status=1;
   tables any_complication*my_allocation;
   by depression;
run;
*produce the frequency distribution of the primary outcome by depression;
proc glimmix data=dat.myfinaldata method=quad;
   class depression my_allocation (ref='0') center; /* Define categorical variables */
   model  any_complication(event='1') = depression my_allocation my_allocation*depression/  
                dist=binary link=logit solution ;
   random intercept / subject=center; /* Specify Site as a random effect */
   lsmeans my_allocation*depression/slice=depression diff cl OR alpha=0.025;
   title 'Complete Case Analysis whodas cancer';
   ods output Diffs=comparisons;
   where surgery_status=1;
run;
proc print data=comparisons;
where depression= _depression and my_allocation NE _my_allocation;
run;




/*frailty status (4 vs = 5)*/
proc sort data=dat.myfinaldata;
by frailty_status;
proc freq data=dat.myfinaldata; 
   where surgery_status=1;
   tables any_complication*my_allocation;
   by frailty_status;
run;

*produce the frequency distribution of the primary outcome by frailty_status;
proc glimmix data=dat.myfinaldata method=quad;
   class frailty_status my_allocation (ref='0') center; /* Define categorical variables */
   model  any_complication(event='1') = frailty_status my_allocation my_allocation*frailty_status/  
                dist=binary link=logit solution ;
   random intercept / subject=center; /* Specify Site as a random effect */
   lsmeans my_allocation*frailty_status/slice=frailty_status diff cl OR alpha=0.025;
   title 'Complete Case Analysis whodas cancer';
   ods output Diffs=comparisons;
   where surgery_status=1;
run;
proc print data=comparisons;
where frailty_status= _frailty_status and my_allocation NE _my_allocation;
run;













