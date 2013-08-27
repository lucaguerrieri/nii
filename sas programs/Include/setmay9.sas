/* Compile the utility macros. */
*libname adhoc1 '/lcl/bks/adhoc/sas/data/stlblps/';
%include './Include/MACRO.sas';

%macro setmay9(dates, tsindex, kvars, outds, align='e');

 /* Compute the number of quarter-specific MAY9 data sets that are
    stacked together. */
       %let numds=%nvar(&dates);

       %do i=1 %to &numds;
           %do;
           %let inds=%scan(&dates, &i);

           /* Create a SAS date values. */
           %let yy=%substr(&inds, 3, 2);
           %let qq=%substr(&inds, 5, 2);

           %let yy1=%substr(&inds, 1, 4);

           %if %substr(&inds, 5, 2)=03 %then %let mm=03;
           %if %substr(&inds, 5, 2)=06 %then %let mm=06;
           %if %substr(&inds, 5, 2)=09 %then %let mm=09;
           %if %substr(&inds, 5, 2)=12 %then %let mm=12;


           %if %substr(&inds, 5, 2)=03 %then %let dd=31;
           %if %substr(&inds, 5, 2)=06 %then %let dd=30;
           %if %substr(&inds, 5, 2)=09 %then %let dd=30;
           %if %substr(&inds, 5, 2)=12 %then %let dd=31;

           data bigg(keep=entity name &tsindex rssd9002 &kvars);
           format &tsindex yyq4.;
           set may9.may9&yy.&mm;
               temp=mdy(&mm,&dd,&yy1);
               &tsindex=intnx('qtr',temp,0,&align);
     
           format name name.;
           name=entity;

           %if &i=1 %then
               %do;
               data &outds;
               set bigg;
               run;
               %end;
           %if &i>1 %then
               %do;
               proc append base=&outds data=bigg force;
               run;
               %end;
           %end;
       %end; * close i loop;

%mend setmay9;




/*
%settiny(dates=200603 200606 200609 200612 200703 200706 200709 200512
               200509 200506 200209 200212 200303 200306 200309 200312
               200003 200006 200009 200012 200103 200106 200109 200112
               199803 199806 199809 199812 199903 199906 199909 199912
               199603 199606 199609 199612 199703 199706 199709 199712
               199403 199406 199409 199412 199503 199506 199509 199512
               199203 199206 199209 199212 199303 199306 199309 199312
               199003 199006 199009 199012 199103 199106 199109 199112
               , 
               outds=adhoc1.capitalds);

proc sort data=adhoc1.capitalds; by entity tsindex;
run;              
*/
