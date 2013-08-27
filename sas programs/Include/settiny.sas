/* Compile the utility macros. */
*libname adhoc1 '/lcl/bks/adhoc/sas/data/stlblps/';
%include './Include/MACRO.sas';

%macro settiny(dates, tsindex, kvars, outds);

 /* Compute the number of quarter-specific TINY data sets that are
    stacked together. */
       %let numds=%nvar(&dates);

       %do i=1 %to &numds;
           %do;
           %let inds=%scan(&dates, &i);

           /* Create a SAS date values. */
           %let yy=%substr(&inds, 3, 2);
           %let qq=%substr(&inds, 5, 2);

           %let yy1=%substr(&inds, 1, 4);

           %if %substr(&inds, 5, 2)=03 %then %let mm='03';
           %if %substr(&inds, 5, 2)=06 %then %let mm='06';
           %if %substr(&inds, 5, 2)=09 %then %let mm='09';
           %if %substr(&inds, 5, 2)=12 %then %let mm='12';


           %if %substr(&inds, 5, 2)=03 %then %let dd='31';
           %if %substr(&inds, 5, 2)=06 %then %let dd='30';
           %if %substr(&inds, 5, 2)=09 %then %let dd='30';
           %if %substr(&inds, 5, 2)=12 %then %let dd='31';

           data bigg(keep=entity name &tsindex rssd9348 &kvars);
           format &tsindex yyq4.;
           set %antiny(&yy&qq);
               datestr=trim(left(&dd.||&mm.||%quote("&yy1")));
               &tsindex=input(datestr, ddmmyy8.);
     
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

%mend settiny;

/* Compile the utility macros. */
*libname adhoc1 '/lcl/bks/adhoc/sas/data/stlblps/';
%include './Include/MACRO.sas';

%macro settiny_allvars(dates, tsindex, outds);

 /* Compute the number of quarter-specific TINY data sets that are
    stacked together. */
       %let numds=%nvar(&dates);

       %do i=1 %to &numds;
           %do;
           %let inds=%scan(&dates, &i);

           /* Create a SAS date values. */
           %let yy=%substr(&inds, 3, 2);
           %let qq=%substr(&inds, 5, 2);

           %let yy1=%substr(&inds, 1, 4);

           %if %substr(&inds, 5, 2)=03 %then %let mm='03';
           %if %substr(&inds, 5, 2)=06 %then %let mm='06';
           %if %substr(&inds, 5, 2)=09 %then %let mm='09';
           %if %substr(&inds, 5, 2)=12 %then %let mm='12';


           %if %substr(&inds, 5, 2)=03 %then %let dd='31';
           %if %substr(&inds, 5, 2)=06 %then %let dd='30';
           %if %substr(&inds, 5, 2)=09 %then %let dd='30';
           %if %substr(&inds, 5, 2)=12 %then %let dd='31';

           data bigg;
           format &tsindex yyq4.;
           set %antiny(&yy&qq);
               datestr=trim(left(&dd.||&mm.||%quote("&yy1")));
               &tsindex=input(datestr, ddmmyy8.);
     
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

%mend settiny_allvars;

