%include './Include/MACRO.sas';
****macro list a sequence of macrovariables of the form yyyymm in quarterly increments.;
****input of the form yyyyQq (ex- dateseq(2006Q3,2007Q2) -> 200609 200612 200703 200706);
%macro dateseq(sdate, edate);
  %let quarters =  03 06 09 12;       
  %let syear = %substr(&sdate,1,4);
  %let sqtr = %substr(&sdate,6,1);
  %let eyear = %substr(&edate,1,4);
  %let eqtr = %substr(&edate,6,1);

  %if &syear = &eyear %then %do;       
    %do jj= %eval(&sqtr) %to %eval(&eqtr);
      &syear.%scan(&quarters,&jj)
    %end;
  %end; %else %do;
    %do jj= %eval(&sqtr) %to 4;
      &syear.%scan(&quarters,&jj)
    %end;  

    %do ii= %eval(&syear)+1 %to %eval(&eyear)-1;
      %do jj = 1 %to 4;
        &ii.%scan(&quarters,&jj)
      %end;
    %end;

    %do jj= 1 %to %eval(&eqtr);
      &eyear.%scan(&quarters,&jj)
    %end;
  %end;
%mend dateseq;

**** creates a list of listvar values as a macrovariable;
%macro gen_macrolist(listname,datain,listvar);
       %global &listname;
       proc sql noprint;
		select distinct &listvar
	    into :&listname separated by ' '
	    from &datain;

        select count(distinct &listvar)
        into: numunique
        from &datain;
       
       /*display list size*/
	   %put List size: &numunique;
	   quit;
%mend gen_macrolist;

**** creates a sas dataset from a fame database;
%macro fame2sas(sdate,edate,data,listin,listout,dbn);
  data famevars;
    length fname$60 sname$32;
    %do ii=1 %to %nvar(&listin);
      %let invar = %scan(&listin,&ii);
      %let outvar = %scan(&listout,&ii);
      fname="'&invar'"; sname="&outvar"; output;
    %end;
  run;

  %getFAME(sdate=&sdate,edate=&edate,data=&data,listin=famevars,dbn=&dbn);

  data &data;
    format date_sas mmddyy.;
    set &data(drop=__date rename=(_sasdate=date_sas));
  run;

  %dsrm(famevars);
%mend;

**** foils macrovariables;
**** ex- %varfoil(A B C, 1 2 3) -> A1 A2 A3 B1 B2 B3 C1 C2 C3;
%macro varfoil(vars1,vars2);
  %local ii;
  %local jj;
  %do ii=1 %to %nvar(&vars1);
    %do jj=1 %to %nvar(&vars2);
    %scan(&vars1,&ii)%scan(&vars2,&jj)
    %end;
  %end;
%mend varfoil;

**** foils macrovariables in reverse order;
**** ex- %varfoil2(A B C, 1 2 3) -> A1 B1 C1 A2 B2 C2 A3 B3 C3;
%macro varfoil2(vars1,vars2);
  %local ii;
  %local jj;
  %do ii=1 %to %nvar(&vars2);
    %do jj=1 %to %nvar(&vars1);
    %scan(&vars1,&jj)%scan(&vars2,&ii)
    %end;
  %end;
%mend varfoil2;

*converts a date from integer yyyymmdd format to date format;
%macro int2date(date);
  mdy(floor(mod(&date,10000)/100),mod(&date,100),floor(&date/10000))
%mend int2date;

*converts a date to integer yyyymmdd format;
%macro date2int(date);
  10000*year(&date)+100*month(&date)+day(&date)
%mend date2int;

*replaces numeric values with new values (ex- from -999 to missing);
%macro replace_num(from,to);
  array xx _numeric_;
  do over xx;
    if xx=&from then xx=&to;
  end;
%mend replace_num;

*left skeleton-merge;
%macro lskmerge(lds,rds,lvar,rvar,outds);
  proc sort nodupkey data=&lds(keep=&lvar) out=ldsv; by &lvar; run;
  proc sort nodupkey data=&rds(keep=&lvar &rvar) out=rdsv; by &lvar &rvar; run;
  data rdsv;
    merge ldsv(in=a) rdsv;
    by &lvar;
    if a;
  run;

  proc sort nodupkey data=rdsv out=rdsv; by &rvar; run;
  
  proc sql;
    create table skds as
    select ldsv.&lvar, rdsv.&rvar
    from ldsv, rdsv
    ;
  quit;

  proc sort data=skds; by &lvar &rvar; run;
  proc sort data=&rds out=rdss; by &lvar &rvar; run;
  data &outds;
    merge skds(in=a) rdss;
    by &lvar &rvar;
    if a;
  run;

  proc sort data=&outds; by &rvar &lvar; run;

  %dsrm(ldsv rdsv skds rdss);
%mend lskmerge;
