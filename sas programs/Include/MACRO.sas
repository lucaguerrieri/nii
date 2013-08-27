* (C) Copyright 1996 Egon Zakrajsek;
* All Rights Reserved;

/**************************************************************************/
/* Purpose: General utility macros.                                       */
/*                                                                        */
/* This Version: 10-05-03                                                 */
/**************************************************************************/

   
***************************************************************************;
* Purpose: To append a suffix to a list of variables;

%macro varsfx(vars, sfx);

	%local ii;

	%if &sfx="" %then %str(&vars;);
	%else
	    %do ii=1 %to %nvar(&vars);
		%scan(&vars, &ii)&sfx
	    %end;
	  
%mend varsfx;

***************************************************************************;
* Purpose: To append a prefix to a list of variables;

%macro varpfx(vars, pfx);

	%local ii;

	%if &pfx="" %then %str(&vars;);
	%else
	    %do ii=1 %to %nvar(&vars);
		&pfx.%scan(&vars, &ii)
	    %end;

%mend varpfx;

***************************************************************************;
/* Purpose: To compute the number of observations in a SAS data set,
	    specified by the argument inds. The number of observations 
	    is assigned to a global macro variable nobs. */

%macro numobs(inds);

	%global nobs;

	proc sql;
	create view _nobs as 
	select count(*) as count from &inds;

	data _null_;
	set _nobs;
	call symput('nobs', left(put(count,8.)));
	run;

	%dsrm(ds=_nobs, mt=view);

%mend numobs;

***************************************************************************;
* Purpose: To create a macro variable of the form PRE1-PREnum;

%macro varlist(pre, num);

	%str(&pre.1-&pre.&num);

%mend varlist;

***************************************************************************;
/* Purpose: To create a macro variable containing the sequence num1 num1+1
            num1+2 ... num2. */

%macro seq(num1, num2, increment=1);

	%local ii;

	%do ii=%eval(&num1) %to %eval(&num2) %by &increment;
	    %do;
	    %str(&ii)
            %end;
	%end;

%mend seq;

***************************************************************************;
/* Purpose: To create a macro variable that replicates the argument x the 
	    number of times given by the argument times. */

%macro rep(x, times);

	%local ii;

	%do ii=1 %to %eval(&times);
	    %do;
	    %str(&x)
	    %end;
	%end;

%mend rep;

***************************************************************************;
/* Purpose: To create a macro variable of the form PREnum1 PRE(num1+1) ... 
	 PREnum2. */

%macro dset(pre, num1, num2);

	%local ii;

	%do ii=%eval(&num1) %to %eval(&num2);
	    %do;
	    %str(&pre.&ii)
	    %end;
	%end;

%mend dset;

***************************************************************************;
/* Purpose: To create a macro variable that contains the same elements as
	    the macro variable vars, except in reverse order (e.g., if 
	    vars=A B C D, the new macro variable is D C B A). */

%macro reverse(vars);

	%local ii _vname;

	%do ii=0 %to %eval(%nvar(&vars)-1);
	    %do;
	    %let _vname=%scan(&vars, %eval(%nvar(&vars) - &ii));
	    %str(&_vname)
	    %end;
	%end;

%mend reverse;

***************************************************************************;
/* Purpose: To create a macro variable of the form &MPREmnum1 
	    &MPRE(mnum1+1) ... &MPREmnum2. */

%macro mdset(mpre, mnum1, mnum2);

	%local ii _mpre;

       %do ii=%eval(&mnum1) %to %eval(&mnum2);
	   %do;
	   %let _mpre=%varsfx(&mpre, &ii);
	   %str(&&&_mpre)
	   %end;
       %end;

%mend mdset;

***************************************************************************;
/* Purpose: To separate a list of macro variables with commas. */

%macro addcomma(vars);

	%local _vnum ii _vname;

	%let _vnum=%nvar(&vars);

	%if &_vnum=1 %then
	    %do;
	    %str(&vars);
	    %end;

	%if &_vnum>1 %then 
	    %do;
	    %do ii=1 %to &_vnum;
	        %do;
		%let _vname=%scan(&vars, &ii);
	        %if &ii=1 %then %str(&_vname);
	        %else
	        %str(,&_vname);
	        %end;
	    %end;
	    %end;

%mend addcomma;

***************************************************************************;
/* Purpose: To separate a list of macro variables with plus (+) signs. */

%macro addplus(vars);

	%local _vnum ii _vname;

	%let _vnum=%nvar(&vars);

	%if &_vnum=1 %then
	    %do;
	    %str(&vars);
	    %end;

	%if &_vnum>1 %then 
	    %do;
	    %do ii=1 %to &_vnum;
	        %do;
		%let _vname=%scan(&vars, &ii);
	        %if &ii=1 %then %str(&_vname);
	        %else
	        %str(+&_vname);
	        %end;
	    %end;
	    %end;

%mend addplus;

***************************************************************************;
/* Purpose: To separate a list of macro variables with star (*) signs. */

%macro addstar(vars);

	%local _vnum ii _vname;

	%let _vnum=%nvar(&vars);

	%if &_vnum=1 %then
	    %do;
	    %str(&vars);
	    %end;

	%if &_vnum>1 %then 
	    %do;
	    %do ii=1 %to &_vnum;
	        %do;
		%let _vname=%scan(&vars, &ii);
	        %if &ii=1 %then %str(&_vname);
	        %else
	        %str(*&_vname);
	        %end;
	    %end;
	    %end;

%mend addstar;

***************************************************************************;
/* Purpose: To create a macro variable of the form PREnum1, ..., PREnum2.
	    Use in PROC REG for F-test. */

%macro strftest(pre, num1, num2);

	%local ii;

	%do ii=%eval(&num1) %to %eval(&num2);
	    %do;
	    %if &ii=%eval(&num1) %then %str(&pre.&ii);
	    %if &ii>%eval(&num1) and &ii<%eval(&num2) %then %str(,&pre.&ii);
	    %if &ii=%eval(&num2) %then %str(,&pre.&ii;);
	    %end;
	%end;

%mend strftest;

***************************************************************************;
/* Purpose: To create a macro variable of the form PREnum1 + ... + PREnum2 
	    Use in PROC REG for the sum of coeffcients test. */

%macro strstest(pre, num1, num2);

	%local ii;

	%do ii=%eval(&num1) %to %eval(&num2);
	    %do;
	    %if &ii=%eval(&num1) %then %str(&pre.&ii);
	    %if &ii>%eval(&num1) and &ii<%eval(&num2) %then %str(+&pre.&ii);
	    %if &ii=%eval(&num2) %then %str(+&pre.&ii;);
	    %end;
	%end;

%mend strstest;

***************************************************************************;
/* Purpose: To create a macro variable of the form b1*VAR1 + ... + bk*VARk.
	    Used in proc MODEL to create the RHS of a linear equation. */ 

%macro rhseq(vars, coeff=b);

	%local ii _var _parm;

	%do ii=1 %to %nvar(&vars);
	    %do;
	    %let _var=%scan(&vars, &ii);
	    %let _parm=&coeff&ii;
	    %let t&ii=&_parm*&_var;
	    %if &ii=1 %then %str(&&t&ii);
	    %else %str(+ &&t&ii);
	    %end;
	%end;

%mend rhseq;

***************************************************************************;
/* Purpose: To compute the number of elements that are listed in a 
	    macro variable vars. */

%macro nvar(vars);

	%local count word;
	%let count=1;

	%let word=%qscan(&vars, &count, %str( ));
	%do %while (&word ne);
	%let count=%eval(&count+1);
	%let word=%qscan(&vars, &count, %str( ));
	%end;
	%eval(&count-1)

%mend nvar;

***************************************************************************;
/* Purpose: To find the largest (numeric) element in the 
	    macro variable numstr. The maximal value is
	    assigned to a global macro variable maxstr. */

%macro mmax(numstr);

	%global maxstr;
	%local ii _numstr;

	%do ii=1 %to %nvar(&numstr);
	    %do;
	    %if &ii=1 %then %let _numstr=%scan(&numstr, &ii);
	    %if &ii>1 and &ii<%nvar(&numstr) %then 
	    %let _numstr=&_numstr , %scan(&numstr, &ii);
	    %if &ii=%nvar(&numstr) %then 
	    %let _numstr=&_numstr, %scan(&numstr, &ii);
	    %end;
	%end;

	data _null_;
	     _maxstr=max(&_numstr);
	     call symput('maxstr', left(put(_maxstr, 8.)));
	run;

%mend mmax;

***************************************************************************;
/* Purpose: To find the smallest (numeric) element in the 
	    macro variable numstr. The minimal  value is
	    assigned to a global macro variable minstr. */

%macro mmin(numstr);

	%global minstr;
	%local ii _numstr;

	%do ii=1 %to %nvar(&numstr);
	    %do;
	    %if &ii=1 %then %let _numstr=%scan(&numstr, &ii);
	    %if &ii>1 and &ii<%nvar(&numstr) %then 
	    %let _numstr=&_numstr , %scan(&numstr, &ii);
	    %if &ii=%nvar(&numstr) %then 
	    %let _numstr=&_numstr, %scan(&numstr, &ii);
	    %end;
	%end;

	data _null_;
	     _minstr=min(&_numstr);
	     call symput('minstr', left(put(_minstr, 8.)));
	run;

%mend mmin;

***************************************************************************;
/* Purpose: To rename a list of variables specifed in vars to &pre1, 
	    &pre2, ... */

%macro renamer(vars, pre);

	%local ii;

	%do ii=1 %to %nvar(&vars);
	    %scan(&vars, &ii)=&pre.&ii
	%end;

%mend renamer;

***************************************************************************;
/* Purpose: To rename a list of variables specified by the vars1 argument 
	    to a list of variables specified by the vars2 argument. */

%macro renamer1(vars1, vars2);

	%local ii;

	%if %nvar(&vars1)>=%nvar(&vars2) %then
	    %do;
	    %do ii=1 %to %nvar(&vars1);
		%scan(&vars1, &ii)=%scan(&vars2, &ii)
	    %end;
	    %end;

%mend renamer1;

***************************************************************************;
/* Purpose: To remove outliers from a data set:
		
inds............input data set.
vars............variables for which to remove outliers.
pmin............specifies the percentile which determines the lower 
		bound for valid observations.
pmax............specifies the percentile which determines the upper 
		bound for valid observations.
		NOTE: Observations below the PMIN and above the PMAX 
		      percentile are set to missing in the OUTDS data 
		      set.
outds...........name of the output data set containing trimmed 
		variables and other variables from the INDS data 
		set. 
na_rm=T.........removes the missing values from OUTDS data set. */

%macro outliers(inds, outds, vars, pmin=10, pmax=90, na_rm=F);

	%local ii _vname;

	%do ii=1 %to %nvar(&vars);
	    %do;
	    %let _vname=%scan(&vars, &ii);

	    proc univariate data=&inds(keep=&_vname) noprint;
	    var &_vname;
	    output out=p&ii pctlpts=&pmin &pmax pctlpre=p&ii
	    pctlname=minn maxx;
	    run;        
	    %end;
	%end;

	data p_out;
	merge %dset(p, num1=1, num2=%nvar(&vars));
	run;

	data &outds;
	set &inds;
	    if _n_=1 then set p_out;
	    %do ii=1 %to %nvar(&vars);
		%do;
		%let _vname=%scan(&vars, &ii);
		if &_vname<p&ii.minn or &_vname>p&ii.maxx then &_vname=.;
		drop p&ii.minn p&ii.maxx;
		%end;
	    %end; * close ii loop;
	run;

	%if &na_rm=T %then
	    %do;
	    data &outds;
	    set &outds;
		%do ii=1 %to %nvar(&vars);
		    %do;
		    %let _vname=%scan(&vars, &ii);
		    if &_vname=. then delete;
		    %end;
		%end;
	    run;
	    %end;

	%dsrm(ds=p_out %dset(p, num1=1, num2=%nvar(&vars)));

%mend outliers;

***************************************************************************;
/* Purpose: To create a full set of seasonal dummies.
		
inds............name of the input data set.
freq............specifies the frequency of the data:
		Q=quarterly (default)
		M=monthly             
outds...........name of the output data set. The OUTDS data 
		set contains all the variables from the INDS 
		data set, as well as a full set of seasonal 
		dummies. */ 

%macro seasonal(inds, outds, freq=Q);

	%local ii;
	
	data &outds;
	set &inds;
	    * Quarterly seasonal dummies;
	    %if &freq=Q %then
		%do;
		%do ii=1 %to 4;
		    %do;
		    if qtr(date)=&ii then sd&ii=1;
		    else sd&ii=0;
		    %end;
		%end; * close ii loop;
		%end; * if freq=Q;
	    * Monthly seasonal dummies;
	    %if &freq=M %then
		%do;
		%do ii=1 %to 12;
		    %do;
		    if month(date)=&ii then sd&ii=1;
		    else sd&ii=0;
		    %end;
		%end; * close ii loop;
		%end; * if freq=M;
	run;

%mend seasonal;

***************************************************************************;
%macro dsrm(ds, lib=work, mt=data);

	proc datasets memtype=&mt nolist lib=&lib;
	delete &ds;
	run;
	quit;

%mend dsrm;

***************************************************************************;
/* Purpose: To transpose a data set sorted by BYVARS and CLASS 
	    variables to a data set sorted by BYVARS variables 
	    only:
		
inds............name of the input data set.
outds...........name of the transposed data set.
byvars..........name(s) of the variable(s) by which the INDS data 
		set is sorted.
class...........name of the variable by which the INDS data set 
		is sorted within the BYVARS sort. That is, the 
		INDS data set is sorted by BYVARS CLASS statement.
vars............name(s) of the variable(s) in the INDS data set 
		that are to be transposed. The naming convention 
		is that the transposed variable is indexed with 
		the value of the CLASS variable as a suffix. 
class_s.........determines the starting value of the CLASS variable.
class_e.........determines the ending value of the CLASS variable. */

%macro xposebyx(inds, outds, byvars, class, vars, class_s, class_e);

	%local ii _vname;

	proc sort data=&inds(keep=&byvars &class &vars) out=&outds;
	by &byvars;
	run;

	%do ii=1 %to %nvar(&vars);
	    %do;
	    %let _vname=%scan(&vars, &ii);

	    proc transpose data=&outds(keep=&byvars &class &_vname)
	    out=&_vname(keep=&byvars &&_vname.&class_s-&&_vname.&class_e) 
	    prefix=&_vname;
	    by &byvars;
	    var &_vname;
	    id &class;
	    where &class>=&class_s and &class<=&class_e;
	    run;
	    %end;
	%end;

	%if %nvar(&vars)>1 %then 
	    %do;
	    data &outds;
	    merge &vars;
	    by &byvars;
	    run;
	    %end;

	%if %nvar(&vars)=1 %then
	    %do;
	    data &outds;
	    set &_vname;
	    run;
	    %end;

	%dsrm(ds=&vars);

%mend xposebyx;

***************************************************************************;
/* Purpose: Detrending and/or deseasonalizing data:

inds............specifies the name of the data set containing the
		variable(s) that are to be detrended and/or 
		deseasonalized.
outds...........specifies the name of the output data set containing 
		the transformed variable(s).
parmds..........specifies the name of the data set containing the 
		estimated seasonal and/or trend parameters.
vars............specifies the names of the variable(s) that are to be
		detrended and/or deseasonalized.
byvars..........detrends and/or deseasonalizes the data on groups of
		observations defined by the BYVARS variable(s). If 
		BYVARS=F, the INDS must be a pure time series data set. 
		If the INDS data set is a panel, then the BYVARS argument 
		must specify the cross-sectional index variable, etc.
ptrend..........the highest power of the trend term in the regression(s);
seasonal=T......deseasonalizes the data by including seasonal dummies in 
		the regression(s).
freq............specifies the frequency of the data:
		Q=quarterly (default)
		M=monthly
center=T........centers the detrended and/or deseasonalized variable(s) 
		to have mean zero.
logxx=T.........converts the data to natural logarithms prior to any
		transformations. The seasonally adjusted and/or detrended
		log level data are exponentiated before they are written
		to the OUTDS data set.
		NOTE: If center=F and logxx=T, it is the mean of the log
		      level is added to the seasonally adjusted and/or
		      detrended data prior to exponentiating.
na_rm=T.........removes (i.e. windows the data) all missing observations
		prior to any transformations.
incl=T..........specifies that the OUTDS data set is to include all the
		variable(s) from the INDS data set (except the un-detrended
		and/or seasonally un-adjusted variable(s) specified by the
		VARS argument). */

%macro sadt(inds, outds, vars, parmds=F, ptrend=0, seasonal=F, freq=Q,
	    center=F, logxx=F, byvars=F, incl=T, na_rm=F);

	* Set options;
	options nonotes;

	/* Declare local macro variables. */
	%local r _sdum _freq byvars1 bylev xvars wvars
	       vname mwvars _dvname _dvname1;

	%let r=%nvar(&vars);
 
	%if &freq=Q %then
	    %do;
	    %let _sdnum=4;
	    %let _freq='qtr';
	    %end;
	%if &freq=M %then
	    %do;
	    %let _sdnum=12;
	    %let _freq='month';
	    %end;

	%if &byvars=F %then
	    %do;
	    %let byvars1=;
	    %let bylev=_mrg;
	    %end;

	%if &byvars^=F %then
	    %do;
	    %let byvars1=&byvars;
	    %let bylev=&byvars;
	    %end;

	%if &ptrend=0 %then
	    %do;
	    %if &seasonal=T %then
		%do;
		%let xvars=%dset(sd, num1=1, num2=&_sdnum);
		%end;
	    %end; * if ptrend=0;

	%if &ptrend>0 %then
	    %do;
	    %if &seasonal=T %then
		%do;
		%let xvars=%dset(trend, num1=1, num2=&ptrend) 
			   %dset(sd, num1=1, num2=&_sdnum);
		%end;
	    %if &seasonal=F %then
		%do;
		%let xvars=%dset(trend, num1=1, num2=&ptrend);
		%end;
	    %end; * if ptrend>0;

	%let wvars=%dset(w, num1=1, num2=&r);

	data sadtds(rename=(%renamer(&vars, w)));
	set &inds(keep=&byvars1 date &vars);
	    %if %length(&byvars1)=0 %then
		%do;
		_mrg=1;
		%end;
	    * Remove all missing observations;
	    %if &na_rm=T %then
		%do;
		%do ii=1 %to &r;
		    %do;
		    %let vname=%scan(&vars, &ii);
		    if &vname=. then delete;
		    %end;
		%end; * close ii loop;
		%end; * if na_rm=T;
	    * Log transformation;
	    %if &logxx=T %then
		%do;
		array zz{*} &vars;
		    do ii=1 to dim(zz);
		       do;
		       zz{ii}=log(zz{ii});
		       end;
		    end; * close k loop;
		    drop ii;
		%end; * if logxx=T;
	run;

	proc sort data=sadtds;
	by &bylev date;
	run;

	* Compute the means for the variables specified by the vars argument;
	%if &center=F %then
	    %do;
	    %let mwvars=%dset(mw, num1=1, num2=&r);
	    proc means data=sadtds noprint;
	    by &bylev;
	    var &wvars;
	    output out=meands(keep=&bylev &mwvars) mean=&mwvars;
	    run;
	    %end; * if center=F;

	* Create the trend term(s);
	%if &ptrend>0 %then
	    %do;
	    data sadtds(drop=int lagdate);
	    set sadtds;
		lagdate=lag(date);
		by &bylev;
		   do;
		   if first.&bylev=1 then
		      do;
		      int=0;
		      trend1=1;
		      end;
		   if first.&bylev^=1 then
		      do;
		      int=intck(&_freq, lagdate, date);
		      trend1=trend1+int;
		      end;
		   end;
		   retain trend1;
		%do ii=2 %to &ptrend;
		    %do;
		    trend&ii=trend1**&ii;
		    %end;
		%end; * close ii loop;
	    run;
	    %end;

	* Include seasonal dummies;
	%if &seasonal=T %then
	    %do;
	    %seasonal(inds=sadtds, outds=sadtds, freq=&freq);
	    %end;

	* Detrend and/or deseasonalize the data;

	* Save the estimated coefficients in a data set;
	%if &parmds^=F %then
	    %do;
	    %if &seasonal=F %then
		%do;
		proc reg data=sadtds outest=parmds(keep=_depvar_ &xvars) noprint;
		by &bylev;
		model &wvars=&xvars;
		output out=sadtds r=&vars;
		run;
		%end;

	    %if &seasonal=T %then
		%do;
		proc reg data=sadtds outest=parmds(keep=_depvar_ &xvars) noprint;
		by &bylev;
		model &wvars=&xvars / noint;
		output out=sadtds r=&vars;
		run;
		%end;

		proc sort data=sadtds(keep=&bylev) out=_indexds nodupkey;
		by &bylev;
		run;

		%do ii=1 %to %nvar(&wvars);
		    %do;
		    %let _dvname=%scan(&wvars, &ii);
		    %let _dvname1=%scan(&vars, &ii);

		    data wds&ii(drop=_depvar_);
		    set parmds;
			if _depvar_="%upcase(&_dvname)";
			depvar="%upcase(&_dvname1)";
		    run;

		    data wds&ii;
		    merge wds&ii _indexds;
		    run;

		    %if &ii=1 %then
			%do;
			data &parmds;
			set wds&ii;
			run;
			%end; * if ii=1;

		    %if &ii>1 %then
			%do;
			proc append base=&parmds data=wds&ii force;
			run;
			%end; * if ii>1;

			%dsrm(ds=wds&ii);
		    %end;
		%end; * close ii loop;
		%dsrm(ds=_indexds);
	    %end; * if parmds^=F;

	* Do not save the estimated coefficients in a data set;
	%if &parmds=F %then
	    %do;
	    %if &seasonal=F %then
		%do;
		proc reg data=sadtds noprint;
		by &bylev;
		model &wvars=&xvars;
		output out=sadtds r=&vars;
		run;
		%end;

	    %if &seasonal=T %then
		%do;
		proc reg data=sadtds noprint;
		by &bylev;
		model &wvars=&xvars / noint;
		output out=sadtds r=&vars;
		run;
		%end;
	    %end; * if parmds=F;

	/* Add the mean(s) of variable(s) to detrended and/or deseasonalized
	   data. */
	%if &center=F %then
	    %do;
	    data sadtds(drop=&mwvars);
	    merge sadtds(in=a) meands(in=b);
	    by &bylev;
	    if a=1;
	       array zz{*} &vars;
	       array mzz{*} &mwvars;
	       %if &logxx=F %then
		   %do;
		   do ii=1 to dim(zz);
		      do;
		      zz{ii}=zz{ii}+mzz{ii};
		      end;
		   end; * close ii loop;
		   drop ii;
		   %end;
	       %if &logxx=T %then
		   %do;
		   do ii=1 to dim(zz);
		      do;
		      zz{ii}=exp(zz{ii}+mzz{ii});
		      end;
		   end; * close ii loop;
		   drop ii;
		   %end;
	    run;

	    %dsrm(ds=meands);
	    %end; * if center=F;

	%if &incl=T %then
	    %do;
	    %if &byvars^=F %then
		%do;
		data &outds;
		merge &inds(in=a drop=&vars)
		      sadtds(in=b keep=&bylev date &vars);
		by &bylev;
		if a=1;
		   %if &logxx=T and &center=T %then
		       %do;
		       array zz{*} &vars;
		       do ii=1 to dim(zz);
			  do;
			  zz{ii}=exp(zz{ii});
			  end;
		       end; * close ii loop;
		       drop ii;
		       %end; * if logxx=T;
		run;
		%end; * if byvars^=F;

	    %if &byvars=F %then
		%do;
		data &outds;
		merge &inds(in=a drop=&vars) sadtds(in=b keep=date &vars);
		by date;
		if a=1;
		   %if &logxx=T and center=T %then
		       %do;
		       array zz{*} &vars;
		       do ii=1 to dim(zz);
			  do;
			  zz{ii}=exp(zz{ii});
			  end;
		       end; * close ii loop;
		       drop ii;
		       %end; * if logxx=T;
		run;
		%end; * if byvars=F;
	    %end; * if incl=T;

	%if &incl=F %then
	    %do;
	    data &outds;
	    set sadtds(keep=&bylev date &vars);
	    run;
	    %end; * if incl=F;

	%dsrm(ds=sadtds);

	* Re-set options;
	options notes;

%mend sadt;

***************************************************************************;
/* Purpose: To compute the necessary statistics for data disclosures:
		
inds............name of the data set for which the disclosure must 
		be obtained.
outds...........name of the data set which will contain all the 
		necessary statistics.
crvar...........name of the variable for which the concentration 
		ratios will be computed.
byvars..........name(s) of the variable(s) which determine the cells 
		for which the necessary statistics will computed. If 
		BYVARS=F, the INDS data set must contain a pure 
		cross-section of firms indexed by sernum. */

%macro disclose(inds, outds, crvar, byvars); 

	%local ncells ii byvname;

	%if &byvars=F %then 
	    %do;
	    %let byvars=;
	    %let ncells=0;
	    %end;

	/* Create a macro variable for each variable in the byvars 
	   argument. */
	%if %length(&byvars)>0 %then
	    %do;
	    %let ncells=%nvar(&byvars);
	    %do ii=1 %to &ncells;
		%do;
		%let byvname=%scan(&byvars, &ii);
		%let byvar&ii=&byvname;
		%end;
	    %end; * close ii loop;
	    %end;

	options compress=no;

	data _dtemp;
	set &inds(keep=&byvars &crvar);
	run;


	%if %length(&byvars)>0 %then
	    %do;
	    proc sort data=_dtemp;
	    by &byvars descending &crvar;
	    run;

	    * Compute the number of observations in each cell;      
	    proc univariate data=_dtemp noprint;
	    by &byvars;
	    var &crvar;
	    output out=nds(keep=&byvars N) n=N;
	    run;

	    * Compute the concentration ratios for each cell;
	    data _dtemp;
	    set _dtemp;
		by &byvars;
		   do;
		   if first.&&byvar&ncells=1 then
		      do;
		      rank=1;
		      st=&crvar;
		      end;
		   if first.&&byvar&ncells^=1 then
		      do;
		      rank=rank+1;
		      st=st+&crvar;
		      end;
		   end;
		retain rank st;
	    run;

	    data _dtemp;
	    set _dtemp;
		by &byvars;
		   do;
		   if first.&&byvar&ncells=1 then
		      do;
		      s2=&crvar;
		      s6=&crvar;
		      s10=&crvar;
		      end;
		   if first.&&byvar&ncells^=1 then
		      do;
		      if rank<=2 then
			 do;
			 s2=s2+&crvar;
			 end;
		      if rank<=6 then
			 do;
			 s6=s6+&crvar;
			 end;
		      if rank<=10 then
			 do;
			 s10=s10+&crvar;
			 end;
		      end;
		   end;
		retain s2 s6 s10;
		cr2=s2/st;
		cr6=s6/st;
		cr10=s10/st;
	    run;

	    data _dtemp;
	    set _dtemp;
		by &byvars;
		   do;
		   if last.&&byvar&ncells=1;
		   end;
	    run;

	    data &outds(keep=&byvars n cr2 cr6 cr10);
	    merge nds _dtemp;
	    by &byvars;
	    run;
	    %end; * if length(byvars)>0;

	%if %length(&byvars)=0 %then
	    %do;
	    * Compute the number of observations in the data set;
	    proc univariate data=_dtemp noprint;
	    var &crvar;
	    output out=nds(keep=N) n=N;
	    run;

	    * Compute the concentration ratios for the data set;
	    proc sort data=_dtemp;
	    by descending &crvar;
	    run;
	    
	    data _dtemp;
	    set _dtemp;
		if _n_=1 then
		   do;
		   st=&crvar;
		   s2=&crvar;
		   s6=&crvar;
		   s10=&crvar;
		   end;
		if _n_>1 then
		   do;
		   st=st+&crvar;
		   end;
		if _n_<=2 then
		   do;
		   s2=s2+&crvar;
		   end;
		if _n_<=6 then
		   do;
		   s6=s6+&crvar;
		   end;
		if _n_<=10 then
		   do;
		   s10=s10+&crvar;
		   end;
		retain st s2 s6 s10;
		cr2=s2/st;
		cr6=s6/st;
		cr10=s10/st;
	    run;

	    proc sort data=_dtemp;
	    by &crvar;
	    run;

	    data _dtemp;
	    set _dtemp;
		if _n_=1;
	    run;

	    data &outds(keep=n cr2 cr6 cr10);
	    merge nds _dtemp;
	    run;
	    %end; * if length(byvars)=0;

%mend disclose;

***************************************************************************;
/* Purpose: To uncompresses a UNIX compressed SAS data set(s):
		
from............UNIX path to the directory containing the 
		compressed data set(s).
datasets........name of the SAS data set(s) to be uncompressed.
		WARNING: Do not include .ssd01.Z extension in 
		the name.
to..............UNIX path to the directory to which to write 
		the uncompressed data set(s).
del=T...........specifes that the original compressed data 
		set(s) is to be deleted after it has been 
		uncompressed. */

%macro uncomp(from, datasets, to, del=F);

	%local ii compds xx yy;

	%do ii=1 %to %nvar(&datasets);
	    %do;
	    %let compds=%scan(&datasets, &ii);
	    %let xx=%unquote(&from&compds).ssd01.Z;
	    %let yy=%unquote(&to&compds).ssd01;

	    x "zcat &xx > &yy";

	    %if &del=T %then
		%do;
		x "rm &xx";
		%end;
	    %end;
	%end; * close ii loop;

%mend uncomp;

***************************************************************************;
/* Purpose: To UNIX compress a SAS data set(s):
		
from............UNIX path to the directory containing the 
		uncompressed data set(s).
datasets........name of the SAS data set(s) to be compressed.
to..............UNIX path to the directory to which to write 
		the compressed data set(s).
del=T...........specifes that the original uncompressed data 
		set(s) is to be deleted after it has been 
		compressed. */

%macro comp(from, datasets, to, del=F);

	%local ii uncompds xx yy;

	%do ii=1 %to %nvar(&datasets);
	    %do;
	    %let ucompds=%scan(&datasets, &ii);
	    %let xx=%unquote(&from&ucompds).ssd01;
	    %let yy=%unquote(&to&ucompds).ssd01.Z;

	    x "compress -c &xx > &yy";

	    %if &del=T %then
		%do;
		x "rm &xx";
		%end;
	    %end;
	%end; * close ii loop;

%mend comp;

***************************************************************************;
/* Purpose: To move SAS data set(s) using the UNIX command mv:
		
from............UNIX path to the directory containing the SAS 
		data set(s) which is to be moved.
datasets........name of the SAS data set(s). 
		WARNING: Do not append .ssd01 or .ssd01.Z extension 
			 when specifying the name of the data set(s) 
			 which is to be moved .  
to..............UNIX path to the directory to which to write the SAS 
		data set(s). 
compress=T......specifies that the data set(s) which is to be moved 
		are UNIX compressed. */

%macro move(from, datasets, to, compress=F);

	%local newdir ii xx sasds;

	%let newdir=%unquote(&to);

	%do ii=1 %to %nvar(&datasets);
	    %do;
	    %if &compress=T %then 
		%do;
		%let xx=%scan(&datasets, &ii);
		%let sasds=%unquote(&from&xx).ssd01.Z;
		%end;

	    %if &compress=F %then 
		%do;
		%let xx=%scan(&datasets, &k);
		%let sasds=%unquote(&from&xx).ssd01;
		%end;
	    
	    x "mv &sasds  &newdir";

	    %end;
	%end; * close ii loop;

%mend move;

***************************************************************************;
/* Purpose: This program creates a data set of a time series
	    of statistics (i.e. means, medians, etc. over time) 
	    for the specified variables. 
	    
inds............specifies the name of the data set containing 
		the variables for which to compute the time
		series of statistics.
stat............specifies the statistic to be computed.
		NOTE: See procedure UNIVARIATE (p. 621) for the 
		      valid keywords and the statistics they 
		      represent.
outds...........specifies the name of the output data set 
		containing the time series of statistics.
vars............names of the variables for which to compute the
		time series of statistics.
byvars..........computes the time-series of statistics on the 
		groups of variables defined by the BYVARS 
		variables.
ptrend..........the highest power of the trend term in the 
		regression;
seasonal=T......deseasonalizes the data by including seasonal 
		dummies in the regression.
freq............specifies the frequency of the data:
		Q=quarterly (default)
		M=monthly
center=T........centers the detrended and/or deseasonalized 
		variable(s) to mean zero.
logxx=T.........converts the data to natural logarithms prior 
		to any transformations. The seasonally adjusted 
		and/or detrended log level data are exponentiated 
		before they are written to the OUTDS data set.
		NOTE: If CENTER=F and LOGXX=T, it is the mean of 
		      the log level is added to the seasonally 
		      adjusted and/or detrended data prior to 
		      exponentiating.
na_rm=T.........removes (i.e. windows the data) all missing 
		observations prior to any transformations. */

%macro ts_stat(inds, outds, vars, stat, byvars=F, seasonal=F, 
	       ptrend=0, logxx=F, center=F, freq=Q, na_rm=F);

	%local _byvars;

	%if &byvars=F %then 
	    %do;
	    %let _byvars=;
	    %end;

	%if &byvars^=F %then 
	    %do;
	    %let _byvars=&byvars;
	    %end;

	data _statds;
	set &inds(keep=date &_byvars &vars);
	    %if &byvars^=F %then
		%do;
		if &_byvars=. then delete;
		%end;
	run;

	proc sort data=_statds;
	by &_byvars date;
	run;

	proc univariate data=_statds noprint;
	by &_byvars date;
	var &vars;
	output out=&outds(keep=&_byvars date &vars) &stat=&vars;
	run;

	%if &seasonal=T or &ptrend>0 or &logxx=T %then
	    %do;
	    %sadt(inds=&outds, outds=&outds, vars=&vars, byvars=&byvars,
		  seasonal=&seasonal, ptrend=&ptrend, logxx=&logxx, 
		  na_rm=&na_rm, center=&center, freq=&freq);
	    %end;

%mend ts_stat;

***************************************************************************;
/* Purpose: To create a SAS transport file(s).
		
from............specifies the SAS library containing the SAS data 
		set(s) that are to be converted to transport files.
tportlib........specifies the name of the xport library to which 
		the transport file(s) are written.
		NOTE: The default is TPORTLIB=TPORT.
tpath...........specifes the UNIX path to the TPORTLIB library.
		NOTE: The default is TPATH=/BKS//HOME/M1EXZ00/TPORT/.
datasets........specifies the name(s) of a SAS data set(s) that 
		are to be converted to transport file(s).
outsfx..........specifies the suffix that is appended to the 
		name(s) of the transport file(s). */

%macro tport(from, datasets, tportlib=tport, tpath=/bks/home/m1exz00/tport/,
	     outsfx=F);

	%local ii inds outfile;

	%if &outsfx=F %then %let outsfx=;

	%do ii=1 %to %eval(%nvar(&datasets));
	    %do;
	    %let inds=%scan(&datasets, &ii);
	    %let outfile=&&tpath.&inds.&outsfx;
	    libname &tportlib xport "&outfile";

	    proc copy in=&from out=&tportlib;
	    select &inds; 			
	    run;
	    %end;
	%end; * close ii loop;

%mend tport;

***************************************************************************;
/* Purpose: To create a SAS data set(s) from a SAS transport 
	    file(s).
		
tportlib........specifies the name of the xport library containing
		the transport file(s).
		NOTE: The default is TPORTLIB=TPORT.
tpath...........specifes the UNIX path to the TPORTLIB library.
		NOTE: The default is TPATH=/BKS/HOME/M1EXZ00/TPORT/.
datasets........specifies the name(s) of a transport data set(s) 
		that are to be converted to SAS data set(s).
		NOTE: If copying multiple data sets, they must all 
		      be in the same directory. 
		WARNING: Transport files cannot have extension 
			 appended to their name(s) (for example 
			 .dat, etc.).
to..............specifies the name of the SAS library to which the 
		SAS data set(s) are written. */

%macro untport(datasets, to, tportlib=tport, tpath=/bks/home/m1exz00/tport/);

	%local ii inds infile;

	%do ii=1 %to %eval(%nvar(&datasets));
	    %do;
	    %let inds=%scan(&datasets, &ii);
	    %let infile=&&tpath.&inds;
	    libname &tportlib xport "&infile";

	    proc copy in=&tportlib out=&to;
	    run;
	    %end;
	%end; * close ii loop;

%mend untport;


***************************************************************************;
/* Purpose: To create a version 6 SAS data set(s) from version 8.1 SAS
	    data set(s).
		
from............specifies the SAS (v8) library containing the version
	        8.1 SAS data set(s) that are to be converted to SAS
		version 6 data set(s).
sas6lib.........specifies the name of the v6 SAS library to which the
		the version 6 data set(s) are written.
		NOTE: The default is SAS6LIB=SAS6LIB.
libpath.........specifes the UNIX path to the SAS6LIB library.
		NOTE: The default is PATH=/BKS//HOME/M1EXZ00/SAS6LIB/.
datasets........specifies the name(s) of the version 8 SAS data set(s) 
	        that is to be converted to version 6 data set(s). */

%macro sas8to6(from, datasets, sas6lib=sas6lib, 
               libpath=/bks/home/m1exz00/sas6lib/);

	%local ii dsname;

	libname &sas6lib v6 "&libpath";

	%do ii=1 %to %eval(%nvar(&datasets));
	    %do;
	    %let dsname=%scan(&datasets, &ii);

	    data &&sas6lib..&dsname;
	    set &&from..&dsname;
	    run;
	    %end;
	%end; * close ii loop;

%mend sas8to6;


***************************************************************************;
/* Purpose: To create a macro variable of the following form:
	    f1**1 + f2**2 + ... + fN**2  
	    The expression is a sum of squares of elements
	    f1, f2, ..., fN and is used in SAS/IML to define 
	    an objective function of a nonlinear optimization
	    problem. */  

%macro lsobj(neqns, fname=f);

	%do ii=1 %to &neqns;
	    %do;
	    %let t&ii=&fname&ii.**2;
	    %if &ii=1 %then %str(&&t&ii);
	    %else
	    %str(+ &&t&ii);
	    %end;
	%end;

%mend lsobj;


***************************************************************************;
/* Purpose: To create the right-hand side equation of the VAR.
	    Use in PROC MODEL to create a RHS of VAR equations.
	    NOTE: Only one equation of the VAR is created by 
		  the invocation of the macro VAReq.

ARGUMENTS:
vars........specifies the variables in the VAR.
nlags.......specifies the order of the VAR.
coeff.......specifies the coefficients of the VAR equation.
lagfn=T.....specifies that the syntax of the RHS equation is created 
            using the lag#() function notation. If LAGFN=F then the 
	    syntax is created using the namesof the VARS argument. */

%macro VAReq(vars, nlags, coeff, lagfn=T);

        %local _dimvar nn ii _parmindex _parm _var;

        /* Dimension of the VAR system: */
	%let _dimvar=%nvar(&vars);

        /* Create a RHS equation of the VAR system using the 
	   lag#() function notation: */
        %if &lagfn=T %then
	    %do;
	    %do nn=1 %to &nlags;
		%do;
		%do ii=1 %to &_dimvar;
		    %do;
		    %let _parmindex=%eval(&ii + %eval(%eval(&nn - 1)*&_dimvar));
		    %let _parm=&coeff._&_parmindex;
		    %let _var=%scan(&vars, &ii);
		    %let t&_parmindex=&_parm*lag&nn(&_var);
		    %if &_parmindex=1 %then %str(&&t&_parmindex);
		    %else %str(+ &&t&_parmindex);
		    %end;
                %end;
	        %end;
            %end;
	    %end;

        /* Create a RHS equation of the VAR system using the 
	   VARS name notation: */
        %if &lagfn=F %then
	    %do;
	    %do nn=1 %to &nlags;
		%do;
		%do ii=1 %to &_dimvar;
		    %do;
		    %let _parmindex=%eval(&ii + %eval(%eval(&nn - 1)*&_dimvar));
		    %let _parm=&coeff._&_parmindex;
		    %let _var=%scan(&vars, &ii);
		    %let t&_parmindex=&_parm*&_var.&_parmindex;
		    %if &_parmindex=1 %then %str(&&t&_parmindex);
		    %else %str(+ &&t&_parmindex);
		    %end;
                %end;
	        %end;
            %end;
	    %end;


%mend VAReq;


***************************************************************************;
/* Purpose: To create a bootstrap sample by random sampling
	    with replacement.

ARGUMENTS:
inds........name of the input data set from which the bootstrap 
	    sample is drawn by random sampling with replacement.
	    NOTE: The bootstrap sample contains all the variables
		  in the INDS data set.
bootds......name of the bootstrap data set. */


%macro bootsample(inds, bootds);

       data &bootds;
       do _i=1 to nobs;
       _pt=round(ranuni(0)*nobs) ;
       set &inds nobs=nobs point=_pt;
       output;
       end;
       stop;  
       drop _i;     
       run;


%mend bootsample;


***************************************************************************;
/* Purpose: To create a macro variable of the form X*VAR1 X*VAR2...X*VARk,
	    where WITH=X. */ 

%macro interact(vars, with);

	%local ii _var _parm;

	%do ii=1 %to %nvar(&vars);
	    %do;
	    %let _var=%scan(&vars, &ii);
	    %let t&ii=&with*&_var;
	    %if &ii=1 %then %str(&&t&ii);
	    %else %str(&&t&ii);
	    %end;
	%end;

%mend interact;


***************************************************************************;
/* Purpose: To create a macro variable used in CONTRAST statement,
	    in which a test of the joint significance of explanatory
	    variables in a linear regression is computed.
	    NOTE: If VARS = X1 X2 X3, the macro creates the following 
		  list X1 1, X2 1, X3 1  */

%macro contrast(vars);
       
        %local _vnum ii _vname;

	%let _vnum=%nvar(&vars);

	%if &_vnum=1 %then
	    %do;
	    %str(&vars 1);
	    %end;

	%if &_vnum>1 %then 
	    %do;
	    %do ii=1 %to &_vnum;
	        %do;
		%let _vname=%scan(&vars, &ii);
	        %if &ii<&_vnum %then %str(&_vname 1,);
	        %else %str(&_vname 1);
	        %end;
	    %end;
	    %end;

%mend contrast;
