* (C) Copyright 2007 Egon Zakrajsek;
* All Rights Reserved;

/****************************************************************************/
/* Purpose: This file contains macros for estimation of linear              */
/*	    panel data models. The file includes the following              */
/*	    macros:                                                         */ 
/*	     1) DEVMEAN.sas                                                 */
/*           2) ORTHODEV.sas                                                */
/*           3) PDIFF.sas                                                   */
/*	     4) PDSELECT.sas                                                */
/*           5) PLAG.sas                                                    */
/* 	     6) PLEAD.sas                                                   */
/*           7) BALPDS.sas                                                  */
/*           8) PDXTRM.sas                                                  */
/*           9) PDSUMMARY.sas                                               */
/*          10) TSCSds.sas                                                  */
/*                                                                          */
/* Originally Written: April 1997                                           */
/* Last Modified: 08-24-07                                                  */
/****************************************************************************/


/****************************************************************************/ 
/*	NOTE: The following notation is used:                               */
/*	      N = number of unique cross-sectional units in the panel.      */
/*	      T = number of unique time periods in the panel.               */
/*	      NT = total number of observations in the panel.               */
/****************************************************************************/


/******************************PROGRAMS**************************************/

/*********************************************************************/
/* Purpose: To sweep (i.e., set to zero) various means               */
/*	    (e.g., cross-sectional, time-series, etc.) from a        */
/*	    panel data set.                                          */
/*                                                                   */
/* Originally Written: 4-14-97                                       */
/*********************************************************************/

/* ARGUMENTS:
inds............input data set. (The INDS data set can be an 
		unbalanced panel with gaps in the time-series 
		dimension.)
byvars..........sweeps the mean to zero for observations in groups 
		defined by the BYVARS variables. If BYVARS=F, the 
		INDS data set has no cross-sectional dimension--the 
		data are viewed as time-series observations only--
		and the time mean of the time-series is set to zero.
vars............variables for which the mean is set to zero.
outds...........the name of the output data set. 
sorting=T.......indicates that SAS internal indexes for the BYVARS
		variables do not exists; therefore, the INDS data 
		set must be first sorted by the BYVARS argument. */

%macro DEVMEAN(inds, byvars, vars, outds, sorting=T);
	
	/* Sorting is necessary. */
	%if &sorting=T %then
	    %do;
	    %if &byvars=F  %then 
	        %do;
	        proc standard data=&inds out=&outds mean=0 noprint;
	        var &vars;
	        run;
	        %end; * if byvars=F;

	    %if &byvars^=F  %then 
	        %do;
		proc sort data=&inds;
		by &byvars;
		run;

	        proc standard data=&inds out=&outds mean=0 noprint;
	        by &byvars;
	        var &vars;
	        run;
	        %end; * if byvars^=F;
	    %end; * if sorting=T;


	/* Sorting is not necessary. */
	%if &sorting=F %then
	    %do;
	    %if &byvars=F  %then 
	        %do;
	        proc standard data=&inds out=&outds mean=0 noprint;
	        var &vars;
	        run;
	        %end; * if byvars=F;

	    %if &byvars^=F  %then 
	        %do;
	        proc standard data=&inds out=&outds mean=0 noprint;
	        by &byvars;
	        var &vars;
	        run;
	        %end; * if byvars^=F;
	    %end; * if sorting=F;

%mend DEVMEAN;



/********************************************************************/
/* Purpose: Orthogonal (Helmert) deviation transformation for       */
/*	    variables in a panel data set; see Arellano and         */
/*	    Bower (1995) for further details.                       */
/*                                                                  */
/* Originally Written: April 1997                                   */
/* Last Modified: 4-19-97                                           */
/********************************************************************/

/* ARGUMENTS:
inds............name of the data set containing the variables
		that are to be transformed. The INDS data set 
		can be an unbalanced panel with gaps in the 
		time-series dimension.
csindex.........name of the variable in the INDS data set that 
		indexes the cross-sectional dimension of the 
		panel.
		NOTE: The cross-sectional index must be a NUMERIC
		      variable; the default is CSINDEX=SERNUM.
tsindex.........name of the variable in the INDS data set that 
		indexes the time-series dimension of the panel.
		NOTE: The time-series index must be a SAS date 
		      variable; the default is TSINDEX=DATE.
freq............specifies the (time-series) frequency of the 
		INDS data set:
			M = monthly data
			Q=quarterly data (this is the default)
			Y=annual data
outds...........name of the data set containing the transformed
		variables.
vars............names of the variables in the INDS data set
		that are to be transformed using orthogonal 
		deviations.
incl=T..........specifies that the OUTDS data set should include
		all the variables from the INDS data set as well
		as the newly transformed variables.
na_rm=T.........removes the last time-series observation for 
		each cross-sectional unit.
		NOTE: By default, the last observation for each
		      cross-sectional unit is set to missing. */

%macro ORTHODEV(inds, outds, vars, incl=T, na_rm=F, csindex=sernum, 
		tsindex=date, freq=Q);


	%if &freq=Q %then %let _freq='qtr';
	%if &freq=M %then %let _freq='month';
	%if &freq=Y %then %let _freq='year';

	%let _PP=%nvar(&vars);

	/* Create the following data sets:
	   _ORTHODS = data set of variables that are to be transformed.
	   _FRWDDS  = data set of tenure records for each individual 
		      cross-sectional unit. */
	data _orthods _frwdds(keep=&csindex _icount);
	set &inds(keep=&csindex &tsindex &vars);
            by &csindex;
               do;
               if first.&csindex = 1 then _icount = 1;
               else _icount =_icount + 1;
	       if last.&csindex = 1 then output _frwdds;
               end; 
               retain _icount;
	    output _orthods;
	run;


	/* Merge the tenure of each individual cross-sectional unit
	   back to the _ORTHODS data set:
           NOTE: This information is used to compute weights that equalize
                  the variances of the transformed variables. */
	data _orthods;
	merge _orthods(in=a) _frwdds(in=b rename=(_icount=_tenure));
	by &csindex;
	if a=1 and b=1;
	   /* Variance equalization weights: */
	   _varwg = ((_tenure-_icount)/(_tenure-_icount+1));
	run;

	/* Compute the maximum tenure in the panel: */
	proc univariate data=_frwdds(drop=&csindex) noprint;
	var _icount;
	output out=_fmaxds max=_fmax;
	run;

	data _null_;
	set _fmaxds;
	    call symput('_fmax', left(put(_fmax, 12.)));
	run;	

	%dsrm(ds=_frwdds _fmaxds);


	/* Transpose the _ORTHODS data set: */
	proc transpose data=_orthods out=_temp prefix=f;
	by &csindex;
	var date &vars;
	run;

	/* Compute the forward means of all variables: */
	data _temp;
	set _temp;
	    %let _miter = &_fmax;
	    %do nn=2 %to %eval(&_miter);
		%do;
		_m&nn = mean(of f&nn-f&_miter);
		%end;
	    %end; * close nn loop;
	run;

	/* Re-transpose the data set back to panel design: */
	proc transpose data=_temp out=_temp;
	by &csindex;
	var _m2-_m&_miter;
	run;

	data _temp(keep=&csindex &vars);
	set _temp;
	    where &tsindex^=.;
	run;

	/* Create the appropriate time-series index for the 
	   forward means: */
	data _tsds;
	set _orthods(keep=&csindex &tsindex);
	    by &csindex;
	       do;
	       if last.&csindex ^= 1;
	       end;
	run;

	data _temp(rename=(%renamer(&vars, _m)));
	merge _temp(in=a) _tsds(in=b);
	by &csindex;
	if a=1 and b=1;
	run;

	data _orthods;
	merge _orthods(in=a) _temp(in=b);
	by &csindex &tsindex;
	if a=1;
	run;

	%dsrm(ds=_temp _tsds);


	/* Compute orthogonal deviations: */
	%if &incl=F %then
	    %do;
	    data &outds(keep=&csindex &tsindex &vars);
	    set _orthods;
	        /* Advance the time series index forward one period: */
		&tsindex = intnx(&_freq, &tsindex, 1);
	        array tv{0:%eval(&_PP-1)} &vars;
	        array fm{0:%eval(&_PP-1)} _m1-_m&_PP;
	        do kk=0 to %eval(&_PP-1);
	           do;
	           by &csindex;
	    	      do;
	    	      if last.&csindex ^= 1 then
	    	         do;
	    	         tv{kk} = (tv{kk}-fm{kk})*sqrt(_varwg);
	    	         end;
	    	      if last.&csindex = 1 then
	    	         do;
	    	         tv{kk} = .;
	    	         end;
	    	      end; 
	           end;
	        end; * close kk loop;
	    run;

	    /* Delete the last time-series observation for each
	       cross-sectional unit: */
	    %if &na_rm=T %then
		%do;
		data &outds;
		set &outds;
		    by &csindex;
		       do;
		       if last.&csindex = 1 then delete;
		       end;
		run;
		%end; * if na_rm=T;

	    %dsrm(ds=_orthods);
	    %end; * if incl=F;

	%if &incl=T %then
	    %do;
	    data _orthods(keep=&csindex &tsindex &vars);
	    set _orthods;
	        array tv{0:%eval(&_PP-1)} &vars;
	        array fm{0:%eval(&_PP-1)} _m1-_m&_PP;
	        do kk=0 to %eval(&_PP-1);
	           do;
	           by &csindex;
	    	      do;
	    	      if last.&csindex ^= 1 then
	    	         do;
	    	         tv{kk} = (tv{kk}-fm{kk})*sqrt(_varwg);
	    	         end;
	    	      if last.&csindex = 1 then
	    	         do;
	    	         tv{kk}=.;
	    	         end;
	    	      end; 
	           end;
	        end; * close kk loop;
	    run;

	    data &outds;
	    merge &inds(in=a) _orthods(in=b);
	    by &csindex &tsindex;
	    if a=1 and b=1;
	        /* Advance the time series index forward one period: */
		&tsindex = intnx(&_freq, &tsindex, 1);
	    run;

	    /* Delete the last time-series observation for each
	       cross-sectional unit: */
	    %if &na_rm=T %then
		%do;
		data &outds;
		set &outds;
		    by &csindex;
		       do;
		       if last.&csindex = 1 then delete;
		       end;
		run;
		%end; * if na_rm=T;

	    %dsrm(ds=_orthods);
	    %end; * if incl=T;

        
%mend ORTHODEV;



/********************************************************************/
/* Purpose: To difference (numeric) variables in a panel data set.  */
/*                                                                  */
/* Originally Written: 4-14-97                                      */
/********************************************************************/

/* ARGUMENTS:			
inds............input panel data set. The INDS data set can be 
		an unbalanced panel with gaps in the time-series 
		dimension.
outds...........specifies the name of the output data set that 
		contains the differenced variables.
diff............specifies the difference to be created (e.g., 
		diff=m specifies that the m-th difference--that
		is, x(t)-x(t-m)--will be created).
vars............variables for which to create the specified 
		difference. 
		NOTE: The naming convention for the differenced
		      variables is as follows:
		      dx(t) = x(t)-x(t-1): 1st difference of x(t)
		      d2x(t) = x(t)-x(t-2): 2nd difference of x(t)
		      dmx(t) = x(t)-x(t-m): m-th difference of x(t)
		NOTE: As a consequence of this naming convention, 
		      the names of variables in the VARS argument 
		      cannot exceed 7 characters if creating the 
		      first difference only and cannot exceed 6 
		      characters if creating a difference greater 
		      than or equal to 2. 
incl=T..........specifies that the OUTDS data set contains all the 
		variables from the INDS data set, as well as the 
		newly created differenced variables.
na_rm=T.........removes observations that are set to missing due 
		to differencing for each cross-sectional unit in 
		the OUTDS data set.
csindex.........name of the variable in the INDS data set that 
		indexes the cross-sectional dimension.
		NOTE: The default is SERNUM.
tsindex.........name of the variable in the INDS data set that 
		indexes the time-series dimension.
		NOTE: The time-series index must be a SAS date 
		      value; the default is a variable named DATE.
freq............specifies the frequency of the data:
		Q=quarterly (default)
		M=monthly
		Y=yearly  */

%macro PDIFF(inds, outds, vars, diff, na_rm=F, incl=T, csindex=sernum,
	     tsindex=date, freq=Q);

	/* Declare local macro variables. */
	%local _dpfx _dvars;

	* Create a necessary lag for the difference;
	%plag(inds=&inds, outds=_diffds, vars=&vars, lags=&diff,
	      incl=F, na_rm=&na_rm, num=T, name=L, csindex=&csindex,
	      tsindex=&tsindex, freq=&freq);

	%if &diff=1 %then
	    %do;
	    %let _dpfx=d;
	    %end;
	%if &diff>1 %then
	    %do;
	    %let _dpfx=d&diff;
	    %end;
	%let _dvars=%varpfx(&vars, &_dpfx);

	%if &incl=F %then
	    %do;
	    data &outds(keep=&csindex &tsindex &_dvars);
	    set _diffds;
		array _xx{*} &vars;
		array _lxx{*} %varsfx(&vars, _&diff);
		array _dxx{*} &_dvars;
		do ii=1 to dim(_xx);
		   do;
		   _dxx{ii}=_xx{ii}-_lxx{ii};
		   end;
		end; * close ii loop;
	    run;
	
	    %dsrm(ds=_diffds);
	    %end; * if incl=F;

	%if &incl=T %then
	    %do;
	    data _diffds(keep=&csindex &tsindex &_dvars);
	    set _diffds;
		array _xx{*} &vars;
		array _lxx{*} %varsfx(&vars, _&diff);
		array _dxx{*} &_dvars;
		do ii=1 to dim(_xx);
		   do;
		   _dxx{ii}=_xx{ii}-_lxx{ii};
		   end;
		end; * close ii loop;
	    run;
	
	    data &outds;
	    merge &inds(in=a) _diffds(in=b);
	    by &csindex &tsindex;
	    if a=1 and b=1;
	    run;

	    %dsrm(ds=_diffds);
	    %end; * if incl=T;
	
%mend PDIFF;



/***********************************************************************/
/* Purpose: To extract a sub-panel with minimum continuous             */
/*	    tenure from an unbalanced panel data set with              */
/*	    (possible) gaps in the time-series dimension.              */
/*                                                                     */
/* Originally Written: 4-15-97                                         */
/***********************************************************************/

/* ARGUMENTS:	    
inds............name of the input panel data set.
		NOTE: The INDS data set can be an unbalanced
		      panel with gaps in the time-series
		      dimension. It must, however, be sorted
		      by the cross-sectional and time-series
		      indexes.
outds...........name of the sub-panel which is extracted from
		the INDS data set.
tenure..........specifies the minimum continuous tenure for
		each cross-sectional unit in the outds data
		set (i.e., if TENURE=p, only cross-sectional 
		units with p or more continuous time-series
		observations are selected for the OUTDS data
		set).
balance=T.......selects a BALANCED panel.
		NOTE: If BALANCE=T and TENURE=p, the OUTDS
		      data set will contain only cross-
		      sectional units with 1,...,p continuous 
		      time-series observations. Time-series
		      observation p+1, p+2,... if they exist, 
		      are deleted.
csindex.........name of a variable in the INDS data set that 
		indexes the cross-sectional dimension.
		NOTE: Default equals SERNUM.
tsindex.........name of a SAS date variable in the INDS data 
		set that indexes the time-series dimension.
		NOTE: Default equals DATE.
freq............specifies the frequency of the data:
		Q=quarterly (default)
		M=monthly
		Y=yearly  */

%macro PDSELECT(inds, outds, tenure, balance=F, csindex=sernum, 
		tsindex=date, freq=Q);

	/* Declare local macro variables. */
	%local _freq;

	* Define the frequency of the data;
	%if &freq=Q %then %let _freq='qtr';
	%if &freq=M %then %let _freq='month';
	%if &freq=Y %then %let _freq='year';

	/* Compute gaps and cumulative gaps in the time-series
	   dimension of each cross-sectional unit. */
	data _pdindex;
	%if &_freq=Q %then format _lag yyq4.;
	%if &_freq=M %then format _lag monyy5.;
	%if &_freq=Y %then format _lag year4.;
	set &inds(keep=&csindex &tsindex);
	    _lag=lag(&tsindex);
	    by &csindex;
	       do;
	       if first.&csindex then 
		  do;
		  * Gap;
		  _skip=0;
		  * Cumulative gap;
		  _cumskip=_skip;
		  end;
	       if first.&csindex^=1 then 
		  do;
		  if intck(&_freq, _lag, &tsindex)^=1 then 
		     do;
		     _skip=1;
		     _cumskip=_cumskip + _skip;
		     end;
		  if intck(&_freq, _lag, &tsindex)=1 then 
		     do;
		     _skip=0;
		     _cumskip=_cumskip + _skip;
		     end;
		  end;
	       end;
	       retain _cumskip;
	run;

	proc sort data=_pdindex;
	by &csindex &tsindex _cumskip;
	run;

	/* Compute a (partial) tenure record for each continuous
	   interval of data within each cross-sectional unit. */
	data _pdindex;
	set _pdindex;
	    by &csindex _cumskip;
	       do;
	       if first._cumskip=1 then
		  do;
		  _ptenure=1;
		  end;
	       if first._cumskip^=1 then
		  do;
		  _ptenure=_ptenure + 1;
		  end;
	       end;
	    retain _ptenure;
	run;

	data _ptenrds;
	set _pdindex(keep=&csindex _cumskip _ptenure);
	    by &csindex _cumskip;
	       do;
	       if last._cumskip=1;
	       end;
	run;

	* Merge the (partial) tenure record to the _pdindex data set;
	data _pdindex;
	merge _pdindex(in=a drop=_ptenure) _ptenrds(in=b);
	by &csindex _cumskip;
	if a=1 and b=1;
	run;

	/* Compute the longest (partial) tenure for each cross-sectional
	   unit. */
	proc univariate data=_ptenrds noprint;
	by &csindex;
	var _ptenure;
	output out=_slctds max=_ptenure;
	run;

	/* Select cross-sectional units with the specified minimum
	   continuous tenure. */
	data _slctds;
	set _slctds;
	    if _ptenure>=&tenure;
	run;

	proc sort data=_slctds;
	by &csindex _ptenure;
	run;

	proc sort data=_pdindex;
	by &csindex _ptenure;
	run;

	data _pdindex(keep=&csindex &tsindex _cumskip _ptenure);
	merge _pdindex(in=a) _slctds(in=b);
	by &csindex _ptenure;
	if a=1 and b=1;
	run;

	proc sort data=_pdindex;
	by &csindex _cumskip &tsindex;
	run;


	/* Eliminate ties (i.e., two or more continuous but 
	   dis-joint time-series segments of duration greater 
	   than equal to the specified tenure argument for one 
	   cross-sectional unit.) 
	   	NOTE: The first segment of proper tenure is 
		      kept. */ 
	proc sort data=_pdindex(keep=&csindex _cumskip) 
		  out=_slctds nodupkey;
	by &csindex _cumskip;
	run;

	data _slctds;
	set _slctds;
	    by &csindex;
	       do;
	       if first.&csindex=1;
	       end;
	run;

	data _pdindex(keep=&csindex &tsindex);
	merge _pdindex(in=a) _slctds(in=b);
	by &csindex _cumskip;
	if a=1 and b=1;
	run;

	data &outds;
	merge &inds(in=a) _pdindex(in=b);
	by &csindex &tsindex;
	if a=1 and b=1;
	run;

	* Balance the panel;
	%if &balance=T %then
	    %do;
	    data &outds(drop=_tenure);
	    set &outds;
		by &csindex;
		   do;
		   if first.&csindex=1 then _tenure=1;
		   else _tenure=_tenure + 1;
		   end;
		retain _tenure;
		if _tenure>&tenure then delete;
	    run;
	    %end; * if balance=T;

	%dsrm(ds=_pdindex _slctds _ptenrds);

%mend PDSELECT;



/*********************************************************************/
/* Purpose: To lag numeric or character variables in a panel data    */
/*	    set.                                                     */
/*                                                                   */
/* Originally Written: 4-14-97                                       */
/*********************************************************************/
		
/* ARGUMENTS:
inds............input panel data set. The INDS data set can be 
		an unbalanced panel with gaps in the time-series 
		dimension.
lags............specifies the lags to be created.
vars............variables for which to create the specified lags. 
name............specifies the naming convention for the lagged 
		variables. If NAME=L, the long name convention is 
		used. In that case, the name of a lagged variable 
		is the name of a variable with the _lag appended as 
		a suffix. (i.e., if variable is SALES then SALES_1 
		is the first lag of sales.) If NAME=S, the short 
		name convention is used. In that case, the name of 
		a lagged variable is the name of a variable with the 
		lag appended as a suffix. (i.e., if variable is SALES 
		then SALES1 is the first lag of sales.) 
		NOTE: Because of the naming convention, names of the 
		      variables in the VARS argument should not exceed 
		      seven characters if NAME=S and should not exceed 
		      six characters if NAME=L. 
incl=T..........specifies that the OUTDS data set contains all the
		variables from the INDS data set, as well as the newly 
		created lagged variables.
na_rm=T.........removes observations 1,2,...,max(lags) for each 
		cross-sectional unit in the OUTDS data set. 
num=T...........indicates that all the variables specified by the VARS
		argument are numeric variables. If NUM=F, then all the 
		variables specified by the VARS argument are character 
		variables.
csindex.........name of the variable in the INDS data set that indexes
		the cross-sectional dimension.
		NOTE: The default is SERNUM.
tsindex.........name of the variable in the INDS data set that indexes
		the time-series dimension.
		NOTE: The time-series index must be a SAS date value;
		      the default is DATE.
freq............specifies the time-series frequency of the data:
		Q=quarterly (default)
		M=monthly
		Y=yearly  */

%macro PLAG(inds, outds, vars, lags, num=T, name=S, incl=T, na_rm=F, 
	    csindex=sernum, tsindex=date, freq=Q);

	/* Declare local macro variables. */
	%local _freq _lsfx ii p;
	
	%if &freq=Q %then %let _freq='qtr';
	%if &freq=M %then %let _freq='month';
	%if &freq=Y %then %let _freq='year';

	%if &na_rm=T %then
	    %do;
	    %mmax(&lags);
	    %end;
	
	%if &incl=T %then
	    %do;
	    data _lagds(drop=kk counter _skip _lag %varsfx(&vars, t));
	    set &inds(keep=&csindex &tsindex &vars);
		%do ii=1 %to %nvar(&lags);
		    %do;
		    %let p=%scan(&lags, &ii);
		    %if &name=S %then %let lsfx=&p;
		    %if &name=L %then %let lsfx=_&p;
		    %let vars&p=%varsfx(&vars, &lsfx);
		    _date&p=lag&p(&tsindex);
		    _lag=lag(&tsindex);
		    %end;
		%end; * close ii loop;

		by &csindex;
		   do;
		   if first.&csindex then 
		      do;
		      counter=1;
		      _skip=0;
		      end;
		   else 
		      do;
		      counter=counter+1;
		      _skip=intck(&_freq, _lag, &tsindex);
		      end;
		   end;
		   retain counter;
 
		%if &num=T %then
		    %do;
		    array _x{*} &vars;
		    array _y{*} %varsfx(&vars, t);
		    %end;
 
		%if &num=F %then
		    %do;
		    array _x{*} $ &vars;
		    array _y{*} $ %varsfx(&vars, t);
		    %end;
 
		do kk=1 to dim(_x);
		   do;
		   _y{kk}=lag(_x{kk});
		   end;
		end; * close kk loop;
	    
		* Lag numeric variables;
		%if &num=T %then
		    %do;
		    %do ii=1 %to %nvar(&lags);
			%do;
			%let p=%scan(&lags, &ii);
			array _x&p{*} &&vars&p;
			do kk=1 to dim(_x);
			   do;
			   _x&p{kk}=lag&p(_x{kk});
			   if counter<=&p then _x&p{kk}=.;
			   if counter>&p then
			      do;
			      if _skip=1 then
				 do;
				 if intck(&_freq, _date&p, &tsindex)=&p then
				 _x&p{kk}=_x&p{kk};
				 else _x&p{kk}=.;
				 end; * if _skip=1;
			      if _skip^=1 then
				 do;
				 if intck(&_freq, _lag, &tsindex)=&p then
				 _x&p{kk}=_y{kk};
				 else _x&p{kk}=.;
				 end; * if _skip^=1; 
			      end; * if counter>&p;
			   end;
			end; * close kk loop;
			drop _date&p;           
			%end;
		    %end; * close ii loop;
		    %end; * if num=T;
 
		* Lag character variables;
		%if &num=F %then
		    %do;
		    %do ii=1 %to %nvar(&lags);
			%do;
			%let p=%scan(&lags, &ii);
			array _x&p{*} $ &&vars&p;
			do kk=1 to dim(_x);
			   do;
			   _x&p{kk}=lag&p(_x{kk});
			   if counter<=&p then _x&p{kk}='';
			   if counter>&p then
			   do;
			   if _skip=1 then
			      do;
			      if intck(&_freq, _date&p, &tsindex)=&p then
			      _x&p{kk}=_x&p{kk};
			      else _x&p{kk}='';
			      end; * if _skip=1;
			   if _skip^=1 then
			      do;
			      if intck(&_freq, _lag, &tsindex)=&p then
			      _x&p{kk}=_y{kk};
			      else _x&p{kk}='';
			      end; * if _skip^=1;
			   end; * if counter>&p;
			   end;
			end; * close kk loop;
			drop _date&p;           
			%end;
		    %end; * close ii loop;
		    %end; * if num=F;
 
		%if &na_rm=T %then
		    %do;
		    if counter<=&maxstr then delete;
		    %end;
	     run;
 
	     %if &na_rm=T %then 
		 %do;    
		 data &outds;
		 merge &inds(in=a) _lagds(in=b);
		 by &csindex &tsindex;
		 if a=1 and b=1;
		 run;
		 %end; * if na_rm=T;
 
	     %if &na_rm=F %then 
		 %do;    
		 data &outds;
		 merge &inds _lagds;
		 by &csindex &tsindex;
		 run;
		 %end; * if na_rm=F;
 
	     %dsrm(ds=_lagds);
	     %end; * if incl=T;
 
 
	%if &incl=F %then
	    %do;
	    data &outds(drop=kk counter _skip _lag %varsfx(&vars, t));
	    set &inds(keep=&csindex &tsindex &vars);
		%do ii=1 %to %nvar(&lags);
		    %do;
		    %let p=%scan(&lags, &ii);
		    %if &name=S %then %let lsfx=&p;
		    %if &name=L %then %let lsfx=_&p;
		    %let vars&p=%varsfx(&vars, &lsfx);
		    _date&p=lag&p(&tsindex);
		    _lag=lag(&tsindex);
		    %end;
		%end; * close ii loop;
 
		by &csindex;
		   do;
		   if first.&csindex then 
		      do;
		      counter=1;
		      _skip=0;
		      end;
		   else 
		      do;
		      counter=counter+1;
		      _skip=intck(&_freq, _lag, &tsindex);
		      end;
		   end;
		   retain counter;
    
		%if &num=T %then
		    %do;
		    array _x{*} &vars;
		    array _y{*} %varsfx(&vars, t);
		    %end;
 
		%if &num=F %then
		    %do;
		    array _x{*} $ &vars;
		    array _y{*} $ %varsfx(&vars, t);
		    %end;
 
		do kk=1 to dim(_x);
		   do;
		   _y{kk}=lag(_x{kk});
		   end;
		end; * close kk loop;
 
		* Lag numeric variables;
		%if &num=T %then
		    %do;
		    %do ii=1 %to %nvar(&lags);
			%do;
			%let p=%scan(&lags, &ii);
			array _x&p{*} &&vars&p;
			do kk=1 to dim(_x);
			   do;
			   _x&p{kk}=lag&p(_x{kk});
			   if counter<=&p then _x&p{kk}=.;
			   if counter>&p then
			      do;
			      if _skip=1 then
				 do;
				 if intck(&_freq, _date&p, &tsindex)=&p then
				 _x&p{kk}=_x&p{kk};
				 else _x&p{kk}=.;
				 end; * if _skip=1;
			      if _skip^=1 then
				 do;
				 if intck(&_freq, _lag, &tsindex)=&p then
				 _x&p{kk}=_y{kk};
				 else _x&p{kk}=.;
				 end; * if _skip^=1;
			      end; * if counter>&p;
			   end;
			end; * close kk loop;
			drop _date&p;           
			%end;
		    %end; * close ii loop;
		    %end; * if num=T;
 
		* Lag character variables;
		%if &num=F %then
		    %do;
		    %do ii=1 %to %nvar(&lags);
			%do;
			%let p=%scan(&lags, &ii);
			array _x&p{*} $ &&vars&p;
			do kk=1 to dim(_x);
			   do;
			   _x&p{kk}=lag&p(_x{kk});
			   if counter<=&p then _x&p{kk}='';
			   if counter>&p then
			      do;
			      if _skip=1 then
				 do;
				 if intck(&_freq, _date&p, &tsindex)=&p then
				 _x&p{kk}=_x&p{kk};
				 else _x&p{kk}='';
				 end; * if _skip=1;
			      if _skip^=1 then
				 do;
				 if intck(&_freq, _lag, &tsindex)=&p then
				 _x&p{kk}=_y{kk};
				 else _x&p{kk}='';
				 end; * if _skip^=1;
			      end; * if counter>&p;
			   end;
			end; * close kk loop;
			drop _date&p;           
			%end;
		    %end; * close ii loop;
		    %end; * if num=F;
 
		%if &na_rm=T %then
		    %do;
		    if counter<=&maxstr then delete;
		    %end;
	    run;
	    %end; * if incl=F;

%mend PLAG;



/**********************************************************************/
/* Purpose: To lead numeric or character variables in a panel data    */
/*	    set.                                                      */
/*                                                                    */
/* Originally Written: 4-14-97                                        */
/**********************************************************************/
	
/* ARGUMENTS:	
inds............input panel data set. The INDS data set can be 
		an unbalanced panel with gaps in the time-series 
		dimension.
lags............specifies the leads to be created.
vars............variables for which to create the specified leads.
name............specifies the naming convention for the leads of 
		the variables in the VARS argument. If NAME=L, the 
		long name convention is used. In that case, the name 
		of a lead is the name of a variable with the _lead 
		appended as a suffix. (i.e., if variable is SALES 
		then SALES_1 is the first lead of sales.) If NAME=S, 
		the short name convention is used. In that case, the 
		name of a lead is the name of a variable with the lead 
		appended as a suffix. (i.e., if variable is SALES then 
		sales1 is the first lead of SALES.) 
		NOTE: Because of the naming convention, names of the 
		      variables in the argument VARS should not exceed 
		      seven characters if NAME=S and should not exceed 
		      six characters if NAME=L. 
incl=T..........specifies that the OUTDS data set contains all the 
		variables from the INDS data set, as well as the newly 
		created leads. 
na_rm=T.........removes observations T-max(leads), T-max(leads)+1,...,T 
		for each cross-sectional unit in the OUTDS data set. 
num=T...........indicates that all the variables specified by the VARS
		argument are numeric variables. If NUM=F, then all the 
		variables specified by the VARS argument are character 
		variables.
csindex.........name of the variable in the INDS data set that indexes
		the cross-sectional dimension.
		NOTE: The default is SERNUM.
tsindex.........name of the variable in the INDS data set that indexes
		the time-series dimension.
		NOTE: The time-series index must be a SAS date value;
		      the default is DATE.
freq............specifies the time-series frequency of the data:
		Q=quarterly (default)
		M=monthly
		Y=yearly  */

%macro PLEAD(inds, outds, vars, leads, num=T, name=L, incl=T, na_rm=F, 
	     csindex=sernum, tsindex=date, freq=Q);

	/* Declare local macro variables. */
	%local _freq ii p lsfx;
	
	%if &freq=Q %then %let _freq='qtr';
	%if &freq=M %then %let _freq='month';
	%if &freq=Y %then %let _freq='year';

	%if &na_rm=T %then
	    %do;
	    %mmax(&leads);
	    %end;

	proc sort data=&inds out=_temp;
	by &csindex descending &tsindex;
	run;

	%if &incl=T %then
	    %do;
	    data _leadds(drop=kk counter _skip _lead %varsfx(&vars, t));
	    set _temp(keep=&csindex &tsindex &vars);
		%do ii=1 %to %nvar(&leads);
		    %do;
		    %let p=%scan(&leads, &ii);
		    %if &name=S %then %let lsfx=&p;
		    %if &name=L %then %let lsfx=_&p;
		    %let vars&p=%varsfx(&vars, &lsfx);
		    _date&p=lag&p(&tsindex);
		    _lead=lag(&tsindex);
		    %end;
		%end; * close ii loop;
		by &csindex;
		   do;
		   if first.&csindex then
		      do;
		      counter=1;
		      _skip=0;
		      end;
		   else 
		      do;
		      counter=counter+1;
		      _skip=abs(intck(&_freq, _lead, &tsindex));
		      end;
		   end;
		   retain counter;
    
		%if &num=T %then
		    %do;
		    array _x{*} &vars;
		    array _y{*} %varsfx(&vars, t);
		    %end;
 
		%if &num=F %then
		    %do;
		    array _x{*} $ &vars;
		    array _y{*} $ %varsfx(&vars, t);
		    %end;
 
		do kk=1 to dim(_x);
		   do;
		   _y{kk}=lag(_x{kk});
		   end;
		end; * close kk loop;
	    
		* Leads for the numeric variables;
		%if &num=T %then
		    %do;
		    %do ii=1 %to %nvar(&leads);
			%do;
			%let p=%scan(&leads, &ii);
			array _x&p{*} &&vars&p;
			do kk=1 to dim(_x);
			   do;
			   _x&p{kk}=lag&p(_x{kk});
			   if counter<=&p then _x&p{kk}=.;
			   if counter>&p then
			      do;
			      if _skip=1 then
				 do;
				 if abs(intck(&_freq, _date&p, &tsindex))=&p then
				 _x&p{kk}=_x&p{kk};
				 else _x&p{kk}=.;
				 end; * if _skip=1;
			      if _skip^=1 then
				 do;
				 if abs(intck(&_freq, _lead, &tsindex))=&p then
				 _x&p{kk}=_y{kk};
				 else _x&p{kk}=.;
				 end; * if _skip^=1; 
			      end; * if counter>&p;
			   end;
			end; * close kk loop;
			drop _date&p;           
			%end;
		    %end; * close ii loop;
		    %end; * if num=T;
 
		* Leads for the character variables;
		%if &num=F %then
		    %do;
		    %do ii=1 %to %nvar(&leads);
			%do;
			%let p=%scan(&leads, &ii);
			array _x&p{*} $ &&vars&p;
			do kk=1 to dim(_x);
			   do;
			   _x&p{kk}=lag&p(_x{kk});
			   if counter<=&p then _x&p{kk}='';
			   if counter>&p then
			   do;
			   if _skip=1 then
			      do;
			      if abs(intck(&_freq, _date&p, &tsindex))=&p then
			      _x&p{kk}=_x&p{kk};
			      else _x&p{kk}='';
			      end; * if _skip=1;
			   if _skip^=1 then
			      do;
			      if abs(intck(&_freq, _lead, &tsindex))=&p then
			      _x&p{kk}=_y{kk};
			      else _x&p{kk}='';
			      end; * if _skip^=1;
			   end; * if counter>&p;
			   end;
			end; * close kk loop;
			drop _date&p;           
			%end;
		    %end; * close ii loop;
		    %end; * if num=F;
 
		%if &na_rm=T %then
		    %do;
		    if counter<=&maxstr then delete;
		    %end;
	     run;
 
	     %if &na_rm=T %then 
		 %do;    
		 data &outds;
		 merge _temp(in=a) _leadds(in=b);
		 by &csindex descending &tsindex;
		 if a=1 and b=1;
		 run;
		 %end; * if na_rm=T;
 
	     %if &na_rm=F %then 
		 %do;    
		 data &outds;
		 merge _temp _leadds;
		 by &csindex descending &tsindex;
		 run;
		 %end; * if na_rm=F;
 
	     %dsrm(ds=_leadds _temp);

	     proc sort data=&outds;
	     by &csindex &tsindex;
	     run;
	     %end; * if incl=T;
 
 
	%if &incl=F %then
	    %do;
	    data &outds(drop=kk counter _skip _lead %varsfx(&vars, t));
	    set _temp(keep=&csindex &tsindex &vars);
		%do ii=1 %to %nvar(&leads);
		    %do;
		    %let p=%scan(&leads, &ii);
		    %if &name=S %then %let lsfx=&p;
		    %if &name=L %then %let lsfx=_&p;
		    %let vars&p=%varsfx(&vars, &lsfx);
		    _date&p=lag&p(&tsindex);
		    _lead=lag(&tsindex);
		    %end;
		%end; * close ii loop;
		by &csindex;
		   do;
		   if first.&csindex then 
		      do;
		      counter=1;
		      _skip=0;
		      end;
		   else 
		      do;
		      counter=counter+1;
		      _skip=abs(intck(&_freq, _lead, &tsindex));
		      end;
		   end;
		   retain counter;
    
		%if &num=T %then
		    %do;
		    array _x{*} &vars;
		    array _y{*} %varsfx(&vars, t);
		    %end;
 
		%if &num=F %then
		    %do;
		    array _x{*} $ &vars;
		    array _y{*} $ %varsfx(&vars, t);
		    %end;
 
		do kk=1 to dim(_x);
		   do;
		   _y{kk}=lag(_x{kk});
		   end;
		end; * close kk loop;
 
		* Leads for the numeric variables;
		%if &num=T %then
		    %do;
		    %do ii=1 %to %nvar(&leads);
			%do;
			%let p=%scan(&leads, &ii);
			array _x&p{*} &&vars&p;
			do kk=1 to dim(_x);
			   do;
			   _x&p{kk}=lag&p(_x{kk});
			   if counter<=&p then _x&p{kk}=.;
			   if counter>&p then
			      do;
			      if _skip=1 then
				 do;
				 if abs(intck(&_freq, _date&p, &tsindex))=&p then
				 _x&p{kk}=_x&p{kk};
				 else _x&p{kk}=.;
				 end; * if _skip=1;
			      if _skip^=1 then
				 do;
				 if abs(intck(&_freq, _lead, &tsindex))=&p then
				 _x&p{kk}=_y{kk};
				 else _x&p{kk}=.;
				 end; * if _skip^=1;
			      end; * if counter>&p;
			   end;
			end; * close kk loop;
			drop _date&p;           
			%end;
		    %end; * close ii loop;
		    %end; * if num=T;
 
		* Leads for the character variables;
		%if &num=F %then
		    %do;
		    %do ii=1 %to %nvar(&leads);
			%do;
			%let p=%scan(&leads, &ii);
			array _x&p{*} $ &&vars&p;
			do kk=1 to dim(_x);
			   do;
			   _x&p{kk}=lag&p(_x{kk});
			   if counter<=&p then _x&p{kk}='';
			   if counter>&p then
			      do;
			      if _skip=1 then
				 do;
				 if abs(intck(&_freq, _date&p, &tsindex))=&p then
				 _x&p{kk}=_x&p{kk};
				 else _x&p{kk}='';
				 end; * if _skip=1;
			      if _skip^=1 then
				 do;
				 if abs(intck(&_freq, _lead, &tsindex))=&p then
				 _x&p{kk}=_y{kk};
				 else _x&p{kk}='';
				 end; * if _skip^=1;
			      end; * if counter>&p;
			   end;
			end; * close kk loop;
			drop _date&p;           
			%end;
		    %end; * close ii loop;
		    %end; * if num=F;
 
		%if &na_rm=T %then
		    %do;
		    if counter<=&maxstr then delete;
		    %end;
	    run;

	    %dsrm(ds=_temp);

	    proc sort data=&outds;
	    by &csindex &tsindex;
	    run;
	    %end; * if incl=F;

%mend PLEAD;



/*********************************************************************/
/* Purpose: To create a balanced panel design from an unbalanced     */
/*	    panel data set.                                          */
/*                                                                   */
/* Originally Written: 12-17-98                                      */
/*********************************************************************/

/* ARGUMENTS:
inds............name of the input (i.e., unbalanced) panel data set.
		NOTE: The INDS data set cannnot have gaps in the 
		      time-series dimension.
csindex.........name of the variable in the INDS data set that 
		indexes the cross-sectional dimension of the data.
		NOTE: The default is SERNUM.
tsindex.........name of the variable in the INDS data set that 
		indexes the time-series dimension of the data.
		NOTE: The variable must be a SAS date value. The
		      default is DATE.
freq............specifies the frequency of the data.
		FREQ = Y - yearly data
		FREQ = Q - quarterly data
		FREQ = M - monthly data
		NOTE: The default is FREQ=Q.
vars............name(s) of the variables in the INDS data set that
		are to be included in the balanced design of the 
		OUTDS data set.
		NOTE: If VARS=F, the balanced design is created
		      only for the CSINDEX and the TSINDEX indexes.
outds...........name of the balanced design output panel data set.
		NOTE: For each cross-sectional unit, the OUTDS 
		      data set contains a time-series record 
		      beginning at t = min[T(i), i=1,...,N] and
		      ending at t = max[T(i), i=1,...,N]. The 
		      missing values for the variables in the 
		      VARS argument are replaced with zeros. In 
		      addition, the indicator variable NA is 
		      created, which equals 1 if the (i,t) 
		      observation is not in the INDS data set 
		      and 0 otherwise. */


%macro BALPDS(inds, outds, vars=F, csindex=sernum, tsindex=date, freq=Q);


	/* Define the necessary macro variables. */
	%if &freq=Y %then 
	    %do;
	    %let _freq='year';
	    %let _fmt=year4.;
	    %end;
	%if &freq=Q %then 
	    %do;
	    %let _freq='qtr';
	    %let _fmt=yyq4.;
	    %end;
	%if &freq=M %then 
	    %do;
	    %let _freq='month';
	    %let _fmt=monyy5.;
	    %end;

	
	/* Create start and end dates for each cross-sectional unit. */
	data _startds(rename=(&tsindex=sdate)) 
	     _endds(rename=(&tsindex=edate));
	set &inds(keep=&csindex &tsindex);
	    by &csindex;
	       do;
	       if first.&csindex=1 then output _startds;
	       if last.&csindex=1 then output _endds;
	       end;
	run;

	data _pddimds;
	merge _startds(in=a) _endds(in=b);
	by &csindex;
	if a=1 and b=1;
	run;		      


	/* Compute the maximum possible tenure in the panel. */
	proc univariate data=_pddimds(keep=sdate edate) noprint;
	var sdate edate;
	output out=_tends(keep=sdate edate) min=sdate x max=y edate;
	run;

	data _tends;
	set _tends;
	    maxpten=intck(&_freq, sdate, edate) + 1;
	run;

	data _null_;
	set _tends;
	    call symput('_MPT', left(put(maxpten,8.)));
	    call symput('_start', left(put(sdate,8.)));
	run;


	/* Compute the number of cross-sectional units. */
	proc sort data=&inds(keep=&csindex) out=_csds nodupkey;
	by &csindex;
	run;

	data _csds;
	set _csds;
	    _iid=_N_;
	run;

	%numobs(_csds);

	
	data _dsgnds(drop=&csindex);
	set &inds(keep=&csindex);
	    if _N_<=&_MPT;
	run;


	%do ii=1 %to &nobs;
	    %do;
	    data _tds;
	    set _dsgnds;
	        _iid=&ii;
	    run;

	    data _tds;
	    set _tds;
	        by _iid;
		   do;
		   if first._iid=1 then date=&_start;
		   else date=intnx(&_freq, date, 1);
		   end;
		retain date;
	    run;

	    %if &ii=1 %then
		%do;
		data &outds;
		set _tds;
		run;
		%end; * if ii=1;
	    %if &ii>1 %then
		%do;
		proc append base=&outds data=_tds;
		run;
		%end; * if ii>1;
	    %end;
	%end; * close ii loop;

	data &outds(keep=&csindex &tsindex);
	format date &_fmt;
	merge &outds(in=a) _csds(in=b);
	by _iid;
	if a=1 and b=1;
	run;


	/* Create the NA indicator variable. */
	data &outds(keep=&csindex &tsindex na);
	format sdate &_fmt;
	format edate &_fmt;
	merge &outds(in=a) _pddimds(in=b);
	by &csindex;
	if a=1 and b=1;
	   if sdate>date then na1=1;
	   else na1=0;
	   if edate<date then na2=1;
	   else na2=0;
	   if na1=0 and na2=0 then na=0;
	   else na=1;
	run;


	/* Add the specified variables to the OUTDS data set. */	
	%if &vars^=F %then
	    %do;
	    data &outds;
	    merge &outds(in=a) &inds(keep=&csindex &tsindex &vars);
	    by &csindex &tsindex;
	    if a=1;
	       %do ss=1 %to %nvar(&vars);
		   %do;
		   %let _vname=%scan(&vars, &ss);
		   if na=1 then &_vname=0;
		   %end;
	       %end; * close ss loop;
	    run;
	    %end; * if vars^=F;


	/* Delete all temporary data sets. */
	%dsrm(ds=_startds _endds _pddimds _tends _csds _dsgnds _tds);

	
%mend BALPDS;



/******************************************************************************/
/* Purpose: To identify and remove extreme observations in a panel data       */
/*	    framework. Extreme observations are removed and replaced by       */
/*          interpolated values.                                              */
/*                                                                            */
/* Original Version: 5-24-02                                                  */
/* This Version: 8-23-04                                                       */
/******************************************************************************/

/* ARGUMENTS:
inds...........specifies the input panel data set.
	       NOTE: The INDS data set can be an unbalanced panel with 
		     gaps in the time-series dimension.
varname........name of the variable in the INDS data set for which to 
               remove extreme observations. 
csindex........name of the variable in the INDS data set indexing the 
               cross-sectional dimension of the data.
	       NOTE: The default is CSINDEX=SERNUM.
tsindex........name of the variable in the INDS data set indexing the 
               the time series dimension of the data.
	       NOTE: The TSINDEX variable must be a SAS date value. 
		     The default is TSINDEX=DATE.
freq...........specifies the time series frequency of the data:
	       W = weekly
	       M = monthly
	       Q = quarterly (the default)
	       Y = annual
outds..........name of the output panel data set containing the series
               free of extreme observations. 
absolute=F.....specifies that an absolute criterion determined by LIM_LOWER 
               and LIM_UPPER is used for outlier criterion.
	       NOTE: The default is ABSOLUTE=F, implying that a relative 
		     outlier criterion is used. 
lim_lower......specifies the lower limit for outlier treshold.
lim_upper......specifies the upper limit for outlier treshold.
               NOTE: If ABSOLUTE=T an observation X for cross-sectional unit 
		     i in period t, denoted by X[i,t], is considered an 
		     outlier if the following condition holds:
                     X[i,t] < LIM_LOWER or X[i,t] > LIM_UPPER
scale..........specifies the scaling factor under the relative criterion used 
               to determine if an observation is considered an outlier.
	       NOTE: The default is SCALE=4.
by_csindex=T...specifies that an observation X for cross-sectional unit 
               i in period t, denoted by X[i,t], is considered an outlier 
	       if the following condition holds:
	       |X[i,t] - MED[i]| > SCALE*IQR[i],
               where
	       MED[i] = cross-section-specific time-series median of 
			X[i,t]
               IQR[i] = cross-section-specific times-series interquartile 
			range
               If BY_INDEX=F then an observation X[i,t], is considered an 
	       outlier if the following condition holds:
	       |X[i,t] - MED| > SCALE*IQR,
               where
	       MED = median of X[i,t] across all cross-sectional units and 
		     time periods
               IQR = interquartile range of X[i,t] across all cross-sectional 
		     units and time periods
               NOTE: The default is BY_CSINDEX=F.
interval.......specifies the length of the interval centered at an extreme 
               observation, observations of which are used to interpolate the 
	       missing value.
	       NOTE: If the extreme observation is detected in period s, the 
                     median of observation in the interval two-sided symmteric 
		     interval [s-interval,s+interval] is used to replace the 
		     extreme observation. */


%macro PDXTRM(inds, outds, varname, absolute=F, lim_lower=0, lim_upper=0, 
              scale=4, by_csindex=F, interval=4, csindex=sernum, tsindex=date, 
	      freq=Q);

       /* Set options: */
       options nodate nonumber;
       title1;
       title2;
       title3;


       /* Create the _EDS data set: */
       data _eds;
       set &inds(keep=&csindex &tsindex &varname where=(&varname^=.));
	   /* Observation index: */
	   _index=_N_;
           %if &absolute=T or &by_csindex=F %then
	       %do;
	       _mrg=1;
	       %end;
       run;

       %if &absolute=F %then
	   %do;
	   /* Compute the median and the IQR for each cross-sectional 
	      unit: */
	   %if &by_csindex=T %then
	       %do;
	       proc univariate data=_eds noprint;
	       by &csindex;
	       var &varname;
	       output out=_statds median=med qrange=iqr;
	       run;
	       %end;

           /* Compute the median and the IQR across all cross-sectional 
	      unit and time periods: */
           %if &by_csindex=F %then
	       %do;
	       proc univariate data=_eds noprint;
	       var &varname;
	       output out=_statds median=med qrange=iqr;
	       run;

	       data _statds;
	       set _statds;
		   _mrg=1;
               run;
	       %end;
           %end;


       %if &absolute=T %then
	   %do;
	   data _statds;
              _mrg=1;
	      lim_lower=&lim_lower;
	      lim_upper=&lim_upper;
	   run;
	   %end;


       /* Identify extreme observations: */
       data _xtrmds(keep=&csindex date_x date_l date_u _index &varname
                    rename=(&varname=xtrm _index=_xindex));
       %if &freq=W %then
	   %do;
	   format date_x weekdate17.;
	   format date_l weekdate17.;
	   format date_u weekdate17.;
	   %end;
       %if &freq=M %then
	   %do;
	   format date_x monyy7.;
	   format date_l monyy7.;
	   format date_u monyy7.;
	   %end;
       %if &freq=Q %then
	   %do;
	   format date_x yyq6.;
	   format date_l yyq6.;
	   format date_u yyq6.;
           %end;
       %if &freq=Y %then
	   %do;
	   format date_x year4.;
	   format date_l year4.;
	   format date_u year4.;
	   %end;
       merge _eds(in=a) _statds(in=b);
       %if &absolute=F %then
	   %do;       
	   %if &by_csindex=T %then
	       %do;
	       by &csindex;
	       %end;
           %if &by_csindex=F %then
	       %do;
	       by _mrg;
	       %end;
           %end;
       %if &absolute=T %then
	   %do;       
	   by _mrg;
           %end;
       if a=1 and b=1;
	  /* Relative selection criterion: */
	  %if &absolute=F %then
	      %do;
	      if abs(&varname - med) > &scale*iqr;
	      %end;
	  /* Absolute selection criterion: */
	  %if &absolute=T %then
	      %do;
	      if &varname<lim_lower or &varname>lim_upper;
	      %end;
	  /* Date of extreme observations: */
	  date_x=&tsindex;
	  /* Lower and upper bounds on the interpolation 
	     interval for each extreme observation: */
	  %if &freq=W %then
	      %do;
	      date_l=intnx('week', date_x, -&interval);
	      date_u=intnx('week', date_x, &interval);
	      %end;
	  %if &freq=M %then
	      %do;
	      date_l=intnx('month', date_x, -&interval);
	      date_u=intnx('month', date_x, &interval);
	      %end;
	  %if &freq=Q %then
	      %do;
	      date_l=intnx('qtr', date_x, -&interval);
	      date_u=intnx('qtr', date_x, &interval);
	      %end;
	  %if &freq=Y %then
	      %do;
	      date_l=intnx('year', date_x, -&interval);
	      date_u=intnx('year', date_x, &interval);
	      %end;
       run;


       /* Compute the number of extreme observations: */
       %numobs(inds=_xtrmds);
       %let Nxtrm=&nobs;

       /* No extreme observations detected: */
       %if &Nxtrm=0 %then
	   %do;
	   data &outds;
	   set _eds;
	   run;

	   %dsrm(ds=_eds _statds _xtrmds);
	   %end;
	   
       /* At least one extreme observation detected: */
       %if &Nxtrm>0 %then
	   %do;
	   /* Print the extreme observations: */
	   proc print data=_xtrmds split="+";
	   var &csindex date_x xtrm;
	   label &csindex="C-S Index+(%upcase(&csindex))";
	   label date_x="Outlier+Date";
	   label xtrm="Outlier+Value";
	   title1 "-- Outliers Detected and Replaced --";
	   title2 "Variable: %upcase(&varname)";
	   %if &absolute=F %then 
	       %do;
	       title3 "Relative Selection Criterion: &scale x IQR";
	       title4 "(BY_CSINDEX = %upcase(&by_csindex))";
	       %end;
	   %if &absolute=T %then 
	       %do;
	       title3 "Absolute Selection Criterion";
	       title4 "LIM_LOWER = &lim_lower and LIM_UPPER = &lim_upper";
	       %end;
	   run;
	   title1;
	   title2;
	   title3;
	   title4;

	   /* Replace extreme observation: */
	   %do n=1 %to &Nxtrm;
	       %do;
	       data _select;
	       set _xtrmds;
		   if _N_=&n;
               run;

	       /* Observation in the interval surrounding the 
		  extreme observation: */
	       data _interval;
	       merge _select(in=a drop=xtrm _xindex) _eds(in=b);
	       by &csindex;
	       if a=1 and b=1;
		  if &tsindex<date_l or &tsindex>date_u then delete;
		  if &tsindex=date_x then &varname=.;
               run;

	       /* Local interpolation: */
	       proc univariate data=_interval noprint;
	       var &varname;
	       output out=_intds median=intvalue;
	       run;

	       /* Replace the extreme observation with its 
		  interpolated value: */
               data _intds;
	       merge _select(keep=&csindex _xindex) _intds;
	       run;

	       data _eds(drop=_xindex intvalue);
	       merge _eds(in=a) _intds(in=b);
	       by &csindex;
	       if a=1;
		  if _index=_xindex then &varname=intvalue;
	       run;
	       %end;
           %end; * close n loop;

	   /* Create the OUTDS data set containing the clean series
	      for each cross-sectional unit: */
	   data &outds(keep=&csindex &tsindex &varname);
	   set _eds;
	   run;

	   %dsrm(ds=_eds _intds _select _interval _xtrmds _statds);
	   %end; * if Nxtrm>0;

       /* Set options: */
       options date number;
       title1;
       title2;
       title3;


%mend PDXTRM;


/******************************************************************************/
/* Purpose: To provide a statistical summary of variable(s) in a panel        */ 
/*	    data set.                                                         */
/******************************************************************************/

/* ARGUMENTS: 
inds............name of the input panel data set.
		NOTE: The INDS data set can be an unbalanced panel with 
                      gaps in the time-series dimensions.
csindex.........name of the variable in the INDS data set indexing the 
                cross-sectional dimension.
tsindex.........name of the variable in the INDS data set indexing the 
                time-series dimension.
		NOTE: The TSINDEX variable must be a SAS date value.
		      The default is TSINDEX = DATE
freq............frequency of the data:
                Y = annual
		Q = quarterly (default)
		M = monthly
		D = daily
vars............name(s) of the variable(s) in the INDS data set for 
                which to compute a statistical summary.
		NOTE: Statistical summary consists of the distribution 
		      of individual-specific means. The default is 
		      VARS = F, implying that only panel dimension
		      are computed.
*/

%macro PDSUMMARY(inds, csindex, tsindex=date, freq=Q, vars=F);


       /* Set options: */
       options nodate nonumber;

       %if &vars=F %then %let _vars=;
       %if &vars^=F %then %let _vars=&vars;

       data _pds;
       set &inds(keep=&csindex &tsindex &_vars);
	   %if &vars^=F %then
	       %do;
	       array _x{*} &_vars;
	       do i = 1 to dim(_x);
		  do;
		  if _x{i} = . then delete;
		  end;
	       end;
	       drop i;
	       %end;
       run;

       proc sort data=_pds;
       by &csindex &tsindex;
       run;

       /* Create a numeric sequential cross-sectional identifier: */
       proc sort data=_pds(keep=&csindex) out=_csds nodupkey;
       by &csindex;
       run;

       data _csds;
       set _csds;
           _csindex = _N_;
       run;

       data _pds;
       merge _csds(in=a) _pds(in=b);
       by &csindex;
       if a=1 and b=1;
       run;

       
       /* Basic panel dimensions:
	  _NT = number of observations
	   _N = number of cross-sectional units
	   _T = number of time periods */    
       proc sql noprint;
       select count(_csindex), count(distinct _csindex), count(distinct &tsindex) 
       into :_NT, :_N, :_T from _pds;
       quit;


       /* Summary of the time-series dimension: */

       /* Time-series span of the data: */
       proc univariate data=_pds(keep=&tsindex) noprint;
       var &tsindex;
       output out=_tmp1 min=_sdate max=_edate;
       run;

       /* Tenure statistics: */
       data _tmp2(keep=_csindex _tenure);
       set _pds;
	   by _csindex;
	      do;
	      if first._csindex = 1 then _tenure = 1;
	      else _tenure = _tenure + 1;
	      if last._csindex = 1;
	      end;
	   retain _tenure;
       run;

       data _tmp2;
       set _tmp2;
           _xx=1/_tenure;
       run;
       
       proc univariate data=_tmp2 noprint;
       var _tenure _xx;
       output out=_tmp2 mean=_avgT 
			q1=_p25T
			q3=_p75T
       		        median=_medT
       		        min=_minT
       		        max=_maxT
       		        sum=_sumT _xx_sum;
       run;
       
       data _tmp2(keep=_minT _p25T _medT _p75T _maxT _avgT _apindex);
       set _tmp2;
           _apindex = &_N/(_avgT*_xx_sum);
       run;


       /* Summary of the cross-sectional dimension: */

       /* The distribution of cross-sectional units across time: */
       proc sort data=_pds(keep=_csindex &tsindex) out=_tmp3;
       by &tsindex;
       run;

       proc univariate data=_tmp3 noprint;
       by &tsindex;
       var _csindex;
       output out=_tmp3 n=_N_t;
       run;

       proc univariate data=_tmp3 noprint;
       var _N_t;
       output out=_tmp3 mean=_avgN_t
			min=_minN_t
			q1=_p25N_t
       			median=_medN_t
			q3=_p75N_t
			max=_maxN_t;
       run;

       data _dimeds;
       merge _tmp1 _tmp2 _tmp3;
       run;
       
       data _null_;
       set _dimeds;
	   _NT=&_NT;
	   _N=&_N;
	   _T=&_T;
	   _avgT = round(_avgT, 0.01);
	   _apindex = round(_apindex, 0.01);
	   _avgN_t = round(_avgN_t, 0.01);
           call symput('_NT', trim(left(_NT)));
           call symput('_N', trim(left(_N)));
           call symput('_T', trim(left(_T)));
           call symput('_sdate', _sdate);
           call symput('_edate', _edate);
           call symput('_avgT', trim(left(_avgT)));
           call symput('_minT', trim(left(_minT)));
           call symput('_p25T', trim(left(_p25T)));
           call symput('_medT', trim(left(_medT)));
           call symput('_p75T', trim(left(_p75T)));
           call symput('_maxT', trim(left(_maxT)));
           call symput('_apindex', trim(left(_apindex)));
           call symput('_avgN_t', trim(left(_avgN_t)));
           call symput('_minN_t', trim(left(_minN_t)));
           call symput('_p25N_t', trim(left(_p25N_t)));
           call symput('_medN_t', trim(left(_medN_t)));
           call symput('_p75N_t', trim(left(_p75N_t)));
           call symput('_maxN_t', trim(left(_maxN_t)));
       run;
       

       /* Statistical summary of the variable(s) specified by the 
	  VARS argument: */
       %if &vars^=F %then
	   %do;
	   /* Individual-specific means: */
	   proc univariate data=_pds noprint;
	   by _csindex;
	   var &_vars;
	   output out=_tmp3 mean=&_vars;
	   run;

	   /* Distribution of individual-specific means: */
	   proc univariate data=_tmp3 noprint;
	   var &_vars;
	   output out=_stateds min=%varsfx(&_vars, _min)
                               q1=%varsfx(&_vars, _p25)
                               median=%varsfx(&_vars, _med)
                               q3=%varsfx(&_vars, _p75)
                               max=%varsfx(&_vars, _max);
	   run;

	   proc iml;
	   use _stateds;
	   read all var _num_ into S;
	   close _stateds;
	   Smat = t(shape(S, 5, %nvar(&_vars)));
	   max = Smat[ ,1];
	   p75 = Smat[ ,2];
	   med = Smat[ ,3];
	   p25 = Smat[ ,4];
	   min = Smat[ ,5];
	   varname = {%upcase(&_vars)};
	   create _stateds var{varname min p25 med p75 max};
	   append;
	   quit;  
	   %end;
	

       /* Print the results. */
       proc iml;

       reset center noname;
       
       print '------------------------ Panel Data Summary ------------------------';
       reset nocenter noname;

       /* Balanced panel: */
       %if &_minT=&_maxT %then
       	   %do;
	   print 'Data Set (balanced panel):' %quote("%upcase(&inds)");
	   print 'Total Number of Observations:' %quote("&_NT");
	   print 'Cross-Sectional Index:' %quote("%upcase(&csindex)");
	   print 'Total Number of Cross-Sectional Units:' %quote("&_N");
	   print 'Time-Series Index:' %quote("%upcase(&tsindex)");
	   print 'Total Number of Time Periods:' %quote("&_T");
	   _sdate=&_sdate;
	   _edate=&_edate;
	   %if &freq=Y %then
	       %do;
	       print 'Time-Series Range:' _sdate[format=year4.] 'to' _edate[format=year4.];
	       %end;
	   %if &freq=Q %then
	       %do;
	       print 'Time-Series Range:' _sdate[format=yyq6.] 'to' _edate[format=yyq6.];
	       %end;
	   %if &freq=M %then
	       %do;
	       print 'Time-Series Range:' _sdate[format=monyy7.] 'to' _edate[format=monyy7.];
	       %end;
	   %if &freq=D %then
	       %do;
	       print 'Time-Series Range:' _sdate[format=weekdate18.] 'to' _edate[format=weekdate18.];
	       %end;

           reset center name;
           quit;
	   %end;

       /* Unbalanced panel: */
       %if &_minT^=&_maxT %then
	   %do;
	   print 'Data Set (unbalanced panel):' %quote("%upcase(&inds)");
	   print 'Total Number of Observations:' %quote("&_NT");
	   print 'Cross-Sectional Index:' %quote("%upcase(&csindex)");
	   print 'Total Number of Cross-Sectional Units:' %quote("&_N");
	   print 'Avg. Number of Cross-Sectional Units per Time Period:' %quote("&_avgN_t"); 
	   print 'Time-Series Index:' %quote("%upcase(&tsindex)");
	   print 'Total Number of Time Periods:' %quote("&_T");
	   _sdate=&_sdate;
	   _edate=&_edate;
	   %if &freq=Y %then
	       %do;
	       print 'Time Series Range:' _sdate[format=year4.] 'to' _edate[format=year4.];
	       %end;
	   %if &freq=Q %then
	       %do;
	       print 'Time Series Range:' _sdate[format=yyq6.] 'to' _edate[format=yyq6.];
	       %end;
	   %if &freq=M %then
	       %do;
	       print 'Time Series Range:' _sdate[format=monyy7.] 'to' _edate[format=monyy7.];
	       %end;
	   %if &freq=D %then
	       %do;
	       print 'Time Series Range:' _sdate[format=weekdate18.] 'to' _edate[format=weekdate18.];
	       %end;
           print 'Average Tenure:' %quote("&_avgT");
           print 'Ahrens-Pincus Index:' %quote("&_apindex");

           reset center name;
           quit;

	   title1;
	   proc print data=_dimeds noobs split='+';
	   var _minT _p25T _medT _p75T _maxT;
	   label _minT = "Minimum";
	   label _p25T = "Pctl-25";
	   label _medT = "Median";
	   label _p75T = "Pctl-75";
	   label _maxT = "Maximum";
	   title 'Distribution of the Tenure in the Panel';
	   run;
	   title1;

	   title1;
	   proc print data=_dimeds noobs split='+';
	   var _minN_t _p25N_t _medN_t _p75N_t _maxN_t;
	   label _minN_t = "Minimum";
	   label _p25N_t = "Pctl-25";
	   label _medN_t = "Median";
	   label _p75N_t = "Pctl-75";
	   label _maxN_t = "Maximum";
	   title 'Distribution of Cross-Sectional Units Across Time';
	   run;
	   title1;
	   %end;


        %if &vars^=F %then
	    %do;
	    title1;
	    proc print data=_stateds noobs split='+';
	    var varname min p25 med p75 max;
	    label varname = "Variable";
            label min = "Minimum";
	    label p25 = "Pctl-25";
	    label med = "Median";
	    label p75 = "Pctl-75";
	    label max = "Maximum";
	    format min p25 med p75 max 8.4;
	    title 'Distribution of Individual-Specific Means';
	    run;
	    title1;

	    %dsrm(ds=_tmp3 _stateds);
	    %end;

       %dsrm(ds=_pds _csds _dimeds _tmp1 _tmp2 _tmp3);

       /* Set options: */
       options date pageno=1;


%mend PDSUMMARY;


/********************************************************************/
/* Purpose: To extract a subset of observations from an unbalanced  */
/*          panel data set, which can be used to identify           */
/*          parameters of a fixed-effects panel data model.         */
/*                                                                  */
/* Originally Written: 8-24-07                                      */
/* Note: This program uses the SAS Institute tscsdata.sas macro.    */
/********************************************************************/

/* ARGUMENTS:			
inds............input panel data set. The INDS data set can be an
                unbalanced panel with gaps in the time-series
                dimension.
outds...........specifies the name of the output data set that
                contains only cross-sectional units with at least
                two time-series observations and time periods with
                at least two cross-sectional units.
vars............all the variables of the underlying regression
                model.
csindex.........name of the variable in the INDS data set that 
		indexes the cross-sectional dimension.
		NOTE: The default is SERNUM.
tsindex.........name of the variable in the INDS data set that 
		indexes the time-series dimension.
*/

%macro TSCSds(inds, outds, vars, csindex=sernum, tsindex=date);


       /* Define the necessary macro variables: */
       %let _K = %eval(%nvar(&vars) - 1);
       %let _select_min = 0;
       
       /* Set up the data for the tscsdata.sas macro: */
       data _pds;
       set &inds(keep=&csindex &tsindex &vars);
           /* Eliminate all missing observations: */
           array _z{*} &vars;
           do i = 1 to dim(_z);
              do;
              if _z{i} = . then delete;
              end;
           end;
           drop i;
       run;

       /* Create the selection indicator (_select):
          NOTE: _select = 0 if observation (i,t) is not
                included in the OUTDS data set and 1
                otherwise. */ 
       data _pds;
       set _pds;
           _select = 1;
       run;


       %do %while (&_select_min < 1);
           /* Run the tscsdata.sas macro on the _PDS data set: */
           %tscsdata(in=_last_, out=_pds, ts=&tsindex, cs=&csindex,
                     vars=&vars, nreg=&_K, keep=_select);

           proc univariate data=_pds noprint;
           var _select;
           output out=_minds min=_select_min;
           run;
           
	   data _null_;
	   set _minds;
	       call symput('_select_min', left(put(_select_min, 1.)));
	   run;	

	   %dsrm(ds=_minds);

           data _pds;
           set _pds(where=(_select=1));
           run;
           %end;
       
       data &outds(keep=&csindex &tsindex &vars);
       set _pds(where=(_select=1));
       run;

       proc sort data=&outds;
       by &csindex &tsindex;
       run;

       %dsrm(ds=_pds _temp);

       
%mend TSCSds;
