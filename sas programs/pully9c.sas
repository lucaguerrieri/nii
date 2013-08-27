/*******************************************************************
Author: Michelle Welch (m1mrh02); Updated by Valentin Bolotnyy (m1vxb00)
Date: March 28, 2012; Date Updated: November 20, 2012.
Purpose: Pull Merger adjusted Y-9C (May9C) data from Banking Analysis
Output: may9c_matchange.sas7bdat

NOTE: nonintinc_tr_tra == the return on trading assets. You might see this
labeled as return_tradingassets in other spreadsheets.
********************************************************************/
* Old "Include" references;
*%include './Include/library';
*%include './Include/bcrmacros.sas';
*%include './Include/PDMACRO.sas';
*%include './Include/setmay9.sas';
*%include './Include/settiny.sas';

* DO NOT CHANGE ANYTHING IN THIS SUBSECTION;
%include '/ofs/research2/ofs_templates/BA_MACROS/antiny.sas';
%include '/ofs/research2/ofs_templates/BA_MACROS/settiny.sas';
%include '/ofs/research2/ofs_templates/BA_MACROS/bcrmacros.sas';
%include '/ofs/research2/ofs_templates/BA_MACROS/MACRO.sas';
%include '/ofs/research2/ofs_templates/BA_MACROS/setmay9.sas';

libname nic           '/ofs/scratch2/m1reb02/';
libname tiny          '/bks/proj/cbp/sas/data/tiny';
libname tiny80        '/bks/proj/cbp/sas/data/tiny/tiny80';
libname tiny90        '/bks/proj/cbp/sas/data/tiny/tiny90';
libname library       '/bks/proj/cbp/sas/data/library_64';
libname may9c         '/bks/proj/cbp/sas/data/may9/ag';

*This is the output directory*
libname out '/ofs/prod1/CCAR/noninterest_income/data';

options sasautos=('/bks/proj/cbp/sas/pgms/tiny/macros',
                  '/bks/proj/cbp/sas/pgms/uniform/macros',
                  '!SASROOT/sasautos',
                  '!SASROOT/frbmac');

* CHANGE THINGS BELOW AS NECESSARY;

%let fpath = /ofs/prod1/CCAR/noninterest_income/data;
libname nii "&fpath";

%let sdate = 1990q1;
%let edate = 2012q4; *Change this as necessary*
*retrieve MAY9 data;
%let mvars = entity BC01000 ACG0100 FI03350 FI03100 FI03150
FI03050 FI01250 FI04400 FI01775 FI01375
FI03300 FI04650 FI04300 FI04250 FI01800
FI01900 FI01850 FI01750 FI01700 ACP0850
ACP0900 FIA0800 FI01950 FI03200 FI03250
FI01300 FI02000 FI01350 FI04150 FI04200
FI04100 FI04050 FI04000 FI01650
;

%let rmvars = entity assets credit_loss_allowances card_interchange_fees earnings_life_insurance 
atm_fees check_printing_fees fiduciary_activities sale_mutual_funds insurcance_commissions 
investment_banking_fees net_change_fair_value ng_assets_fv ng_credit_deriv ng_sale_loans 
ng_other_assets ng_other_real_estate net_sec_income net_servicing_fees res_mortgages_closed
res_mortgages_open other_adj_equity other_nii rent_other_real_estate safe_deposit_rent 
service_charges total_nii total_tr tr_comm tr_credit tr_equity tr_foreign_ex tr_int_rt_exp 
vc_rev  
              ;

%setmay9(dates=%dateseq(&sdate,&edate),tsindex=date,kvars=&mvars,outds=may9_raw);
data may9; set may9_raw(rename=(%renamer1(&mvars,&rmvars))); run;
proc sort data=may9; by entity date; run;

 *keep only domestic banks;
data attr; 
     set nic.attr(keep=id_rssd d_dt_start d_dt_end domestic_ind nm_short); 
run;

proc sql;
  create table may9c_dm as
  select t.*, a.*
  from may9 t left join attr a on (t.entity=a.id_rssd)
  where (d_dt_start <= date <= d_dt_end and domestic_ind="Y");
quit;


*keep only top 25 BHCs;
proc sort data=may9c_dm; by date descending assets; run;
data may9c_bhc25(where=(rank<=25));
  set may9c_dm;
  by date;
  if first.date then rank=1; 
  else rank+1;
  retain rank;
run;

*aggregate May9c data;
/*proc univariate noprint data=may9c_bhc25;
  by date;
  var &rmvars;
  output out=may9c_agg25 sum=&rmvars;
run;

*eliminate erroneous charge-off rates;
data may9c_agg25;
  set may9c_agg25;
  %replace_num(0,.);
run;
*/
/*construct financial variables*/
/*data may9c_vars;
      set may9c_agg25;
      nonintinc_xtr = sum(nonintinc,-nonintinc_tr);
      nonintexp_xgw = sum(nonintexp,-impair_gw);
      nonintexp_oth = sum(nonintexp,-nonintexp_comp,-nonintexp_prop,-impair_gw,-impair_oint);
      ppnr_y9 = 100*sum(netintinc,nonintinc,-nonintexp)/assets_avg;
      ppnr_base = 100*sum(netintinc,nonintinc,-nonintexp,impair_gw,impair_oint,-inc_extra)/assets_avg;
      ppnr_xtr = 100*sum(netintinc,nonintinc_xtr,-nonintexp)/assets_avg;  
      nonintinc_xtr_ta = 100*nonintinc_xtr/assets_avg;
      nonintinc_tr_ta = 100*nonintinc_tr/assets_avg;
      nonintinc_tr_tra = 100*nonintinc_tr/assets_tr;
      nim_y9 = 100*netintinc/(assets_ie_avg+assets_tr);
      nim_ta = 100*netintinc/assets_avg;
      nonintexp_comp_ta = 100*nonintexp_comp/assets_avg;
      nonintexp_prop_ta = 100*nonintexp_prop/assets_avg;
      nonintexp_oth_ta = 100*nonintexp_oth/assets_avg;
      deps = 100*sum(dep_dom,dep_fgn)/assets_avg;
      payout = sum(dividends,tstock_prchs);
      alll = 100*alll_lvl/loans_avg;
      lllp_y9 = 100*lllp_lvl/loans_avg;
      tier1cr = tier1c/rwa;
      chgoff_y9 = 100*nchg/loans_avg;
      chgoff_ci_y9 = 100*nchg_ci/loans_ci;
      chgoff_cre_y9 = 100*(sum(nchg_cld,nchg_mfam,nchg_nfnr)/sum(loans_cld,loans_mfam,loans_nfnr));
      chgoff_cld_y9 = 100*nchg_cld/loans_cld;
      chgoff_mfam_y9 = 100*nchg_mfam/loans_mfam;
      chgoff_nfnr_y9 = 100*nchg_nfnr/loans_nfnr;
      chgoff_heloc_y9 = 100*nchg_heloc/loans_heloc;
      chgoff_sfam_y9 = 100*nchg_sfam/loans_sfam;
      chgoff_sfam_l1_y9 = 100*nchg_sfam_l1/loans_sfam_l1;
      chgoff_sfam_l2_y9 = 100*nchg_sfam_l2/loans_sfam_l2;
      chgoff_cons_y9 = 100*nchg_cons/loans_cons;
      chgoff_cc_y9 = 100*nchg_cc/loans_cc;
      chgoff_consxcc_y9 = 100*nchg_consxcc/loans_consxcc;
      exposure_ci_y9 = 100*loans_ci/assets_ie_avg;
      exposure_cre_y9 = 100*sum(loans_cld,loans_mfam,loans_nfnr)/assets_ie_avg;
      exposure_rre_y9 = 100*sum(loans_heloc,loans_sfam_l1,loans_sfam_l2)/assets_ie_avg;
      exposure_cc_y9 = 100*loans_cc/assets_ie_avg;
      exposure_othcons_y9 = 100*sum(loans_cons,-loans_cc)/assets_ie_avg;
      exposure_tr_y9 = 100*assets_tr/assets_ie_avg;

run;
*/
  

/*seasonally adjust series;
%let svars = ppnr nim netinc_ptax nonintinc_net nonintinc nonintexp 
             nonintinc_xtr nonintinc_tr nonintexp_comp nonintexp_prop nonintexp_oth
             tier1r lllp chgoff
             chgoff_ci chgoff_cre chgoff_cld chgoff_mfam chgoff_nfnr 
             chgoff_rre chgoff_heloc chgoff_sfam chgoff_cc chgoff_othcons chgoff_oth;
proc x12 noprint data=tiny_agg25 date=date interval=qtr;
  var &svars;
  x11 mode=add;
  output out=seasonal d11;
run;

%let svars_nsa = %varsfx(&svars,_nsa);
data tiny_agg25_sa;
  merge seasonal(rename=(%renamer1(%varsfx(&svars,_D11),&svars)))
        tiny_agg25(rename=(%renamer1(&svars,&svars_nsa)));
  by date;
run;
*/

  *output;
  data nii.may9c_noninterest; set may9c_bhc25; run;
  proc export data=may9c_bhc25 outfile="&fpath./may9c_noninterest_nonagg.csv" dbms=csv replace; run;  

endsas;
