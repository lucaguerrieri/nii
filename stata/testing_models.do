/*clear
log using nii_log, replace
cd /ofs/prod1/CCAR/noninterest_income/data
insheet using normalized_variables_for_OxMetrics.csv

egen date2 = concat(year quarter)
destring date2, replace

drop credit_loss_allowances card_interchange_fees earnings_life_insurance atm_fees check_printing_fees fiduciary_activities sale_mutual_funds insurance_commissions investment_banking_fees net_chage_fair_value ng_assets_fv ng_credit_derivative ng_sale_loans ng_other_assets ng_other_real_estate net_sec_income net_servicing_fees res_mortgages_closed res_mortgages_open other_adj_equity other_nii rent_other_real_estate safe_deposit_rent service_charges total_tr tr_comm tr_credit tr_equity tr_foreign_ex int_rt_exp vc_rev

tsset date2

foreach var of varlist nii pca_priv_ewma_iss_nonvc pca_piv_eq_iss_nonvc pca_priv_place_fin_lp_sdc pca_priv_place_fin_sdc pca_priv_place_sdc_nonfin pca_total_iss pca_priv_vc_pwc pca_cash_val_ann_dom_merg pca_cash_val_comp_dom_merg pca_liab_val_comp_dom_merg pca_stock_val_comp_dom_merg pca_cash_val_for_comp_merg pca_liab_comp_for_merg pca_comp_for_merg pca_stock_val_comp_for_merg usgdp pca_baltic_dry euro_area_gdp jp_gdp gb_gdp mx_gdp la_gdp pca_gsci pca_msci pca_hhub_nat_gas pca_brent_crude pca_nf_junk_iss pca_nf_unrated_iss pca_fin_inv_iss pca_fin_junk_iss pca_fin_unrated_iss pca_iss_dom_mfg pca_iss_dom_elec pca_iss_dom_trans pca_iss_dom_tele pca_iss_dom_fin pca_nf_for_us_corp_iss pca_fin_for_us_corp_iss pca_3mo_tr_yield pca_10yr_tr_yield pca_us_nat_gas pca_bbb10yr pca_vix pca_sp500 pca_us_ipo pca_us_dpi pca_us_dpi_nom pca_us_ur pca_us_inflation pca_us_corp pca_us_mort pca_us_hpi pca_us_cre{
gen `var'_1 = `var'[_n-1]
gen `var'_2 = `var'[_n-2]
gen `var'_3 = `var'[_n-3]
gen `var'_4 = `var'[_n-4]
gen `var'_5 = `var'[_n-5]
gen `var'_6 = `var'[_n-6]
gen `var'_7 = `var'[_n-7]
gen `var'_8 = `var'[_n-8]
}

save nii_test.dta, replace
*/
clear
use nii_test.dta

sample 50

reg nii nii_1 nii_2 pca_10yr_tr_yield_1 pca_10yr_tr_yield_2 pca_bbb10yr_1 pca_bbb10yr_2, robust
reg nii nii_1 usgdp_1 usgdp_2 usgdp_3 usgdp_4 pca_sp500_1 pca_sp500_2, robust
reg nii nii_1 la_gdp_1 la_gdp_2 la_gdp_3 la_gdp_4 la_gdp_5 la_gdp_6 la_gdp_7, robust
reg nii nii_1 nii_2 nii_3 pca_3mo_tr_yield_1 pca_vix_1 pca_us_dpi_1, robust
reg nii nii_1 nii_2 nii_3 nii_4 pca_3mo_tr_yield_1 pca_vix_1 pca_us_dpi_1, robust
reg nii nii_1 nii_2 pca_vix_1 pca_us_dpi_1 pca_hhub_nat_gas_1, robust
reg nii nii_1 nii_2 nii_3 pca_vix_1, robust
reg nii nii_1 euro_area_gdp_1 jp_gdp_1 la_gdp_1, robust
reg nii nii_1 usgdp_1 usgdp_2 pca_us_hpi_1 pca_us_ipo_1 pca_us_mort_1 pca_us_ur_1, robust

reg nii nii_1 pca_us_hpi_1 pca_us_ur_1 euro_area_gdp_1 jp_gdp_1, robust

//log close
