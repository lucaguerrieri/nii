
clear;

%setpath;

dataset_option = 1;

%[yobs, dates, yields, nims, nfactors, nothers, tau, factors,...
    %shadow_bank_share_assets,...
    %total_interest_earning_assets,...
   % assets_depository_inst, assets_securities_notrade,...
    %assets_fedfunds, assets_all_loans, assets_trading_accnts, ... 
   % interest_income_to_ie_assets, interest_expense_to_ie_assets] = load_data_ml(dataset_option);
   
  start_yobs = 1996.75;
end_yobs = 2012.75;
dates = start_yobs:.25:end_yobs;



    nii = csvread('../data/normalized_variables.csv',2,4);

start_pos = find(dates==start_yobs);
end_pos = find(dates==end_yobs);
nii = nii(start_pos:end_pos,:)';
nii_varlist = char('assets','nii', 'US_GDP', 'euro_area_gdp', 'jp_gdp', 'la_gdp',...
    'pca_hhub_nat_gas','pca_brent_crude','pca_3mo_tr_yield','pca_10yr_tr_yield',...
    'pca_bbb10yr','pca_vix','pca_sp500','pca_us_ipo','pca_us_dpi','pca_us_ur',...
    'pca_us_mort','pca_us_hpi');
 
out_of_sample_start_pos = find(dates==2000.0);
end_sample_pos = length(dates);

%nstates = nfactors + nothers;
%ntaus = length(tau);





%% extract smoothed estimates of the yield curve factors
forecast_horizon = 10;




%% get RMSEs
lag = 1;

% Table 1 -- Shortened sample:
[rmse_forecast_combination_mat1, forecast_combination_mat1] = calc_rmse_forecast_combination_conditional(nii(:,15:end),nii_varlist(:,15:end), out_of_sample_start_pos-14, end_sample_pos-14, forecast_horizon, 1,4);

[rmse_multivariate_mat1, forecast_multivariate_mat1] = calc_rmse_multivariate_conditional(nii(:,15:end), nii_varlist(:,15:end), out_of_sample_start_pos-14, end_sample_pos-14, forecast_horizon, lag);


npc = 3;
[rmse_pc_mat1, forecast_pc_mat1] = calc_rmse_pc(nii(:,15:end), nii_varlist(:,15:end), out_of_sample_start_pos-14, end_sample_pos-14, forecast_horizon, npc);


[rmse_forecast_combination_mat3, forecast_combination_mat3] = calc_rmse_forecast_combination_conditional(nii(:,15:end),nii_varlist(:,15:end), out_of_sample_start_pos-14, end_sample_pos-14, forecast_horizon, 1,4);

varlag=4;
[rmse_varmat2, forecast_var_mat2] = calc_rmse_var_conditional(nii(:,15:end), nii_varlist(:,15:end), out_of_sample_start_pos-14, end_sample_pos-14, forecast_horizon, varlag);
[rmse_nochangemat1, forecast_nochange_mat1] = calc_rmse_nochange(nii(:,15:end), out_of_sample_start_pos-14, end_sample_pos-14, forecast_horizon);



table1 = [
rmse_forecast_combination_mat1
rmse_multivariate_mat1
rmse_pc_mat1
rmse_forecast_combination_mat3
rmse_varmat2
rmse_nochangemat1]




columnlabels = char('Step 1','Step 2','Step 3','Step 4','Step 5','Step 6','Step 7','Step 8','Step 9','Step 10');
rowlabels = char('1. F. Combination - Yields',...
                 '2. Multivariate Regression',...
                 '4. PCR',...
                 '6. F. Combination - Observed Factors',...
                 '7. VAR on Observed Factors',...
                 '8. No-Change Forecast');
table1_tex = tablelatex(table1,columnlabels,rowlabels);
char(table1_tex)


