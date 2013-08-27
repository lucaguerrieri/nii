function [yobs, dates, nii, nims, nfactors, nothers, tau, factors,...
    shadow_bank_share_assets,...
    total_interest_earning_assets,...
    assets_depository_inst, assets_securities_notrade,...
    assets_fedfunds, assets_all_loans, assets_trading_accnts, ... 
    varargout] = load_data_ml(varargin)

start_yobs = 1996.75;
end_yobs = 2012.75;
dates = start_yobs:.25:end_yobs;


if isunix
    nii = csvread('../data/normalized_variables.csv', 1, 2);
else
    nii = csvread('..\data\normalized_variables.csv', 1, 2);
end
dates_yields = 1985.75:.25:2013.0;
start_pos = find(dates==start_yobs);
end_pos = find(dates==end_yobs);
nii = nii(start_pos:end_pos,:)';
nii_varlist = char('assets','nii', 'US_GDP', 'euro_area_gdp', 'jp_gdp', 'la_gdp',...
    'pca_hhub_nat_gas','pca_brent_crude','pca_3mo_tr_yield','pca_10yr_tr_yield',...
    'pca_bbb10yr','pca_vix','pca_sp500','pca_us_ipo','pca_us_dpi','pca_us_ur',...
    'pca_us_mort','pca_us_hpi');

yobs = nii; 
nfactors = 3;
nothers = 0;
tau = [3 6 9 12 24 36 60 84 120 180 240 360];

factor1 = (yields(find(tau == 3),:)...
    +  yields(find(tau == 24),:)...
    +  yields(find(tau == 120),:) )/3;

%factor1 = (yields(find(tau == 60),:));

factor2 = (yields(find(tau == 3),:) - yields(find(tau == 120),:));

factor3 = ( 2*yields(find(tau == 24),:) ...
    - yields(find(tau == 120),:) ...
    - yields(find(tau == 3),:)  )*5;

factors = [factor1; factor2; factor3];