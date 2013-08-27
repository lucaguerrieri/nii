equation test1.ls nii nii(-1 to -4) pca_3mo_tr_yield(-1)   pca_vix(-1) pca_us_dpi(-1)
'This equation is statistically significant for all but 2-3 lags of NII. Need NII lags to get significance.

equation test2.ls nii nii(-1)  la_gdp(-1 to -7)
'This equation is statistically significant for lags 1-3 & lag 7 of latin american gdp

equation us1.ls nii nii(-1) us_gdp(-1 to -4) pca_sp500(-1 to -2)
'This equation is statistically significant for lags 1&4 of us gdp & 2 of SP500.

equation test4.ls nii nii(-1 to -4) pca_3mo_tr_yield(-1) pca_vix(-1) pca_us_dpi(-1)
'This equation is statistically significant for all variables except lags 2-4 of NII. Need NII lags to get significance.

equation test5.ls nii nii(-1 to -2) pca_vix(-1) pca_us_dpi(-1) pca_hhub_nat_gas(-1)
'This equation is statistically significant for all variables except for NII -2. Need NII lag to get significance.


