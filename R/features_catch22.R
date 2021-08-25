#
#
#
# features_catch22 <- function(C, window_sizes = c(5, 22)) {
#
#   # solve No visible binding for global variable
#   # symbol <- open <- high <- low <- close <- volume <- close_ath <- NULL
#
#   # checks
#   testSubset(c("symbol", "datetime", "close"), colnames(ohlcv))
#   assert_double(ohlcv$close, lower = 1e-005)
#
#   # returns
#   new_cols <- paste0("returns_", window_sizes)
#   ohlcv[, (new_cols) := lapply(window_sizes, function(w) close / shift(close, n = w) - 1), by = symbol]
#
#   # symbols
#   price_sybmols <- unique(prices$symbol)
#
#   for (s in price_sybmols[1:800]) {
#     # print(s)
#
#     # data sample
#     sample_ <- copy(prices)
#     sample_ <- sample_[symbol == s, close]
#
#     # create catch 22 features
#
#     # save
#     cols <- c("symbol", "date",
#               colnames(sample_)[which(colnames(sample_) == "CO_f1ecac"):ncol(sample_)])
#     fwrite(sample_[, ..cols], paste0("D:/fundamental_data/catch22/", s, "-", n, ".csv"))
#
#     # remove hgarbage collection
#     gc()
#   }
#
# }
#
#
# my_catch22 <- function(DT, n = 22) {
#   DT[, `:=`(
#     CO_Embed2_Dist_tau_d_expfit_meandiff = frollapply(close, n, Rcatch22::CO_Embed2_Dist_tau_d_expfit_meandiff),
#     CO_f1ecac = frollapply(close, n, Rcatch22::CO_f1ecac),
#     CO_FirstMin_ac = frollapply(close, n, Rcatch22::CO_FirstMin_ac),
#     CO_HistogramAMI_even_2_5 = frollapply(close, n, Rcatch22::CO_HistogramAMI_even_2_5),
#     CO_trev_1_num = frollapply(close, n, Rcatch22::CO_trev_1_num),
#     DN_HistogramMode_10 = frollapply(close, n, Rcatch22::DN_HistogramMode_10),
#     DN_HistogramMode_5 = frollapply(close, n, Rcatch22::DN_HistogramMode_5),
#     DN_OutlierInclude_n_001_mdrmd = frollapply(close, n, Rcatch22::DN_OutlierInclude_n_001_mdrmd),
#     DN_OutlierInclude_p_001_mdrmd = frollapply(close, n, Rcatch22::DN_OutlierInclude_p_001_mdrmd),
#     FC_LocalSimple_mean1_tauresrat = frollapply(close, n, Rcatch22::FC_LocalSimple_mean1_tauresrat),
#     FC_LocalSimple_mean3_stderr = frollapply(close, n, Rcatch22::FC_LocalSimple_mean3_stderr),
#     IN_AutoMutualInfoStats_40_gaussian_fmmi = frollapply(close, n, Rcatch22::IN_AutoMutualInfoStats_40_gaussian_fmmi),
#     MD_hrv_classic_pnn40 = frollapply(close, n, Rcatch22::MD_hrv_classic_pnn40),
#     PD_PeriodicityWang_th0_01 = frollapply(close, n, Rcatch22::PD_PeriodicityWang_th0_01),
#     SB_BinaryStats_diff_longstretch0 = frollapply(close, n, Rcatch22::SB_BinaryStats_diff_longstretch0),
#     SB_BinaryStats_mean_longstretch1 = frollapply(close, n, Rcatch22::SB_BinaryStats_mean_longstretch1),
#     SB_MotifThree_quantile_hh = frollapply(close, n, Rcatch22::SB_MotifThree_quantile_hh),
#     SB_TransitionMatrix_3ac_sumdiagcov = frollapply(close, n, Rcatch22::SB_TransitionMatrix_3ac_sumdiagcov),
#     SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 = frollapply(close, n, Rcatch22::SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1),
#     SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 = frollapply(close, n, Rcatch22::SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1),
#     SP_Summaries_welch_rect_area_5_1 = frollapply(close, n, Rcatch22::SP_Summaries_welch_rect_area_5_1),
#     SP_Summaries_welch_rect_centroid = frollapply(close, n, Rcatch22::SP_Summaries_welch_rect_centroid)
#   )]
# }
