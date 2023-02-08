####################################################################
### SETTINGS + LIBRARIES
####################################################################
pacman::p_load(tidyverse,
               haven, 
               sjlabelled, 
               sjmisc,
               usethis)
# Clear
use_blank_slate()

####################################################################
### DATA
####################################################################

# DART0034_OUTPUT.dta is survey data provided by YouGov.
# DART0034_CCESpost_append_v2.dta is 2018 CCES data provided by YouGov.
# active.tsv is user-level browser activity data.
# Note that activity/merged.tsv and activity/youtube/merged.tsv is on a secure server and cannot be shared.

# YouGov data
YG <- read_dta("./Data/DART0034/DART0034_OUTPUT.dta")
CCES <- read_dta("./Data/DART0034/DART0034_CCESpost_append_v2.dta")
new_weights <- read_csv('./Data/DART0034/dart0034-combined-weight.csv')
crosswalk <- read_csv("./Data/DART0034/DART0034_crosswalk.csv") %>% 
  select(caseid_d34 = caseid,
         user_id = visa_DART0034) %>% 
  distinct()
#add new weights
YG <- YG %>% 
  left_join(new_weights, by = 'caseid')

# aggregated user-level browser extension data
agg_browser <- read_tsv("data/active.tsv") %>%
  left_join(crosswalk, by = "user_id") %>% 
  mutate(browser_sample = 1) 

channel_types <- c("alternative",
                   "extremist",
                   "mainstream",
                   "other")

### --- rename YG variables to be more intuitive
YG <- YG %>% 
  select(ideology = q1,
         polint = q2,
         trump_approve = q3, 
         rr_blk_minorities = q4_1,
         rr_blk_slavery = q4_2,
         rr_blk_deserve = q4_3,
         rr_blk_try_hard = q4_4,
         rr_over_wte_adv = q4_5,
         rr_over_rare = q4_6,
         fem_complain = q4_7,
         fem_reasonable = q4_8,
         freq_play_games = q5,
         agg_hit = q6_2,
         agg_disagree = q6_4,
         agg_argue = q6_5,
         agg_temper = q6_7,
         agg_flare_up = q6_9,
         agg_raw_deal = q6_10,
         agg_blows = q6_1,
         agg_threaten = q6_3,
         agg_argumentative = q6_6,
         agg_off_handle = q6_8,
         agg_get_breaks = q6_11,
         agg_bitter = q6_12,
         cons_plots = q7_1,
         cons_democ = q7_2,
         cons_not_known = q7_3,
         cons_secret = q7_4,
         trust_media = q8,
         news_accurate = q9,
         # why do q10s and q11s have so many non-responses?
         trust_info_orgs = q12_1,
         trust_info_celeb = q12_2,
         trust_info_distant_ppl = q12_3,
         trust_info_close_online = q12_4,
         trust_info_close_offline = q12_5,
         trust_info_mass_media = q12_6,
         google_freq_use = q13,
         google_info_accurate = q14,
         google_personalize = q15,
         google_search = q28,
         google_info_trust = q29,
         google_search_bias = q30,
         youtube_freq_use = yt_freq,
         youtube_info_accurate = q17,
         youtube_personalize = q18,
         youtube_vid_quality = q31,
         youtube_vid_trust = q32,
         youtube_vid_bias = q33,
         covid_self = q34_1,
         covid_family = q34_2,
         covid_threat = q35,
         covid_avoid_crowd = q36_2,
         covid_masks = q36_3,
         covid_symptoms = q36_4,
         covid_hydroxy = q36_6,
         covid_ccp = q36_8,
         covid_gates = q36_10,
         covid_handwash = q36_1,
         covid_senses = q36_5,
         covid_5Gcell = q36_7,
         covid_exaggerated = q36_9,
         vacc_protect_children = q37_1,
         vacc_doctor = q37_2,
         vacc_safe = q37_3,
         vacc_children_common = q37_4,
         vacc_side_effects =  q37_5,
         vacc_autism = q37_6,
         vacc_achievement = q37_7,
         serious = q38,
         lookup_info = q39,
         everything()) %>% 
  mutate_at(
    vars('race', 'educ', 'gender', 'presvote16post',
         'employ', 'marstat', 'religpew', 'votereg',
         'polint'),
    list(~as_label(.))
  ) 

### ----------------- merge browser and yg data
merged_yg_browser <- YG %>%
  rename(user_id = session_visa) %>%
  left_join(agg_browser, by = "user_id") %>%
  # reverse code  some rr vars first --- higher is higher racial resentment (don't do anything to ones in correct direction)
  mutate(
    rr_blk_minorities = (max(rr_blk_minorities) - rr_blk_minorities) + min(rr_blk_minorities),
    rr_blk_try_hard = (max(rr_blk_try_hard) - rr_blk_try_hard) + min(rr_blk_try_hard),
    rr_over_rare = (max(rr_over_rare) - rr_over_rare) + min(rr_over_rare),
    # same for fem scale --- higher is higher gender resentment
    fem_complain = (max(fem_complain) - fem_complain) + min(fem_complain)
  ) %>%
  mutate(
    # racial resentment
    rr_blk_combined = rowSums(.[names(.)[grepl("^rr_blk_", names(.))]]),
    # racism overstated
    rr_over_combined = rowSums(.[names(.)[grepl("^rr_over_", names(.))]]),
    rr_blk_respondent_answered = rowSums(!is.na(.[names(.)[grepl("^rr_blk_", names(.))]])),
    rr_over_respondent_answered = rowSums(!is.na(.[names(.)[grepl("^rr_over_", names(.))]])),
    rr_blk_mean = rr_blk_combined / rr_blk_respondent_answered,
    rr_over_mean = rr_over_combined / rr_over_respondent_answered,
    # zscore or 0 to 1 normalization?
    rr_blk_std = (rr_blk_mean - min(rr_blk_mean)) / (max(rr_blk_mean) - min(rr_blk_mean)),
    rr_over_std = (rr_over_mean - min(rr_over_mean)) / (max(rr_over_mean) - min(rr_over_mean)),
    # gender resentment
    fem_combined = rowSums(.[names(.)[grepl("^fem_", names(.))]]),
    fem_respondent_answered = rowSums(!is.na(.[names(.)[grepl("^fem_", names(.))]])),
    fem_mean = fem_combined / fem_respondent_answered,
    fem_std = (fem_mean - min(fem_mean)) / (max(fem_mean) - min(fem_mean)),
    # group youtube categories
    youtube_freq_use_3 = factor(
      case_when(
        youtube_freq_use <= 3 ~ "Once a day or more",
        youtube_freq_use > 3 &
          youtube_freq_use <= 5 ~ "Once or a few times a week",
        youtube_freq_use > 5 ~ "A few times a month to never",
        TRUE ~ NA_character_
      ),
      levels = c(
        "A few times a month to never",
        "Once or a few times a week",
        "Once a day or more"
      )
    )
  ) %>%
  mutate(
    presvote16post  = ifelse(
      presvote16post %in% c('Hillary Clinton', 'Donald Trump'),
      as.character(presvote16post),
      "Other"
    ),
    race = factor(
      case_when(
        !race %in% c("White", "Black", "Hispanic", "Asian") ~ "Other",
        TRUE ~ as.character(race)
      ),
      levels = c("White", "Black", "Hispanic", "Asian", "Other")
    ),
    religion = case_when(
      religpew == "Protestant" ~ "Protestant",
      religpew == "Roman Catholic" ~ "Roman Catholic",
      religpew %in% c("Atheist", "Agnostic", "Nothing in particular") ~ "Atheist/Agnostic",
      TRUE ~ "Other"
    ),
    marital = case_when(
      !marstat %in% c("Married", "Divorced", "Never married") ~ "Other",
      TRUE ~ as.character(marstat)
    ),
    employ = case_when(
      employ %in% c("Full-time", "Part-time", "") ~ "Employed",
      employ %in% c("Unemployed", "Temporarily laid off") ~ "Unemployed",
      TRUE ~ "Other"
    ),
    faminc_new = case_when(
      faminc_new == 1 ~ "< $10,000",
      faminc_new %in% c(2:3) ~ "$10,000 - $29,999",
      faminc_new %in% c(4:7) ~ "$30,000 - $69,999",
      faminc_new %in% c(8:10) ~ "$70,000 - $119,999",
      faminc_new %in% c(11:16) ~ "> $120,000",
      TRUE ~ "Other"
    ),
    faminc_new = factor(
      faminc_new,
      levels = c(
        "< $10,000",
        "$10,000 - $29,999",
        "$30,000 - $69,999",
        "$70,000 - $119,999",
        "> $120,000"
      )
    ),
    pid3 = case_when(pid3 == 1 ~ "Democrat",
                     pid3 == 2 ~ "Republican",
                     pid3 == 3 ~ "Independent"),
    pid3 = factor(pid3, levels = c("Democrat", "Independent", "Republican")),
    pid_lean = case_when(pid7%in% 1:3 ~ "Democrat",
                         pid7%in% 5:7 ~ "Republican",
                         pid7 %in% 4 ~ "Independent"),
    pid_lean = factor(pid_lean, levels = c( "Independent", "Democrat", "Republican")),
    age = 2020 - birthyr,
    #18-34, 35-54, 55-64, 65+
    age_group = case_when(
      age %in% 18:34 ~ "18-34",
      age %in% 35:54 ~ "35-54",
      age %in% 55:64 ~ "55-64",
      age >= 65 ~ "65+"
    ),
    polint = sjlabelled::as_factor(
      polint,
      levels = c(
        "Extremely interested",
        "Very interested",
        "Somewhat interested",
        "Not very interested",
        "Not at all interested"
      ),
      ordered = T
    ),
    educ2 = case_when(
      educ %in% "High school graduate" ~ "High school graduate",
      educ %in% c("Some college", "2-year") ~ "Some college",
      educ %in% "4-year" ~ "4-year",
      educ %in% "Post-grad" ~ "Post-grad",
      TRUE ~ NA_character_
    ),
    educ2 = factor(educ2,
                   levels =    c("High school graduate",
                                 "Some college",
                                 "4-year",
                                 "Post-grad")),
    browser_sample = ifelse(!is.na(browser_sample), "browser", "full"))

### --- merge with 2018 CCES (422 are resentment variables)
youtube_vars <- merged_yg_browser %>% 
  select(contains("yt")) %>% 
  names()

merged_all <- merged_yg_browser %>% 
  left_join(CCES %>% rename(caseid = DART0034_caseid), by = "caseid") %>% 
  # reverse code  some rr vars first --- higher is higher racial resentment (don't do anything to ones in correct direction)
  mutate(
    CC18_422e = (max(CC18_422e, na.rm = T)+1) - CC18_422e,
    CC18_422h = (max(CC18_422h, na.rm = T)+1) - CC18_422h ,
    CC18_422b = (max(CC18_422b, na.rm = T)+1) - CC18_422b,
    # anti-fem
    CC18_422c  = (max(CC18_422c, na.rm = T)+1) - CC18_422c
  ) %>% 
  mutate( # racial resentment
    rr_blk_combined_cces = rowSums(.[names(.)[names(.) %in% c("CC18_422e", "CC18_422f", "CC18_422g", "CC18_422h")]], na.rm = T),
    # racism overstated
    rr_over_combined_cces = rowSums(.[names(.)[ names(.) %in% c("CC18_422a", "CC18_422b")]], na.rm=T),
    rr_blk_respondent_answered_cces = rowSums(!is.na(.[names(.)[names(.) %in% c("CC18_422e", "CC18_422f", "CC18_422g", "CC18_422h")]]), na.rm=T),
    rr_over_respondent_answered_cces = rowSums(!is.na(.[names(.)[names(.) %in% c("CC18_422a", "CC18_422b")]]), na.rm=T),
    rr_blk_mean_cces = rr_blk_combined_cces / rr_blk_respondent_answered_cces ,
    rr_over_mean_cces = rr_over_combined_cces  / rr_over_respondent_answered_cces ,
    # minmax normalization
    rr_blk_std_cces = (rr_blk_mean_cces - min(rr_blk_mean_cces)) / (max(rr_blk_mean_cces) - min(rr_blk_mean_cces)),
    rr_over_std_cces = (rr_over_mean_cces - min(rr_over_mean_cces)) / (max(rr_over_mean_cces) - min(rr_over_mean_cces)),
    #fem
    fem_mean_cces = rowMeans(select(., CC18_422c, CC18_422d), na.rm = T)
  ) %>% 
  mutate(
    rr_tercile = case_when(
      rr_blk_mean_cces <= quantile(rr_blk_mean_cces, 1 / 3, na.rm = T) ~ "low",
      rr_blk_mean_cces > quantile(rr_blk_mean_cces, 1 /
                                    3, na.rm = T) &
        rr_blk_mean_cces <= quantile(rr_blk_mean_cces, 2 / 3, na.rm = T) ~ "med",
      rr_blk_mean_cces > quantile(rr_blk_mean_cces, 2 /
                                    3, na.rm = T) ~ "high"
    ),
    rr_tercile = factor(rr_tercile, levels = c("low", "med", "high")),
    ft_jew_tercile = case_when(
      feeling_Jews <= quantile(feeling_Jews, 1 / 3, na.rm = T) ~ "low",
      feeling_Jews > quantile(feeling_Jews, 1 /
                                3, na.rm = T) &
        feeling_Jews <= quantile(feeling_Jews, 2 / 3, na.rm = T) ~ "med",
      feeling_Jews > quantile(feeling_Jews, 2 /
                                3, na.rm = T) ~ "high"
    ),
    ft_jew_tercile = factor(ft_jew_tercile, levels = c("low", "med", "high")),
    ft_jew_binned = case_when(
      feeling_Jews <= 40 ~ "cold",
      feeling_Jews > 40 &
        feeling_Jews <= 60 ~ "medium",
      feeling_Jews > 60 ~ "warm"
    ),
    ft_jew_binned = factor(ft_jew_binned, levels = rev(c("warm", "medium", "cold"))),
    race = factor(case_when(
      race == "White" ~ "White",
      race != "White" ~ "Non-white"
    ), levels = c("White", "Non-white"))
  ) %>% 
  rename(rr_cts = rr_blk_mean_cces,
         jw_cts = feeling_Jews,
         fem_cts = fem_mean_cces)  %>%
  # impute 0 for youtube NA and activity not null
  mutate(across(
    .cols = all_of(youtube_vars[str_detect(youtube_vars, "activity")]),
    .fns = ~ ifelse(
      !is.na(activity_n_total) & is.numeric(.) & browser_sample == "browser",
      tidyr::replace_na(., 0),
      .)
  ))%>%
  # impute zeros browser history 
  mutate(across(
    .cols = all_of(youtube_vars[str_detect(youtube_vars, "browser_history")]),
    .fns = ~ ifelse(
      !is.na(browser_history_n_total) & is.numeric(.) & browser_sample == "browser",
      tidyr::replace_na(., 0),
      .)
  )) %>% 
  mutate(
    bh_alt = if_else((
      browser_history_yt_n_video_alternative_all > 0 
    ), 1, 0 ),
    bh_ext = if_else((
      browser_history_yt_n_video_extremist_all > 0
    ), 1, 0 ),
    bh_msm = if_else((
      browser_history_yt_n_video_mainstream_all > 0 
    ), 1, 0 ),
    bh_other = if_else((
      browser_history_yt_n_video_other_all > 0 
    ), 1, 0 ),
    at_alt = if_else((
      activity_yt_n_video_alternative_all > 0 
    ), 1, 0 ),
    at_ext = if_else((
      activity_yt_n_video_extremist_all > 0
    ), 1, 0 ),
    at_msm = if_else((
      activity_yt_n_video_mainstream_all > 0
    ), 1, 0 ),
    at_other = if_else((
      activity_yt_n_video_other_all > 0
    ), 1, 0 ))


##########################

# save final merge of all three
write_rds(merged_all, "data/yg_browser_cces_merged.rds")

# only extension data
yg_browser_data <- merged_all %>% 
  filter(browser_sample == "browser")
###================= separate data by different metrics 
### BROWSER HISTORY
bh_data <- yg_browser_data %>%
  mutate(
    bh_any_alt_ext = if_else((
      browser_history_yt_n_video_alternative_all > 0 |
        browser_history_yt_n_video_extremist_all > 0
    ), 1, 0 ),
    bh_alt_and_ext = if_else((
      browser_history_yt_n_video_alternative_all > 0 &
        browser_history_yt_n_video_extremist_all > 0
    ), 1, 0 ),
    bh_prop_alt_of_total = browser_history_yt_n_video_alternative_all / browser_history_yt_n_video_all,
    bh_prop_ext_of_total = browser_history_yt_n_video_extremist_all / browser_history_yt_n_video_all,
    bh_prop_msm_of_total = browser_history_yt_n_video_mainstream_all / browser_history_yt_n_video_all,
    bh_n_video_alt_ext = rowSums(.[c(
      "browser_history_yt_n_video_alternative_all",
      "browser_history_yt_n_video_extremist_all"
    )]),
    bh_prop_any_of_total = bh_n_video_alt_ext / browser_history_yt_n_video_all,
    bh_time_elapsed_total_alt_ext = rowSums(.[c(
      "browser_history_yt_video_time_elapsed_total_alternative_all",
      "browser_history_yt_video_time_elapsed_total_extremist_all"
    )]),
    bh_prop_time_alt_ext = bh_time_elapsed_total_alt_ext/browser_history_yt_time_elapsed_video,
    bh_prop_time_alt = browser_history_yt_video_time_elapsed_capped_total_alternative_all/browser_history_yt_time_elapsed_capped_video,
    bh_prop_time_ext = browser_history_yt_video_time_elapsed_capped_total_extremist_all/browser_history_yt_time_elapsed_capped_video,
    bh_prop_time_msm = browser_history_yt_video_time_elapsed_capped_total_mainstream_all/browser_history_yt_time_elapsed_capped_video,
    bh_any_alt_ext_norm = (bh_any_alt_ext/browser_history_yt_window)*100
  ) %>% 
  mutate(bh_any_alternative_subscribed = ifelse(browser_history_yt_n_video_alternative_subscribed > 0 &
                                                  browser_history_yt_n_video_alternative_all > 0, 1, 0),
         bh_any_extremist_subscribed = ifelse(browser_history_yt_n_video_extremist_subscribed > 0 &
                                                browser_history_yt_n_video_extremist_all > 0, 1, 0),
         bh_any_mainstream_subscribed = ifelse(browser_history_yt_n_video_mainstream_subscribed > 0 &
                                                 browser_history_yt_n_video_mainstream_all > 0, 1, 0),
         bh_any_other_subscribed = ifelse(browser_history_yt_n_video_other_subscribed > 0 &
                                            browser_history_yt_n_video_other_all > 0, 1, 0),
         # % who watched ANY alt video while subscribed vs % who watched
         # an alt video but never one that was subscribed
         bh_any_alternative_notsubscribed = ifelse(browser_history_yt_n_video_alternative_notsubscribed > 0 &
                                                     browser_history_yt_n_video_alternative_subscribed < 1, 1, 0),
         bh_any_extremist_notsubscribed = ifelse(browser_history_yt_n_video_extremist_notsubscribed > 0 &
                                                   browser_history_yt_n_video_extremist_subscribed < 1, 1, 0),
         bh_any_mainstream_notsubscribed = ifelse(browser_history_yt_n_video_mainstream_notsubscribed > 0 &
                                                    browser_history_yt_n_video_mainstream_subscribed < 1, 1, 0),
         bh_any_other_notsubscribed = ifelse(browser_history_yt_n_video_other_notsubscribed > 0 &
                                               browser_history_yt_n_video_other_subscribed < 1, 1, 0))

#grouped time units
bh_time_all <- paste0(
  "browser_history_yt_video_time_elapsed_capped_total_",channel_types,"_all"
)
bh_time_subscribed <- paste0(
  "browser_history_yt_video_time_elapsed_capped_total_",channel_types,"_subscribed"
)

bh_time_notsubscribed <- paste0(
  "browser_history_yt_video_time_elapsed_capped_total_",channel_types,"_notsubscribed"
)
minutes_bh_time_all <- paste0("minutes_", bh_time_all)
minutes_bh_time_subscribed <- paste0("minutes_", bh_time_subscribed)
minutes_bh_time_notsubscribed <- paste0("minutes_", bh_time_notsubscribed)

bh_data <- bh_data %>% 
  mutate(week = browser_history_window/7, # how many weeks are they in data
         fortnight = week/2, #n fortnightly
         month = week/4) %>%  # n monthly
  mutate(across(.cols = all_of(c(bh_time_all, bh_time_subscribed, bh_time_notsubscribed)),
                .fns = ~.x/60,
                .names = "minutes_{.col}")) %>% 
  mutate(across(.cols = all_of( c(minutes_bh_time_all)),
                .fns = ~.x/week,
                .names = "{.col}_week")) %>% 
  mutate(across(.cols = all_of( c(minutes_bh_time_all, minutes_bh_time_subscribed, minutes_bh_time_notsubscribed)),
                .fns = ~.x/week,
                .names = "{.col}_week")) %>% 
  mutate(across(.cols = all_of(c(minutes_bh_time_all, minutes_bh_time_subscribed, minutes_bh_time_notsubscribed)),
                .fns = ~.x/fortnight,
                .names = "{.col}_fortnight")) %>% 
  mutate(across(.cols = all_of(c(minutes_bh_time_all, minutes_bh_time_subscribed, minutes_bh_time_notsubscribed)),
                .fns = ~.x/month,
                .names = "{.col}_month")) %>% 
  select(!starts_with("activity_")) %>%
  filter(!is.na(browser_history_n_total)) %>% # only people with any browser history data
  filter(browser_history_window > 1) 

write_rds(bh_data, "data/browser_history_yg_cces.rds")


### ACTIVITY
at_data <- yg_browser_data %>%
  mutate(
    at_any_alt_ext = if_else((
      activity_yt_n_video_alternative_all > 0 |
        activity_yt_n_video_extremist_all > 0
    ), 1, 0 ),
    at_alt_and_ext = if_else((
      activity_yt_n_video_alternative_all > 0 &
        activity_yt_n_video_extremist_all > 0
    ), 1, 0 ),
    at_prop_alt_of_total = activity_yt_n_video_alternative_all / activity_yt_n_video_all,
    at_prop_ext_of_total = activity_yt_n_video_extremist_all / activity_yt_n_video_all,
    at_prop_msm_of_total = activity_yt_n_video_mainstream_all / activity_yt_n_video_all,
    at_n_video_alt_ext = rowSums(.[c(
      "activity_yt_n_video_alternative_all",
      "activity_yt_n_video_extremist_all"
    )]),
    at_prop_any_of_total = at_n_video_alt_ext / activity_yt_n_video_all,
    at_time_elapsed_total_alt_ext = rowSums(.[c(
      "activity_yt_video_time_elapsed_capped_total_alternative_all",
      "activity_yt_video_time_elapsed_capped_total_extremist_all"
    )]),
    at_prop_time_alt_ext = at_time_elapsed_total_alt_ext/activity_yt_time_elapsed_video,
    at_prop_time_alt = activity_yt_video_time_elapsed_capped_total_alternative_all/activity_yt_time_elapsed_capped_video,
    at_prop_time_ext = activity_yt_video_time_elapsed_capped_total_extremist_all/activity_yt_time_elapsed_capped_video,
    at_prop_time_msm = activity_yt_video_time_elapsed_capped_total_mainstream_all/activity_yt_time_elapsed_capped_video,
    at_any_alt_ext_norm = (at_any_alt_ext/activity_yt_window)*100
  )  %>% 
  # % who watched ANY alt video while subscribed vs 
  mutate(at_any_alternative_subscribed = ifelse(activity_yt_n_video_alternative_subscribed > 0, 1, 0),
         at_any_extremist_subscribed = ifelse(activity_yt_n_video_extremist_subscribed > 0, 1, 0),
         at_any_mainstream_subscribed = ifelse(activity_yt_n_video_mainstream_subscribed > 0, 1, 0),
         at_any_other_subscribed = ifelse(activity_yt_n_video_other_subscribed > 0, 1, 0),
         # % who watched an alt video but never one that was subscribed
         at_any_alternative_notsubscribed = ifelse(activity_yt_n_video_alternative_notsubscribed > 0 &
                                                     activity_yt_n_video_alternative_subscribed < 1, 1, 0),
         at_any_extremist_notsubscribed = ifelse(activity_yt_n_video_extremist_notsubscribed > 0 &
                                                   activity_yt_n_video_extremist_subscribed < 1, 1, 0),
         at_any_mainstream_notsubscribed = ifelse(activity_yt_n_video_mainstream_notsubscribed > 0 &
                                                    activity_yt_n_video_mainstream_subscribed < 1, 1, 0),
         at_any_other_notsubscribed = ifelse(activity_yt_n_video_other_notsubscribed > 0 &
                                               activity_yt_n_video_other_subscribed < 1, 1, 0))


#grouped time units
at_time_all <- paste0(
  "activity_yt_video_time_elapsed_capped_total_",channel_types,"_all"
)
at_time_subscribed <- paste0(
  "activity_yt_video_time_elapsed_capped_total_",channel_types,"_subscribed"
)

at_time_notsubscribed <- paste0(
  "activity_yt_video_time_elapsed_capped_total_",channel_types,"_notsubscribed"
)
minutes_at_time_all <- paste0("minutes_", at_time_all)
minutes_at_time_subscribed <- paste0("minutes_", at_time_subscribed)
minutes_at_time_notsubscribed <- paste0("minutes_", at_time_notsubscribed)

at_data <- at_data %>% 
  mutate(week = activity_window/7, # how many weeks are they in data
         fortnight = week/2, #n fortnightly
         month = week/4) %>%  # n monthly
  mutate(across(.cols = all_of(c(at_time_all, at_time_subscribed, at_time_notsubscribed)),
                .fns = ~.x/60,
                .names = "minutes_{.col}")) %>% 
  mutate(across(.cols = all_of( c(minutes_at_time_all)),
                .fns = ~.x/week,
                .names = "{.col}_week")) %>% 
  mutate(across(.cols = all_of( c(minutes_at_time_all, minutes_at_time_subscribed, minutes_at_time_notsubscribed)),
                .fns = ~.x/week,
                .names = "{.col}_week")) %>% 
  mutate(across(.cols = all_of(c(minutes_at_time_all, minutes_at_time_subscribed, minutes_at_time_notsubscribed)),
                .fns = ~.x/fortnight,
                .names = "{.col}_fortnight")) %>% 
  mutate(across(.cols = all_of(c(minutes_at_time_all, minutes_at_time_subscribed, minutes_at_time_notsubscribed)),
                .fns = ~.x/month,
                .names = "{.col}_month")) %>% 
  mutate(minutes_at_yt_video_time_elapsed_capped_total_week =
           minutes_activity_yt_video_time_elapsed_capped_total_alternative_all_week + 
           minutes_activity_yt_video_time_elapsed_capped_total_extremist_all_week + 
           minutes_activity_yt_video_time_elapsed_capped_total_mainstream_all_week + 
           minutes_activity_yt_video_time_elapsed_capped_total_other_all_week) %>%  
  select(!starts_with("browser_history_")) %>% # remove bh vars 
  filter(!is.na(activity_n_total)) %>% # only keep people who have any activity data
  filter(activity_window > 1) # keep people with more than one day of acivity

write_rds(at_data, "data/activity_yg_cces.rds")

# want to confine analysis to only users that have activity data and survey data
# so, will filter based on these IDs
final_sample_ids <- at_data  %>% 
  pull(user_id)
write_rds(final_sample_ids, "data/final_sample_ids.rds")

####################################################################
### REFERRERS DATA
####################################################################
# CODE ONLY, THESE DATA IS ON SECURE SERVER
# define referrer domains
external_referrers <- list(
  # other social 
  main_social = c("facebook.com",
                  "apps.facebook.com",
                  "messenger.com",
                  "twitter.com",
                  "tweetdeck.twitter.com",
                  "instagram.com",
                  "linkedin.com",
                  "discord.com",
                  "tiktok.com",
                  "tumblr.com",
                  "tinder.com",
                  "bumble.com",
                  "pinterest.com",
                  "twitch.tv",
                  "reddit.com",
                  "old.reddit.com"),
  # alternative social
  alternative_social = c("twitchy.com",
                         "gab.com",
                         "parler.com",
                         "banned.video",
                         "boards.4channel.org",
                         "boards.4chan.org",
                         "4chan.org"),
  # search engine
  search_engine = c("google.com",
                    "bing.com",
                    "yahoo.com", 
                    "search.yahoo.com",
                    "duckduckgo.com"),
  # webmail
  webmail = c("mail.google.com",
              "mail.yahoo.com",
              "mail.com",
              "mail.aol.com",
              "outlook.office365.com",
              "outlook.office.com",
              "outlook.live.com")
)

internal_referrers_other <- c(
  "playlist",
  "post",    
  "custom",  
  "other",   
  "user",    
  "redirect"
)

internal_referrers <- c(
  "youtube homepage",
  "youtube video",   
  "youtube search",  
  "youtube channel"
)

referrers_list <- list(
  external_referrers = external_referrers,
  internal_referrers_other = internal_referrers_other,
  internal_referrers = internal_referrers
)

external_referrers <- referrers_list$external_referrers
internal_referrers_other <- referrers_list$internal_referrers_other
internal_referrers <- referrers_list$internal_referrers

# ACCESS BY SERVER ONLY
all_activity_data <- read_delim(paste0(data_dir,'activity/merged.tsv'), delim='\t') # (not user-level) all activity data
youtube_urls <- read_delim(paste0(data_dir, 'activity/youtube/merged.tsv'), delim='\t') # youtube videos (activity)

# merge
all_activity_youtube_data <- all_activity_data %>% 
  left_join(youtube_urls, by = 'id', suffix = c('_all', '_youtube')) 

# create variables for immediate preceding url (by timestamp)
tidy_merged <- all_activity_youtube_data %>% 
  mutate(
    channel_type = case_when(
      alternative_match == 1 ~ "alternative",
      extremist_match == 1 ~ "extremist",
      mainstream_match == 1 ~ "mainstream",
      other_match == 1 ~ "other"
    ),
    url_type = recode(
      url_type,
      `homepage` = "youtube homepage",
      `video` = "youtube video",
      `search` = "youtube search",
      `channel` = "youtube channel"
    )
  ) %>% 
  group_by(user_id_all, session_all) %>% # within broader (non-youtube sessions), for each user 
  arrange(user_id_all, session_all, timestamp_all) %>% # sorted timeline
  group_by(user_id_all, session_all) %>%
  mutate(predecessor_is_youtube = if_else(lag(domain_all, n = 1L) == "youtube.com", 1, 0),
         predecessor_domain = lag(domain_all, n = 1L),
         predecessor_url = lag(url_all, n = 1L),
         predecessor_channel_type = lag(channel_type, n = 1L),
         predecessor_youtube_url_type = lag(url_type, n = 1L)) %>% 
  ungroup()
# 

#
predecessor_grouping_data <- tidy_merged %>% 
  mutate(predecessor_group = case_when(!is.na(predecessor_channel_type) & predecessor_is_youtube == 1 ~ predecessor_channel_type,
                                       predecessor_is_youtube == 1 & is.na(predecessor_channel_type) ~ "on-platform",
                                       (predecessor_is_youtube != 1) & domain_all == "youtube.com" ~ "off-platform"),
         internal_referrers_grouped = case_when(predecessor_youtube_url_type %in% internal_referrers_other ~ "other on-platform",
                                                TRUE ~ predecessor_youtube_url_type),
         external_referrers_grouped = case_when(predecessor_domain %in% external_referrers$main_social ~ "main social",
                                                predecessor_domain %in% external_referrers$alternative_social ~ "alternative social",
                                                predecessor_domain %in% external_referrers$search_engine ~ "search engine",
                                                predecessor_domain %in% external_referrers$webmail ~ "webmail",
                                                !is.na(predecessor_domain) & (!predecessor_domain %in% unlist(external_referrers)) & is.na(internal_referrers_grouped) ~ "other off-platform"),
         all_referrers_grouped = coalesce(internal_referrers_grouped, external_referrers_grouped),
         youtube_video_referrers_by_channel_type = case_when(
           predecessor_youtube_url_type == "youtube video" ~  predecessor_channel_type,
           predecessor_youtube_url_type %in% c("youtube search", "youtube channel", "youtube homepage",
                                               internal_referrers_other) ~  "Non-video on-platform",
           is.na(predecessor_youtube_url_type) & (predecessor_is_youtube != 1) & !is.na(predecessor_is_youtube)~  "Off-platform"
         ),
         is_youtube_video = if_else(!is.na(video_id),1,0),
         on_off_platform = case_when(
           all_referrers_grouped == internal_referrers_grouped ~ "on-platform",
           all_referrers_grouped == external_referrers_grouped ~ "off-platform"
         ))

referrers_data <- predecessor_grouping_data %>% 
  select(timestamp_all, session_all, user_id_all, 
         url_all, predecessor_url, video_id, channel_id,
         domain_all, channel_type, url_type, 
         starts_with("predecessor_"),
         all_referrers_grouped,
         youtube_video_referrers_by_channel_type,
         is_youtube_video,
         on_off_platform)

# only referrers to youtube videos
youtube_referrers_data <-  referrers_data %>%
  filter(user_id_all %in% final_sample_ids)%>% 
  filter(is_youtube_video == 1) 

write_tsv(referrers_data, 
          paste0(data_dir, 'youtube/referrers_data.tsv'))
write_csv(youtube_referrers_data, 
          paste0(data_dir, 'youtube/youtube_referrers.csv'))

activity_data <- sample_data %>% 
  filter(browser_sample=='browser' & activity_window>1) %>% 
  select(user_id, browser_sample, samplegroup, weight_cmd)

yt_smp_merge_data <- youtube_referrers_data  %>%  
  left_join(activity_data,
            by = c("user_id_all"="user_id"))

on_platform_referrers_by_channel_wtd <- yt_smp_merge_data %>%
  group_by(user_id_all, channel_type, youtube_video_referrers_by_channel_type) %>%
  summarise(n = sum(weight_cmd)) %>%
  filter(!is.na(youtube_video_referrers_by_channel_type)) %>%
  group_by(channel_type, youtube_video_referrers_by_channel_type) %>%
  summarise(n = sum(n)) %>%
  mutate(
    total = sum(n),
    proportion = (n / sum(n)),
    std_err = sqrt((proportion * (1 - proportion)) / total),
    ci_lwr = 100 * (proportion - std_err * 1.96),
    ci_upr = 100 * (proportion + std_err * 1.96),
    percentage = 100 * proportion
  ) %>%
  ungroup() %>%
  mutate(
    channel_type_referrer = interaction(channel_type, youtube_video_referrers_by_channel_type),
    youtube_video_referrers_by_channel_type = factor(
      youtube_video_referrers_by_channel_type,
      levels = c(
        "alternative",
        "extremist",
        "mainstream",
        'other',
        'Non-video on-platform',
        'Off-platform'
      )
    )
  ) %>%
  arrange(channel_type, youtube_video_referrers_by_channel_type) %>%
  mutate(order_var = 1:nrow(.))

write_csv(on_platform_referrers_by_channel_wtd, 
          'data/on_platform_referrers_by_channel_wtd.csv')


aggregated_referrers_by_channel_wtd <- yt_smp_merge_data %>%
  group_by(user_id_all, channel_type, all_referrers_grouped) %>%
  summarise(n = sum(weight_cmd)) %>%
  filter(!is.na(all_referrers_grouped)) %>%
  group_by(channel_type, all_referrers_grouped) %>%
  summarise(n = sum(n)) %>%
  mutate(
    total = sum(n),
    proportion = (n / sum(n)),
    std_err = sqrt((proportion * (1 - proportion)) / total),
    ci_lwr = 100 * (proportion - std_err * 1.96),
    ci_upr = 100 * (proportion + std_err * 1.96),
    percentage = 100 * proportion,
    on_off_platform = case_when(
      all_referrers_grouped %in% c(str_replace(names(external_referrers), "_", " "), "other off-platform") ~ "off-platform",
      all_referrers_grouped %in% c(internal_referrers, "other on-platform") ~ "on-platform"
    )
  ) %>%
  ungroup() %>%
  mutate(channel_type_referrer = interaction(channel_type, all_referrers_grouped)) %>%
  arrange(channel_type, on_off_platform) %>%
  mutate(order_var = 1:nrow(.)) %>%
  arrange(on_off_platform, percentage) %>%
  mutate(
    all_referrers_grouped = factor(all_referrers_grouped, levels = unique(.$all_referrers_grouped)),
    all_referrers_grouped = str_replace(all_referrers_grouped, "youtube", "YouTube")
  )

write_csv(aggregated_referrers_by_channel_wtd, 
          paste0('data/aggregated_referrers_by_channel_wtd.csv'))

####################################################################
### RECOMMENDATIONS DATA
####################################################################
# CODE ONLY, SOURCE DATA IS ON SECURE SERVER

# video visits
youtube_visits <- read_delim(paste0(data_dir, 'youtube/merged.tsv'), delim = '\t')
youtube_video_visits <- youtube_visits %>% 
  filter(!is.na(video_id))
# recs shown
youtube_recs_shown_summary <- read_delim(paste0(data_dir, 'youtube/video_recs_visit_summary.tsv'), delim = '\t')
# recs followed
youtube_recs_followed_summary  <- read_delim(paste0(data_dir, 'youtube/video_recs_matches_visit_summary.tsv'),
                                             delim = '\t')

### === merge data
youtube_merged_visits_recs <- youtube_video_visits %>% 
  left_join(youtube_recs_shown_summary, by = "id") %>% 
  left_join(youtube_recs_followed_summary, by = "id") %>% 
  filter(user_id %in% final_sample_ids)  %>% 
  left_join(activity_data %>% select(user_id, weight_cmd),
            by = 'user_id')

### === create recs shown/followed table conditional on the channel type of current video
channel_types = c(
  "alternative",
  "extremist",
  "mainstream",
  "other"
)

channel_type_vars <- c(
  'alternative_match',
  'extremist_match',
  'mainstream_match',
  'other_match'
)

recs_shown_vars <- c(
  'rec_n_video_alternative_all',
  'rec_n_video_extremist_all',
  'rec_n_video_mainstream_all',
  'rec_n_video_other_all'
)

recs_followed_vars <- c(
  'rec_match_alternative_match',
  'rec_match_extremist_match',
  'rec_match_mainstream_match',
  'rec_match_other_match'
)

recs_table_fxn <- function(channel_type_var, rec_statistic) {
  youtube_merged_visits_recs %>% 
    mutate(across(all_of(rec_statistic), .f = ~weight_cmd * .x)) %>% 
    select(user_id,
           all_of(rec_statistic),
           .data[[channel_type_var]]) %>% 
    group_by(user_id, .data[[channel_type_var]]) %>% 
    summarize(across(all_of(rec_statistic), .f = ~sum(.x, na.rm = T))) %>% 
    group_by(.data[[channel_type_var]]) %>% 
    summarize(across(all_of(rec_statistic), .f = ~sum(.x, na.rm = T))) %>% 
    filter(.data[[channel_type_var]] == 1) %>% 
    mutate(channel_type = str_replace(channel_type_var, "_match", "")) %>% 
    select(-.data[[channel_type_var]])
}

# recs shown
recs_shown_table <- bind_rows(map(channel_type_vars, ~recs_table_fxn(.x, recs_shown_vars))) %>% 
  pivot_longer(-channel_type, 
               names_to = 'variable') %>% 
  pivot_wider(variable, names_from = channel_type) 
# recs followed
recs_followed_table <- bind_rows(map(channel_type_vars, ~recs_table_fxn(.x, recs_followed_vars)))%>% 
  pivot_longer(-channel_type, 
               names_to = 'variable') %>% 
  pivot_wider(variable, names_from = channel_type)

#all totals
total_visit_table <- youtube_merged_visits_recs %>% 
  select(user_id, weight_cmd, all_of(channel_type_vars)) %>% 
  mutate(across(all_of(channel_type_vars), .f = ~weight_cmd * .x)) %>% 
  select(-user_id, -weight_cmd) %>% 
  summarize_all(.funs = ~sum(.x, na.rm = T)) %>% 
  rename_all(.funs = ~str_replace(.x, "_match", "")) %>% 
  mutate(variable =  "total visits" )

total_recs_shown_table <- recs_shown_table %>% 
  summarize(across(all_of(channel_types), .f = ~sum(.x, na.rm = T)))%>% 
  mutate(variable = "total recs shown")

total_recs_followed_table <- recs_followed_table_unwtd %>% 
  summarize(across(all_of(channel_types), .f = ~sum(.x, na.rm = T)))%>% 
  mutate(variable = "total recs followed")

totals_table <- bind_rows(total_visit_table, 
                          total_recs_shown_table, 
                          total_recs_followed_table)

# put totals and recs tables together to generate recommendation_pipeline.tsv
recommendation_pipeline_table <- totals_table %>%
  bind_rows(recs_shown_table) %>%
  bind_rows(recs_followed_table) %>%
  select(variable, everything())

write_tsv(recommendation_pipeline_table, 
          paste0(data_dir, 'youtube/recommendation_pipeline_wtd.tsv'))


####################################################################
### SUBSCRIPTIONS DATA
####################################################################
# CODE ONLY, SOURCE DATA IS ON SECURE SERVER


youtube_data = read_delim(paste0(data_dir, 'youtube/merged.tsv'), delim = '\t')
youtube_recs = read_delim(paste0(data_dir, 'youtube/video_recs_merged.tsv'),
                          delim = '\t')
youtube_recs_match = read_delim(paste0(data_dir, 'youtube/video_recs_matches_visit_summary.tsv'),
                                delim = '\t')
#youtube_urls = read_delim(paste0(data_dir, 'youtube/urls.tsv'), delim = '\t')

channel_types = c(
  "alternative",
  "extremist",
  "mainstream",
  "other"
)

### ===
youtube_video_visits <- youtube_data %>%
  select(id, user_id, timestamp, yt_session, yt_session_idx, video_id,
         channel_id, subscribed, ends_with("_match"), -adl_match) %>%
  pivot_longer(c('alternative_match',
                 'extremist_match',
                 'mainstream_match',
                 'other_match'),
               names_to = "channel_type", 
               values_to = "count") %>%
  filter(count == 1) %>% # lose some obs b/c not all are videos
  mutate(channel_type = str_replace(channel_type, "_match$", ""))

# rec_match_exposed_video_ids -- recommended videos shown 
# rec_match_video_id -- is this video followed OR video that they are on? (seems like recs followed)
youtube_video_recs <- youtube_recs_match %>% 
  mutate(rec_match_channel_type = case_when(
    rec_match_alternative_match == 1 ~ "alternative",
    rec_match_extremist_match == 1 ~ "extremist",
    rec_match_mainstream_match == 1 ~ "mainstream",
    rec_match_other_match == 1 ~ "other"
  )) %>% 
  select(-ends_with("_match"))

# merge visits with recs data
combined_with_recs <- youtube_video_visits %>% 
  left_join(youtube_video_recs, by = c('id'))

# function for tallying subs
cumulative_subs_fxn_v2 <- function(user, channel_category,  .d) {
  print(user)
  
  # starting values
  user_.d <- .d %>% 
    filter(user_id == user) %>% 
    arrange(timestamp)
  
  # which channels are the first time they hit a video from that channel, they are already subscribed
  channels_subscribed_tmp <- user_.d %>% 
    distinct(channel_id, .keep_all = TRUE) %>% 
    mutate(first_channel_subscribed = if_else(subscribed == 1 & channel_type == channel_category, 1, 0)) %>% 
    filter(first_channel_subscribed == 1) %>% 
    pull(channel_id)
  
  # empty vector to fill
  channels_subscribed <- list(mode = "character", length = nrow(user_.d))
  n_channels_subscribed <- vector(mode = "numeric", length = nrow(user_.d))
  for (i in 1:nrow(user_.d)) {
    print(i)
    # is channel of type of interest?
    if (user_.d[i,]$channel_type == channel_category) {
      # if they are subscribed to a channel of that type, add to temp channel list if not already on it
      if ((user_.d[i,]$subscribed == 1) & 
          (!user_.d[i,]$channel_id %in% channels_subscribed_tmp) & 
          !is.na(user_.d[i,]$subscribed)) {
        channels_subscribed_tmp <- append(channels_subscribed_tmp, user_.d[i,]$channel_id)
        channels_subscribed[[i]] <- channels_subscribed_tmp
        n_channels_subscribed[i] <- length(channels_subscribed_tmp)
        
        # if they are no longer subscribed to the channel, remove from channel list 
      } else if ((user_.d[i,]$subscribed == 0) & (user_.d[i,]$channel_id %in% channels_subscribed_tmp) & !is.na(user_.d[i,]$subscribed)) {
        channels_subscribed_tmp <- channels_subscribed_tmp[channels_subscribed_tmp != user_.d[i,]$channel_id]
        channels_subscribed[[i]] <- channels_subscribed_tmp
        n_channels_subscribed[i] <- length(channels_subscribed_tmp)
        # if not subscribed, keep number of previously subscribed
      } else {
        channels_subscribed[[i]] <- channels_subscribed_tmp
        n_channels_subscribed[i] <- length(channels_subscribed_tmp)
      } 
      # otherwise continue
    } else {
      channels_subscribed[[i]] <- channels_subscribed_tmp
      n_channels_subscribed[i] <- length(channels_subscribed_tmp)
    }
  }
  out <- tibble(user_.d, channels_subscribed, n_channels_subscribed)
  return(out)
}

# vector of user ids
unique_viewers <- combined_with_recs %>% 
  distinct(user_id) %>% 
  pull(user_id)

# run over all users who visited extremist content
cumulative_subs_outcome_v2 <- vector("list", length = 4)
for (category in channel_types) {
  cumulative_subs_outcome_v2[[category]] <-
    bind_rows(lapply(unique_viewers, function(.x)
      cumulative_subs_fxn_v2(.x,
                             channel_category = category,
                             .d = combined_with_recs))) 
}

summary_youtube_subs <-
  tibble(
    alternative = cumulative_subs_outcome_v2[['alternative']],
    extremist = cumulative_subs_outcome_v2[['extremist']],
    mainstream = cumulative_subs_outcome_v2[['mainstream']],
    other = cumulative_subs_outcome_v2[['other']]
  )

summary_youtube_recs <- youtube_recs %>%
  select(id, timestamp, user_id, video_id,
         alternative_match, extremist_match,  mainstream_match, other_match,
         subscribed) %>%
  group_by(id) %>%
  summarise(n_alternative_recs = sum(alternative_match),
            n_extremist_recs = sum(extremist_match),
            n_mainstream_recs = sum(mainstream_match),
            n_other_recs = sum(other_match))

# extremist
merged_cumulative_extremist <- summary_youtube_subs$extremist %>%
  distinct(id, .keep_all = T) %>% 
  select(id,  user_id, subscribed, channel_type,
         extremist_channels_subscribed  = channels_subscribed,
         extremist_n_channels_subscribed  = n_channels_subscribed) %>%
  left_join(summary_youtube_recs)
#alternative
merged_cumulative_alternative <-summary_youtube_subs$alternative %>%
  distinct(id, .keep_all = T) %>% 
  dplyr::select(id,  user_id,subscribed, channel_type,
                alternative_channels_subscribed  = channels_subscribed,
                alternative_n_channels_subscribed  = n_channels_subscribed) %>%
  distinct(id, .keep_all = T) %>% 
  left_join(summary_youtube_recs)
#mainstream
merged_cumulative_mainstream <- summary_youtube_subs$mainstream %>%
  dplyr::select(id, user_id, subscribed, channel_type,
                mainstream_channels_subscribed  = channels_subscribed,
                mainstream_n_channels_subscribed  = n_channels_subscribed) %>%
  distinct(id, .keep_all = T) %>% 
  left_join(summary_youtube_recs)
#other
merged_cumulative_other <- summary_youtube_subs$other  %>%
  distinct(id, .keep_all = T) %>% 
  dplyr::select(id,  user_id,subscribed, channel_type,
                other_channels_subscribed  = channels_subscribed,
                other_n_channels_subscribed  = n_channels_subscribed) %>%
  left_join(summary_youtube_recs)

# put all together
merged_cumulative_all <- left_join(merged_cumulative_extremist,
                                   merged_cumulative_alternative) %>%
  left_join(merged_cumulative_mainstream)  %>%
  left_join(merged_cumulative_other) %>%
  mutate(across(c('n_alternative_recs', 'n_extremist_recs', 'n_mainstream_recs', 'n_other_recs'),
                ~ifelse(.x > 0, 1, 0),
                .names = "{.col}_dummy")) %>%
  # add condition removing current channel
  mutate(cum_extremist_sub_no_current_channel = ifelse(subscribed == 1 & channel_type == "extremist", extremist_n_channels_subscribed - 1, extremist_n_channels_subscribed),
         cum_alternative_sub_no_current_channel = ifelse(subscribed == 1 & channel_type == "alternative", alternative_n_channels_subscribed - 1, alternative_n_channels_subscribed),
         cum_mainstream_sub_no_current_channel = ifelse(subscribed == 1 & channel_type == "mainstream", mainstream_n_channels_subscribed - 1, mainstream_n_channels_subscribed),
         cum_other_sub_no_current_channel = ifelse(subscribed == 1 & channel_type == "other", other_n_channels_subscribed - 1, other_n_channels_subscribed) )


# remove folks not matched to survey w/activity data
final_sample_ids <- activity_data$user_id
merged_cumulative_all <- merged_cumulative_all %>% 
  filter(user_id %in% final_sample_ids) %>% 
  # add survey sampling info
  left_join(activity_data)

#Bar graph of subscribed, not subscribed to that one but to another one, 
#not subscribed to any in channel - binary, one for alt and one for extremist
summary_youtube_subscriptions <- merged_cumulative_all %>% 
  mutate(
    subscribed_alternative = case_when(
      subscribed == 1 & channel_type == "alternative" ~ "subscribed to current",
      cum_alternative_sub_no_current_channel > 0 & channel_type == "alternative" ~ "subscribed to another",
      cum_alternative_sub_no_current_channel == 0 & channel_type == "alternative"~ "not subscribed"
    ),
    subscribed_extremist = case_when(
      subscribed == 1 & channel_type == "extremist" ~ "subscribed to current",
      cum_extremist_sub_no_current_channel > 0 & channel_type == "extremist" ~ "subscribed to another",
      cum_extremist_sub_no_current_channel == 0 & channel_type == "extremist"~ "not subscribed"
    ),
    subscribed_mainstream = case_when(
      subscribed == 1 & channel_type == "mainstream" ~ "subscribed to current",
      cum_mainstream_sub_no_current_channel > 0 & channel_type == "mainstream" ~ "subscribed to another",
      cum_mainstream_sub_no_current_channel == 0 & channel_type == "mainstream"~ "not subscribed"
    ),
    subscribed_other = case_when(
      subscribed == 1 & channel_type == "other" ~ "subscribed to current",
      cum_other_sub_no_current_channel > 0 & channel_type == "other" ~ "subscribed to another",
      cum_other_sub_no_current_channel == 0 & channel_type == "other"~ "not subscribed"
    )) 


summarize_subscribe_fxn <- function(sub_group) {
  summary_youtube_subscriptions %>%
    group_by(user_id, .data[[sub_group]]) %>%
    summarise(wtd_n = sum(weight_cmd)) %>%
    ungroup()  %>%
    group_by(.data[[sub_group]]) %>%
    summarise(n = sum(wtd_n)) %>%
    ungroup() %>%
    filter(!is.na(.data[[sub_group]])) %>%
    mutate(total = sum(n),
           percent = 100*(n/total),
           channel_type = sub_group) %>%
    rename(subscribed_group = .data[[sub_group]]) %>%
    mutate(stderr = sqrt((percent*(100 - percent))/n),
           ci_lwr95 = percent - qt(.025, df = total, lower.tail = F)*stderr,
           ci_upr95 = percent + qt(.025, df = total, lower.tail = F)*stderr,
           ci_lwr90 = percent - qt(.05, df = total, lower.tail = F)*stderr,
           ci_upr90 = percent + qt(.05, df = total, lower.tail = F)*stderr,
           total_label = scales::comma(n))
}

prop_table <- merged_cumulative_all %>% 
  group_by(user_id, channel_type) %>%
  summarise(wtd_n = sum(weight_cmd)) %>% 
  group_by(channel_type) %>%
  summarise(n = sum(wtd_n)) %>% 
  ungroup() %>%
  mutate(prop = round(n/sum(n), 3))


summarize_subscribe_table_wtd <- map_dfr(
  c('subscribed_alternative',
    'subscribed_extremist',
    'subscribed_mainstream',
    'subscribed_other'), 
  ~summarize_subscribe_fxn(sub_group = .x)) %>% 
  mutate(channel_type = case_when(channel_type == "subscribed_alternative" ~ paste0("Alternative channel\n(",prop_table[prop_table$channel_type == "alternative",]$prop*100,"%)"),
                                  channel_type == "subscribed_extremist" ~  paste0("Extremist channel\n(",prop_table[prop_table$channel_type == "extremist",]$prop*100,"%)"),
                                  channel_type == "subscribed_mainstream" ~  paste0("Mainstream media\n(",prop_table[prop_table$channel_type == "mainstream",]$prop*100,"%)"),
                                  channel_type == "subscribed_other" ~  paste0("Other channel\n(",prop_table[prop_table$channel_type == "other",]$prop*100,"%)")),
         subscribed_group = factor(subscribed_group, 
                                   levels = c("subscribed to current",
                                              "subscribed to another",
                                              "not subscribed")))


write_csv(summarize_subscribe_table_wtd, "./data/summarize_subscribe_table_wtd.csv")


