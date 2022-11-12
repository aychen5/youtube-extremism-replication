usethis::use_blank_slate()
pacman::p_load(tidyverse, haven, sjlabelled, sjmisc)

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
      #educ %in% "No HS" ~ "No high school",
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
# CCES %>%
#   select(DART0034_caseid, contains("_422"))

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
    # zscore or 0 to 1 normalization?
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


