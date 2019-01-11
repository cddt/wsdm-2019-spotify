rm(list = ls())
gc()

library('keras')
library('data.table')
library('tidyverse')

train_dir = '/home/cddt/data-space/spotify-challenge/data/training_set/'
test_dir = '/home/cddt/data-space/spotify-challenge/data/test_set/'
sub_dir = '/home/cddt/data-space/spotify-challenge/data/submissions/sub27/'
model_dir = '/home/cddt/data-space/spotify-challenge/data/models/models27/'

filelist = list.files(path = train_dir, pattern = 'log_*', all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
filelist_models = list.files(path = model_dir, pattern = 'log_*', all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

track = fread(file = '/home/cddt/data-space/spotify-challenge/data/data_augmentation/track_train1')
track_file_list = c('/home/cddt/data-space/spotify-challenge/data/track_features/tf_000000000000.csv','/home/cddt/data-space/spotify-challenge/data/track_features/tf_000000000001.csv')
l <- lapply(track_file_list, fread)
track2 <- rbindlist(l)
rm(l)
gc()
track2[, c('minor','major') := data.table(to_categorical(track2[, ifelse(mode == 'major', TRUE, FALSE)]))]
track2[, mode := NULL]
track2[, paste0('key_',0:11) := data.table(to_categorical(track2[, key]))]
track2[, key := NULL]
track2[, release_year := (release_year - min(track2[,release_year])) / (max(track2[,release_year]) - min(track2[,release_year]))]
track2[, us_popularity_estimate := (us_popularity_estimate - min(track2[,us_popularity_estimate])) / (max(track2[,us_popularity_estimate]) - min(track2[,us_popularity_estimate]))]
track2[, dyn_range_mean := (dyn_range_mean - min(track2[,dyn_range_mean])) / (max(track2[,dyn_range_mean]) - min(track2[,dyn_range_mean]))]
track2[, flatness := (flatness - min(track2[,flatness])) / (max(track2[,flatness]) - min(track2[,flatness]))]
track2[, duration := (duration - min(track2[,duration])) / (max(track2[,duration]) - min(track2[,duration]))]
track2[, loudness := (loudness - min(track2[,loudness])) / (max(track2[,loudness]) - min(track2[,loudness]))]
track2[, tempo := (tempo - min(track2[,tempo])) / (max(track2[,tempo]) - min(track2[,tempo]))]
track2[, time_signature := (time_signature - min(track2[,time_signature])) / (max(track2[,time_signature]) - min(track2[,time_signature]))]

ave_pre = function(test_data){
  test_data[, test_position := session_position - (session_length - N)]
  test_data[, correct := skip_2 == ifelse(prediction >= 0.5, 1, 0)]
  setorder(test_data, session_id, session_position) 
  test_data[, precision := correct * cumsum(correct) / test_position, by = session_id]
  accuracy = test_data[, sum(precision) / first(N) , by = session_id]
  average_precision = sum(accuracy[, V1]) / length(accuracy[, V1])
  first_accuracy = sum(test_data[test_position == 1, precision]) / length(test_data[test_position == 1, precision])
  return(c(average_precision, first_accuracy))
}

prepare_file = function(file = filelist[1]) {
  l <- lapply(paste0(train_dir, file), fread, select = c('session_id','session_position','skip_1','skip_2','skip_3','session_length','context_switch','no_pause_before_play','short_pause_before_play','long_pause_before_play','hist_user_behavior_n_seekfwd','hist_user_behavior_n_seekback','hist_user_behavior_is_shuffle','hour_of_day','premium','date','context_type','hist_user_behavior_reason_start','hist_user_behavior_reason_end','track_id_clean'))
  train1 <- rbindlist(l)
  rm(l)
  
  train1[,'first_half'] = ifelse(with(train1, session_position / session_length) > 0.5, FALSE, TRUE)
  test1 = train1[train1$first_half == FALSE, c('session_id','session_position','session_length','skip_2','track_id_clean')]
  train1 = train1[train1$first_half == TRUE, ]
  gc()
  
  train1 = merge(x = train1, y = train1[, .N, by = session_id], by = 'session_id', all.x = TRUE)
  test1 = merge(x = test1, y = test1[, .N, by = session_id], by = 'session_id', all.x = TRUE)
  train1 = merge(x = train1, y = track2[, .(track_id, acoustic_vector_0, acoustic_vector_1, acoustic_vector_2, acoustic_vector_3, acoustic_vector_4 ,acoustic_vector_5 ,acoustic_vector_6, acoustic_vector_7, duration,release_year,us_popularity_estimate,acousticness,beat_strength,bounciness,danceability,dyn_range_mean,energy,flatness,instrumentalness,liveness,loudness,mechanism,organism,speechiness,tempo,time_signature,valence,minor, major, key_0, key_1, key_2, key_3, key_4, key_5, key_6, key_7, key_8, key_9, key_10, key_11)], by.x = 'track_id_clean', by.y = 'track_id', all.x = TRUE)
  test1 = merge(x = test1, y = track[, .(track_id, percent_total)], by.x = 'track_id_clean', by.y = 'track_id', all.x = TRUE)
  test1 = merge(x = test1, y = track2[, .(track_id, acoustic_vector_0, acoustic_vector_1, acoustic_vector_2, acoustic_vector_3, acoustic_vector_4 ,acoustic_vector_5 ,acoustic_vector_6, acoustic_vector_7, duration,release_year,us_popularity_estimate,acousticness,beat_strength,bounciness,danceability,dyn_range_mean,energy,flatness,instrumentalness,liveness,loudness,mechanism,organism,speechiness,tempo,time_signature,valence,minor, major, key_0, key_1, key_2, key_3, key_4, key_5, key_6, key_7, key_8, key_9, key_10, key_11)], by.x = 'track_id_clean', by.y = 'track_id', all.x = TRUE)
  #rm(track)
  #gc()
  
  train1[, train_position := (N - session_position + 1)]
  train1[, temp_1 := 1]
  test1[, test_position := session_position - (session_length - N)]
  
  #skip_2 = train1[, list(mean_skip_2 = mean(skip_2)), by = 'session_id']
  #train1 = merge(x = train1, y = skip_2, by = 'session_id', all.x = TRUE)
  #test1 = merge(x = test1, y = skip_2, by = 'session_id', all.x = TRUE)
  
  to.replace <- names(which(sapply(train1, is.logical)))
  for (var in to.replace) train1[, (var):= as.numeric(get(var))]
  to.replace <- names(which(sapply(test1, is.logical)))
  for (var in to.replace) test1[, (var):= as.numeric(get(var))]
  
  train1[, dayname:= weekdays(as.Date(train1[,date]))]
  
  context_type = dcast(train1, session_id + train_position ~ context_type, value.var = c('temp_1'))
  hist_user_behavior_reason_end = dcast(train1, session_id + train_position ~ hist_user_behavior_reason_end, value.var = c('temp_1'))
  hist_user_behavior_reason_start = dcast(train1, session_id + train_position ~ hist_user_behavior_reason_start, value.var = c('temp_1'))
  hour_of_day = dcast(train1, session_id + train_position ~ hour_of_day, value.var = c('temp_1'))
  dayname = dcast(train1, session_id + train_position ~ dayname, value.var = c('temp_1'))
  
  context_type_cols = fread(file = '/home/cddt/data-space/spotify-challenge/data/data_augmentation/context_type', col.names = 'context_type', header = FALSE)
  #context_type = data.table(cbind(session_id = rownames(with(train1, table(session_id, context_type))), 
  #                               (rbind(with(train1, table(session_id, context_type))))))
  Missing = setdiff(c('session_id',context_type_cols[, context_type]), names(context_type))  
  if (length(Missing) > 0) {
    context_type[, c(Missing) := NA]
  }
  to.replace <- names(context_type[, -c('session_id','train_position')])
  for (var in to.replace) context_type[, (var):= as.numeric(get(var))]
  setcolorder(context_type, order(names(context_type)))
  #train1 = merge(x = train1, y = context_type, by = 'session_id', all.x = TRUE)
  #test1 = merge(x = test1, y = context_type, by = 'session_id', all.x = TRUE)
  cols1 = colnames(context_type[, -c('session_id','train_position')])
  
  hist_user_behavior_reason_end_cols = fread(file = '/home/cddt/data-space/spotify-challenge/data/data_augmentation/hist_user_behavior_reason_end', col.names = 'hist_user_behavior_reason_end', header = FALSE)
  #hist_user_behavior_reason_end = data.table(cbind(session_id = rownames(with(train1, table(session_id, hist_user_behavior_reason_end))), 
  #                                                (rbind(with(train1, table(session_id, hist_user_behavior_reason_end))))))
  Missing = setdiff(c('session_id',hist_user_behavior_reason_end_cols[, hist_user_behavior_reason_end]), names(hist_user_behavior_reason_end))  
  if (length(Missing) > 0) {
    hist_user_behavior_reason_end[, c(Missing) := NA]
  }
  to.replace <- names(hist_user_behavior_reason_end[, -c('session_id','train_position')])
  for (var in to.replace) hist_user_behavior_reason_end[, (var):= as.numeric(get(var))]
  colnames(hist_user_behavior_reason_end)[3:dim(hist_user_behavior_reason_end)[2]] = paste0(colnames(hist_user_behavior_reason_end)[3:dim(hist_user_behavior_reason_end)[2]],'_end')
  setcolorder(hist_user_behavior_reason_end, order(names(hist_user_behavior_reason_end)))
  #train1 = merge(x = train1, y = hist_user_behavior_reason_end, by = 'session_id', all.x = TRUE)
  #test1 = merge(x = test1, y = hist_user_behavior_reason_end, by = 'session_id', all.x = TRUE)
  cols2 = colnames(hist_user_behavior_reason_end[, -c('session_id','train_position')])
  
  hist_user_behavior_reason_start_cols = fread(file = '/home/cddt/data-space/spotify-challenge/data/data_augmentation/hist_user_behavior_reason_start', col.names = 'hist_user_behavior_reason_start', header = FALSE)
  #hist_user_behavior_reason_start = data.table(cbind(session_id = rownames(with(train1, table(session_id, hist_user_behavior_reason_start))), 
  #                                                  (rbind(with(train1, table(session_id, hist_user_behavior_reason_start))))))
  Missing = setdiff(c('session_id',hist_user_behavior_reason_start_cols[, hist_user_behavior_reason_start]), names(hist_user_behavior_reason_start))  
  if (length(Missing) > 0) {
    hist_user_behavior_reason_start[, c(Missing) := NA]
  }
  to.replace <- names(hist_user_behavior_reason_start[, -c('session_id','train_position')])
  for (var in to.replace) hist_user_behavior_reason_start[, (var):= as.numeric(get(var))]
  colnames(hist_user_behavior_reason_start)[3:dim(hist_user_behavior_reason_start)[2]] = paste0(colnames(hist_user_behavior_reason_start)[3:dim(hist_user_behavior_reason_start)[2]],'_start')
  setcolorder(hist_user_behavior_reason_start, order(names(hist_user_behavior_reason_start)))
  #train1 = merge(x = train1, y = hist_user_behavior_reason_start, by = 'session_id', all.x = TRUE)
  #test1 = merge(x = test1, y = hist_user_behavior_reason_start, by = 'session_id', all.x = TRUE)
  cols3 = colnames(hist_user_behavior_reason_start[, -c('session_id','train_position')])
  
  hour_of_day_cols = as.character(0:23)
  Missing = setdiff(c('session_id',hour_of_day_cols), names(hour_of_day))  
  if (length(Missing) > 0) {
    hour_of_day[, c(Missing) := NA]
  }
  to.replace <- names(hour_of_day[, -c('session_id','train_position')])
  for (var in to.replace) hour_of_day[, (var):= as.numeric(get(var))]
  setcolorder(hour_of_day, order(names(hour_of_day)))
  setnames(hour_of_day, old = as.character(0:23), new = paste0('hour_', as.character(0:23)))
  cols4 = colnames(hour_of_day[, -c('session_id','train_position')])
  
  dayname_cols = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
  Missing = setdiff(c('session_id', dayname_cols), names(dayname))  
  if (length(Missing) > 0) {
    dayname[, c(Missing) := NA]
  }
  to.replace <- names(dayname[, -c('session_id','train_position')])
  for (var in to.replace) dayname[, (var):= as.numeric(get(var))]
  setcolorder(dayname, order(names(dayname)))
  cols5 = colnames(dayname[, -c('session_id','train_position')])
  
  context_type = dcast(context_type, session_id ~ train_position, value.var = c(cols1))
  hist_user_behavior_reason_end = dcast(hist_user_behavior_reason_end, session_id ~ train_position, value.var = c(cols2))
  hist_user_behavior_reason_start = dcast(hist_user_behavior_reason_start, session_id ~ train_position, value.var = c(cols3))
  hour_of_day = dcast(hour_of_day, session_id ~ train_position, value.var = c(cols4))
  dayname = dcast(dayname, session_id ~ train_position, value.var = c(cols5))
  
  train1 = dcast(train1, session_id + session_length ~ train_position, value.var = c("skip_2","skip_1","skip_3",'context_switch','no_pause_before_play','short_pause_before_play','long_pause_before_play','hist_user_behavior_n_seekfwd','hist_user_behavior_n_seekback','hist_user_behavior_is_shuffle','premium','acoustic_vector_0','acoustic_vector_1','acoustic_vector_2','acoustic_vector_3','acoustic_vector_4','acoustic_vector_5','acoustic_vector_6','acoustic_vector_7','duration','beat_strength','tempo','release_year','us_popularity_estimate','acousticness','bounciness','danceability','dyn_range_mean','energy','flatness','instrumentalness','liveness','loudness','mechanism','organism','speechiness','time_signature','valence'))
  train1 = merge(x = train1, y = context_type, by = 'session_id', all.x = TRUE)
  train1 = merge(x = train1, y = hist_user_behavior_reason_end, by = 'session_id', all.x = TRUE)
  train1 = merge(x = train1, y = hist_user_behavior_reason_start, by = 'session_id', all.x = TRUE)
  train1 = merge(x = train1, y = hour_of_day, by = 'session_id', all.x = TRUE)
  train1 = merge(x = train1, y = dayname, by = 'session_id', all.x = TRUE)
  #colnames(train1[, -c('session_id','session_length')])
  
  rm(list = c('context_type','hist_user_behavior_reason_end','hist_user_behavior_reason_start','hour_of_day','dayname'))
  gc()
  
  test1_test = dcast(test1, session_id ~ test_position, value.var = c('skip_2','percent_total','acoustic_vector_0','acoustic_vector_1','acoustic_vector_2','acoustic_vector_3','acoustic_vector_4','acoustic_vector_5','acoustic_vector_6','acoustic_vector_7','duration','beat_strength','tempo','release_year','us_popularity_estimate','acousticness','bounciness','danceability','dyn_range_mean','energy','flatness','instrumentalness','liveness','loudness','mechanism','organism','speechiness','time_signature','valence'))
  colnames(test1_test)[2:11] = paste0('skip_target_',1:10)
  colnames(test1_test)[22:31] = paste0('acoustic_vector_0_target_',1:10)
  colnames(test1_test)[32:41] = paste0('acoustic_vector_1_target_',1:10)
  colnames(test1_test)[42:51] = paste0('acoustic_vector_2_target_',1:10)
  colnames(test1_test)[52:61] = paste0('acoustic_vector_3_target_',1:10)
  colnames(test1_test)[62:71] = paste0('acoustic_vector_4_target_',1:10)
  colnames(test1_test)[72:81] = paste0('acoustic_vector_5_target_',1:10)
  colnames(test1_test)[82:91] = paste0('acoustic_vector_6_target_',1:10)
  colnames(test1_test)[92:101] = paste0('acoustic_vector_7_target_',1:10)
  colnames(test1_test)[102:111] = paste0('duration_target_',1:10)
  colnames(test1_test)[112:121] = paste0('beat_strength_target_',1:10)
  colnames(test1_test)[122:131] = paste0('tempo_target_',1:10)
  colnames(test1_test)[132:141] = paste0('release_year_target_',1:10)
  colnames(test1_test)[142:151] = paste0('us_popularity_estimate_target_',1:10)
  colnames(test1_test)[152:161] = paste0('acousticness_target_',1:10)
  colnames(test1_test)[162:171] = paste0('bounciness_target_',1:10)
  colnames(test1_test)[172:181] = paste0('danceability_target_',1:10)
  colnames(test1_test)[182:191] = paste0('dyn_range_mean_target_',1:10)
  colnames(test1_test)[192:201] = paste0('energy_target_',1:10)
  colnames(test1_test)[202:211] = paste0('flatness_target_',1:10)
  colnames(test1_test)[212:221] = paste0('instrumentalness_target_',1:10)
  colnames(test1_test)[222:231] = paste0('liveness_target_',1:10)
  colnames(test1_test)[232:241] = paste0('loudness_target_',1:10)
  colnames(test1_test)[242:251] = paste0('mechanism_target_',1:10)
  colnames(test1_test)[252:261] = paste0('organism_target_',1:10)
  colnames(test1_test)[262:271] = paste0('speechiness_target_',1:10)
  colnames(test1_test)[272:281] = paste0('time_signature_target_',1:10)
  colnames(test1_test)[282:291] = paste0('valence_target_',1:10)
  
  train1 = merge(x = train1, y = test1_test, by = 'session_id', all.x = TRUE)
  
  l <- list(train1, test1, test1_test)
  return(l)
}

files = 23 
#files = c(85, 295, 476)
#files = sample(x = c(1:650)[-96], size = 1, replace = FALSE)
prepared = prepare_file(file = filelist[files])
train1 = prepared[[1]]
test1 = prepared[[2]]
test1_test = prepared[[3]]
rm(prepared)
gc()
prepared2 = prepare_file(filelist[96])
train2 = prepared2[[1]]
test2 = prepared2[[2]]
test2_test = prepared2[[3]]
rm(prepared2)
rm(list = c('track','track2'))
gc()

train_length = dim(train1)[1]
test_length = dim(train2)[1]

x_train <- train1[, .SD, .SDcols=c('skip_2_10','skip_2_9','skip_2_8','skip_2_7','skip_2_6','skip_2_5','skip_2_4','skip_2_3','skip_2_2','skip_2_1',
                                   'trackdone_end_10','trackdone_end_9','trackdone_end_8','trackdone_end_7','trackdone_end_6','trackdone_end_5','trackdone_end_4','trackdone_end_3','trackdone_end_2','trackdone_end_1',
                                   'skip_1_10','skip_1_9','skip_1_8','skip_1_7','skip_1_6','skip_1_5','skip_1_4','skip_1_3','skip_1_2','skip_1_1',
                                   'skip_3_10','skip_3_9','skip_3_8','skip_3_7','skip_3_6','skip_3_5','skip_3_4','skip_3_3','skip_3_2','skip_3_1',
                                   'context_switch_10','context_switch_9','context_switch_8','context_switch_7','context_switch_6','context_switch_5','context_switch_4','context_switch_3','context_switch_2','context_switch_1',
                                   'no_pause_before_play_10','no_pause_before_play_9','no_pause_before_play_8','no_pause_before_play_7','no_pause_before_play_6','no_pause_before_play_5','no_pause_before_play_4','no_pause_before_play_3','no_pause_before_play_2','no_pause_before_play_1',
                                   'short_pause_before_play_10','short_pause_before_play_9','short_pause_before_play_8','short_pause_before_play_7','short_pause_before_play_6','short_pause_before_play_5','short_pause_before_play_4','short_pause_before_play_3','short_pause_before_play_2','short_pause_before_play_1',
                                   'long_pause_before_play_10','long_pause_before_play_9','long_pause_before_play_8','long_pause_before_play_7','long_pause_before_play_6','long_pause_before_play_5','long_pause_before_play_4','long_pause_before_play_3','long_pause_before_play_2','long_pause_before_play_1',
                                   'hist_user_behavior_n_seekfwd_10','hist_user_behavior_n_seekfwd_9','hist_user_behavior_n_seekfwd_8','hist_user_behavior_n_seekfwd_7','hist_user_behavior_n_seekfwd_6','hist_user_behavior_n_seekfwd_5','hist_user_behavior_n_seekfwd_4','hist_user_behavior_n_seekfwd_3','hist_user_behavior_n_seekfwd_2','hist_user_behavior_n_seekfwd_1',
                                   'hist_user_behavior_n_seekback_10','hist_user_behavior_n_seekback_9','hist_user_behavior_n_seekback_8','hist_user_behavior_n_seekback_7','hist_user_behavior_n_seekback_6','hist_user_behavior_n_seekback_5','hist_user_behavior_n_seekback_4','hist_user_behavior_n_seekback_3','hist_user_behavior_n_seekback_2','hist_user_behavior_n_seekback_1',
                                   'hist_user_behavior_is_shuffle_10','hist_user_behavior_is_shuffle_9','hist_user_behavior_is_shuffle_8','hist_user_behavior_is_shuffle_7','hist_user_behavior_is_shuffle_6','hist_user_behavior_is_shuffle_5','hist_user_behavior_is_shuffle_4','hist_user_behavior_is_shuffle_3','hist_user_behavior_is_shuffle_2','hist_user_behavior_is_shuffle_1',
                                   'editorial_playlist_10','editorial_playlist_9','editorial_playlist_8','editorial_playlist_7','editorial_playlist_6','editorial_playlist_5','editorial_playlist_4','editorial_playlist_3','editorial_playlist_2','editorial_playlist_1',
                                   'user_collection_10','user_collection_9','user_collection_8','user_collection_7','user_collection_6','user_collection_5','user_collection_4','user_collection_3','user_collection_2','user_collection_1',
                                   'radio_10','radio_9','radio_8','radio_7','radio_6','radio_5','radio_4','radio_3','radio_2','radio_1',
                                   'personalized_playlist_10','personalized_playlist_9','personalized_playlist_8','personalized_playlist_7','personalized_playlist_6','personalized_playlist_5','personalized_playlist_4','personalized_playlist_3','personalized_playlist_2','personalized_playlist_1',
                                   'catalog_10','catalog_9','catalog_8','catalog_7','catalog_6','catalog_5','catalog_4','catalog_3','catalog_2','catalog_1',
                                   'charts_10','charts_9','charts_8','charts_7','charts_6','charts_5','charts_4','charts_3','charts_2','charts_1',
                                   'fwdbtn_end_10','fwdbtn_end_9','fwdbtn_end_8','fwdbtn_end_7','fwdbtn_end_6','fwdbtn_end_5','fwdbtn_end_4','fwdbtn_end_3','fwdbtn_end_2','fwdbtn_end_1',
                                   'backbtn_end_10','backbtn_end_9','backbtn_end_8','backbtn_end_7','backbtn_end_6','backbtn_end_5','backbtn_end_4','backbtn_end_3','backbtn_end_2','backbtn_end_1',
                                   'endplay_end_10','endplay_end_9','endplay_end_8','endplay_end_7','endplay_end_6','endplay_end_5','endplay_end_4','endplay_end_3','endplay_end_2','endplay_end_1',
                                   'logout_end_10','logout_end_9','logout_end_8','logout_end_7','logout_end_6','logout_end_5','logout_end_4','logout_end_3','logout_end_2','logout_end_1',
                                   'clickrow_end_10','clickrow_end_9','clickrow_end_8','clickrow_end_7','clickrow_end_6','clickrow_end_5','clickrow_end_4','clickrow_end_3','clickrow_end_2','clickrow_end_1',
                                   'appload_end_10','appload_end_9','appload_end_8','appload_end_7','appload_end_6','appload_end_5','appload_end_4','appload_end_3','appload_end_2','appload_end_1',
                                   'uriopen_end_10','uriopen_end_9','uriopen_end_8','uriopen_end_7','uriopen_end_6','uriopen_end_5','uriopen_end_4','uriopen_end_3','uriopen_end_2','uriopen_end_1',
                                   'clickside_end_10','clickside_end_9','clickside_end_8','clickside_end_7','clickside_end_6','clickside_end_5','clickside_end_4','clickside_end_3','clickside_end_2','clickside_end_1',
                                   'popup_end_10','popup_end_9','popup_end_8','popup_end_7','popup_end_6','popup_end_5','popup_end_4','popup_end_3','popup_end_2','popup_end_1',
                                   'remote_end_10','remote_end_9','remote_end_8','remote_end_7','remote_end_6','remote_end_5','remote_end_4','remote_end_3','remote_end_2','remote_end_1',
                                   'trackdone_start_10','trackdone_start_9','trackdone_start_8','trackdone_start_7','trackdone_start_6','trackdone_start_5','trackdone_start_4','trackdone_start_3','trackdone_start_2','trackdone_start_1',
                                   'fwdbtn_start_10','fwdbtn_start_9','fwdbtn_start_8','fwdbtn_start_7','fwdbtn_start_6','fwdbtn_start_5','fwdbtn_start_4','fwdbtn_start_3','fwdbtn_start_2','fwdbtn_start_1',
                                   'backbtn_start_10','backbtn_start_9','backbtn_start_8','backbtn_start_7','backbtn_start_6','backbtn_start_5','backbtn_start_4','backbtn_start_3','backbtn_start_2','backbtn_start_1',
                                   'clickrow_start_10','clickrow_start_9','clickrow_start_8','clickrow_start_7','clickrow_start_6','clickrow_start_5','clickrow_start_4','clickrow_start_3','clickrow_start_2','clickrow_start_1',
                                   'appload_start_10','appload_start_9','appload_start_8','appload_start_7','appload_start_6','appload_start_5','appload_start_4','appload_start_3','appload_start_2','appload_start_1',
                                   'playbtn_start_10','playbtn_start_9','playbtn_start_8','playbtn_start_7','playbtn_start_6','playbtn_start_5','playbtn_start_4','playbtn_start_3','playbtn_start_2','playbtn_start_1',
                                   'remote_start_10','remote_start_9','remote_start_8','remote_start_7','remote_start_6','remote_start_5','remote_start_4','remote_start_3','remote_start_2','remote_start_1',
                                   'trackerror_start_10','trackerror_start_9','trackerror_start_8','trackerror_start_7','trackerror_start_6','trackerror_start_5','trackerror_start_4','trackerror_start_3','trackerror_start_2','trackerror_start_1',
                                   'endplay_start_10','endplay_start_9','endplay_start_8','endplay_start_7','endplay_start_6','endplay_start_5','endplay_start_4','endplay_start_3','endplay_start_2','endplay_start_1',
                                   'clickside_start_10','clickside_start_9','clickside_start_8','clickside_start_7','clickside_start_6','clickside_start_5','clickside_start_4','clickside_start_3','clickside_start_2','clickside_start_1',
                                   'uriopen_start_10','uriopen_start_9','uriopen_start_8','uriopen_start_7','uriopen_start_6','uriopen_start_5','uriopen_start_4','uriopen_start_3','uriopen_start_2','uriopen_start_1',
                                   'popup_start_10','popup_start_9','popup_start_8','popup_start_7','popup_start_6','popup_start_5','popup_start_4','popup_start_3','popup_start_2','popup_start_1',
                                   'percent_total_10','percent_total_9','percent_total_8','percent_total_7','percent_total_6','percent_total_5','percent_total_4','percent_total_3','percent_total_2','percent_total_1',
                                   #'Monday_10','Monday_9','Monday_8','Monday_7','Monday_6','Monday_5','Monday_4','Monday_3','Monday_2','Monday_1',
                                   #'Tuesday_10','Tuesday_9','Tuesday_8','Tuesday_7','Tuesday_6','Tuesday_5','Tuesday_4','Tuesday_3','Tuesday_2','Tuesday_1',
                                   #'Wednesday_10','Wednesday_9','Wednesday_8','Wednesday_7','Wednesday_6','Wednesday_5','Wednesday_4','Wednesday_3','Wednesday_2','Wednesday_1',
                                   #'Thursday_10','Thursday_9','Thursday_8','Thursday_7','Thursday_6','Thursday_5','Thursday_4','Thursday_3','Thursday_2','Thursday_1',
                                   #'Friday_10','Friday_9','Friday_8','Friday_7','Friday_6','Friday_5','Friday_4','Friday_3','Friday_2','Friday_1',
                                   #'Saturday_10','Saturday_9','Saturday_8','Saturday_7','Saturday_6','Saturday_5','Saturday_4','Saturday_3','Saturday_2','Saturday_1',
                                   #'Sunday_10','Sunday_9','Sunday_8','Sunday_7','Sunday_6','Sunday_5','Sunday_4','Sunday_3','Sunday_2','Sunday_1',
                                   'hour_0_10','hour_0_9','hour_0_8','hour_0_7','hour_0_6','hour_0_5','hour_0_4','hour_0_3','hour_0_2','hour_0_1',
                                   'hour_1_10','hour_1_9','hour_1_8','hour_1_7','hour_1_6','hour_1_5','hour_1_4','hour_1_3','hour_1_2','hour_1_1',
                                   'hour_2_10','hour_2_9','hour_2_8','hour_2_7','hour_2_6','hour_2_5','hour_2_4','hour_2_3','hour_2_2','hour_2_1',
                                   'hour_3_10','hour_3_9','hour_3_8','hour_3_7','hour_3_6','hour_3_5','hour_3_4','hour_3_3','hour_3_2','hour_3_1',
                                   'hour_4_10','hour_4_9','hour_4_8','hour_4_7','hour_4_6','hour_4_5','hour_4_4','hour_4_3','hour_4_2','hour_4_1',
                                   'hour_5_10','hour_5_9','hour_5_8','hour_5_7','hour_5_6','hour_5_5','hour_5_4','hour_5_3','hour_5_2','hour_5_1',
                                   'hour_6_10','hour_6_9','hour_6_8','hour_6_7','hour_6_6','hour_6_5','hour_6_4','hour_6_3','hour_6_2','hour_6_1',
                                   'hour_7_10','hour_7_9','hour_7_8','hour_7_7','hour_7_6','hour_7_5','hour_7_4','hour_7_3','hour_7_2','hour_7_1',
                                   'hour_8_10','hour_8_9','hour_8_8','hour_8_7','hour_8_6','hour_8_5','hour_8_4','hour_8_3','hour_8_2','hour_8_1',
                                   'hour_9_10','hour_9_9','hour_9_8','hour_9_7','hour_9_6','hour_9_5','hour_9_4','hour_9_3','hour_9_2','hour_9_1',
                                   'hour_10_10','hour_10_9','hour_10_8','hour_10_7','hour_10_6','hour_10_5','hour_10_4','hour_10_3','hour_10_2','hour_10_1',
                                   'hour_11_10','hour_11_9','hour_11_8','hour_11_7','hour_11_6','hour_11_5','hour_11_4','hour_11_3','hour_11_2','hour_11_1',
                                   'hour_12_10','hour_12_9','hour_12_8','hour_12_7','hour_12_6','hour_12_5','hour_12_4','hour_12_3','hour_12_2','hour_12_1',
                                   'hour_13_10','hour_13_9','hour_13_8','hour_13_7','hour_13_6','hour_13_5','hour_13_4','hour_13_3','hour_13_2','hour_13_1',
                                   'hour_14_10','hour_14_9','hour_14_8','hour_14_7','hour_14_6','hour_14_5','hour_14_4','hour_14_3','hour_14_2','hour_14_1',
                                   'hour_15_10','hour_15_9','hour_15_8','hour_15_7','hour_15_6','hour_15_5','hour_15_4','hour_15_3','hour_15_2','hour_15_1',
                                   'hour_16_10','hour_16_9','hour_16_8','hour_16_7','hour_16_6','hour_16_5','hour_16_4','hour_16_3','hour_16_2','hour_16_1',
                                   'hour_17_10','hour_17_9','hour_17_8','hour_17_7','hour_17_6','hour_17_5','hour_17_4','hour_17_3','hour_17_2','hour_17_1',
                                   'hour_18_10','hour_18_9','hour_18_8','hour_18_7','hour_18_6','hour_18_5','hour_18_4','hour_18_3','hour_18_2','hour_18_1',
                                   'hour_19_10','hour_19_9','hour_19_8','hour_19_7','hour_19_6','hour_19_5','hour_19_4','hour_19_3','hour_19_2','hour_19_1',
                                   'hour_20_10','hour_20_9','hour_20_8','hour_20_7','hour_20_6','hour_20_5','hour_20_4','hour_20_3','hour_20_2','hour_20_1',
                                   'hour_21_10','hour_21_9','hour_21_8','hour_21_7','hour_21_6','hour_21_5','hour_21_4','hour_21_3','hour_21_2','hour_21_1',
                                   'hour_22_10','hour_22_9','hour_22_8','hour_22_7','hour_22_6','hour_22_5','hour_22_4','hour_22_3','hour_22_2','hour_22_1',
                                   'hour_23_10','hour_23_9','hour_23_8','hour_23_7','hour_23_6','hour_23_5','hour_23_4','hour_23_3','hour_23_2','hour_23_1',
                                   'premium_10','premium_9','premium_8','premium_7','premium_6','premium_5','premium_4','premium_3','premium_2','premium_1',
                                   'acoustic_vector_0_10','acoustic_vector_0_9','acoustic_vector_0_8','acoustic_vector_0_7','acoustic_vector_0_6','acoustic_vector_0_5','acoustic_vector_0_4','acoustic_vector_0_3','acoustic_vector_0_2','acoustic_vector_0_1',
                                   'acoustic_vector_1_10','acoustic_vector_1_9','acoustic_vector_1_8','acoustic_vector_1_7','acoustic_vector_1_6','acoustic_vector_1_5','acoustic_vector_1_4','acoustic_vector_1_3','acoustic_vector_1_2','acoustic_vector_1_1',
                                   'acoustic_vector_2_10','acoustic_vector_2_9','acoustic_vector_2_8','acoustic_vector_2_7','acoustic_vector_2_6','acoustic_vector_2_5','acoustic_vector_2_4','acoustic_vector_2_3','acoustic_vector_2_2','acoustic_vector_2_1',
                                   'acoustic_vector_3_10','acoustic_vector_3_9','acoustic_vector_3_8','acoustic_vector_3_7','acoustic_vector_3_6','acoustic_vector_3_5','acoustic_vector_3_4','acoustic_vector_3_3','acoustic_vector_3_2','acoustic_vector_3_1',
                                   'acoustic_vector_4_10','acoustic_vector_4_9','acoustic_vector_4_8','acoustic_vector_4_7','acoustic_vector_4_6','acoustic_vector_4_5','acoustic_vector_4_4','acoustic_vector_4_3','acoustic_vector_4_2','acoustic_vector_4_1',
                                   'acoustic_vector_5_10','acoustic_vector_5_9','acoustic_vector_5_8','acoustic_vector_5_7','acoustic_vector_5_6','acoustic_vector_5_5','acoustic_vector_5_4','acoustic_vector_5_3','acoustic_vector_5_2','acoustic_vector_5_1',
                                   'acoustic_vector_6_10','acoustic_vector_6_9','acoustic_vector_6_8','acoustic_vector_6_7','acoustic_vector_6_6','acoustic_vector_6_5','acoustic_vector_6_4','acoustic_vector_6_3','acoustic_vector_6_2','acoustic_vector_6_1',
                                   'acoustic_vector_7_10','acoustic_vector_7_9','acoustic_vector_7_8','acoustic_vector_7_7','acoustic_vector_7_6','acoustic_vector_7_5','acoustic_vector_7_4','acoustic_vector_7_3','acoustic_vector_7_2','acoustic_vector_7_1',
                                   'acoustic_vector_0_target_10','acoustic_vector_0_target_9','acoustic_vector_0_target_8','acoustic_vector_0_target_7','acoustic_vector_0_target_6','acoustic_vector_0_target_5','acoustic_vector_0_target_4','acoustic_vector_0_target_3','acoustic_vector_0_target_2','acoustic_vector_0_target_1',
                                   'acoustic_vector_1_target_10','acoustic_vector_1_target_9','acoustic_vector_1_target_8','acoustic_vector_1_target_7','acoustic_vector_1_target_6','acoustic_vector_1_target_5','acoustic_vector_1_target_4','acoustic_vector_1_target_3','acoustic_vector_1_target_2','acoustic_vector_1_target_1',
                                   'acoustic_vector_2_target_10','acoustic_vector_2_target_9','acoustic_vector_2_target_8','acoustic_vector_2_target_7','acoustic_vector_2_target_6','acoustic_vector_2_target_5','acoustic_vector_2_target_4','acoustic_vector_2_target_3','acoustic_vector_2_target_2','acoustic_vector_2_target_1',
                                   'acoustic_vector_3_target_10','acoustic_vector_3_target_9','acoustic_vector_3_target_8','acoustic_vector_3_target_7','acoustic_vector_3_target_6','acoustic_vector_3_target_5','acoustic_vector_3_target_4','acoustic_vector_3_target_3','acoustic_vector_3_target_2','acoustic_vector_3_target_1',
                                   'acoustic_vector_4_target_10','acoustic_vector_4_target_9','acoustic_vector_4_target_8','acoustic_vector_4_target_7','acoustic_vector_4_target_6','acoustic_vector_4_target_5','acoustic_vector_4_target_4','acoustic_vector_4_target_3','acoustic_vector_4_target_2','acoustic_vector_4_target_1',
                                   'acoustic_vector_5_target_10','acoustic_vector_5_target_9','acoustic_vector_5_target_8','acoustic_vector_5_target_7','acoustic_vector_5_target_6','acoustic_vector_5_target_5','acoustic_vector_5_target_4','acoustic_vector_5_target_3','acoustic_vector_5_target_2','acoustic_vector_5_target_1',
                                   'acoustic_vector_6_target_10','acoustic_vector_6_target_9','acoustic_vector_6_target_8','acoustic_vector_6_target_7','acoustic_vector_6_target_6','acoustic_vector_6_target_5','acoustic_vector_6_target_4','acoustic_vector_6_target_3','acoustic_vector_6_target_2','acoustic_vector_6_target_1',
                                   'acoustic_vector_7_target_10','acoustic_vector_7_target_9','acoustic_vector_7_target_8','acoustic_vector_7_target_7','acoustic_vector_7_target_6','acoustic_vector_7_target_5','acoustic_vector_7_target_4','acoustic_vector_7_target_3','acoustic_vector_7_target_2','acoustic_vector_7_target_1',
                                   'duration_10','duration_9','duration_8','duration_7','duration_6','duration_5','duration_4','duration_3','duration_2','duration_1',
                                   'duration_target_10','duration_target_9','duration_target_8','duration_target_7','duration_target_6','duration_target_5','duration_target_4','duration_target_3','duration_target_2','duration_target_1',
                                   'beat_strength_target_10','beat_strength_target_9','beat_strength_target_8','beat_strength_target_7','beat_strength_target_6','beat_strength_target_5','beat_strength_target_4','beat_strength_target_3','beat_strength_target_2','beat_strength_target_1',
                                   'beat_strength_10','beat_strength_9','beat_strength_8','beat_strength_7','beat_strength_6','beat_strength_5','beat_strength_4','beat_strength_3','beat_strength_2','beat_strength_1',
                                   'tempo_10','tempo_9','tempo_8','tempo_7','tempo_6','tempo_5','tempo_4','tempo_3','tempo_2','tempo_1',
                                   'tempo_target_10','tempo_target_9','tempo_target_8','tempo_target_7','tempo_target_6','tempo_target_5','tempo_target_4','tempo_target_3','tempo_target_2','tempo_target_1',
                                   'release_year_10','release_year_9','release_year_8','release_year_7','release_year_6','release_year_5','release_year_4','release_year_3','release_year_2','release_year_1',
                                   'release_year_target_10','release_year_target_9','release_year_target_8','release_year_target_7','release_year_target_6','release_year_target_5','release_year_target_4','release_year_target_3','release_year_target_2','release_year_target_1',
                                   'us_popularity_estimate_10','us_popularity_estimate_9','us_popularity_estimate_8','us_popularity_estimate_7','us_popularity_estimate_6','us_popularity_estimate_5','us_popularity_estimate_4','us_popularity_estimate_3','us_popularity_estimate_2','us_popularity_estimate_1',
                                   'us_popularity_estimate_target_10','us_popularity_estimate_target_9','us_popularity_estimate_target_8','us_popularity_estimate_target_7','us_popularity_estimate_target_6','us_popularity_estimate_target_5','us_popularity_estimate_target_4','us_popularity_estimate_target_3','us_popularity_estimate_target_2','us_popularity_estimate_target_1',
                                   'acousticness_10','acousticness_9','acousticness_8','acousticness_7','acousticness_6','acousticness_5','acousticness_4','acousticness_3','acousticness_2','acousticness_1',
                                   'bounciness_10','bounciness_9','bounciness_8','bounciness_7','bounciness_6','bounciness_5','bounciness_4','bounciness_3','bounciness_2','bounciness_1',
                                   'danceability_10','danceability_9','danceability_8','danceability_7','danceability_6','danceability_5','danceability_4','danceability_3','danceability_2','danceability_1',
                                   'dyn_range_mean_10','dyn_range_mean_9','dyn_range_mean_8','dyn_range_mean_7','dyn_range_mean_6','dyn_range_mean_5','dyn_range_mean_4','dyn_range_mean_3','dyn_range_mean_2','dyn_range_mean_1',
                                   'energy_10','energy_9','energy_8','energy_7','energy_6','energy_5','energy_4','energy_3','energy_2','energy_1',
                                   'flatness_10','flatness_9','flatness_8','flatness_7','flatness_6','flatness_5','flatness_4','flatness_3','flatness_2','flatness_1',
                                   'instrumentalness_10','instrumentalness_9','instrumentalness_8','instrumentalness_7','instrumentalness_6','instrumentalness_5','instrumentalness_4','instrumentalness_3','instrumentalness_2','instrumentalness_1',
                                   'liveness_10','liveness_9','liveness_8','liveness_7','liveness_6','liveness_5','liveness_4','liveness_3','liveness_2','liveness_1',
                                   'loudness_10','loudness_9','loudness_8','loudness_7','loudness_6','loudness_5','loudness_4','loudness_3','loudness_2','loudness_1',
                                   'mechanism_10','mechanism_9','mechanism_8','mechanism_7','mechanism_6','mechanism_5','mechanism_4','mechanism_3','mechanism_2','mechanism_1',
                                   'organism_10','organism_9','organism_8','organism_7','organism_6','organism_5','organism_4','organism_3','organism_2','organism_1',
                                   'speechiness_10','speechiness_9','speechiness_8','speechiness_7','speechiness_6','speechiness_5','speechiness_4','speechiness_3','speechiness_2','speechiness_1',
                                   'time_signature_10','time_signature_9','time_signature_8','time_signature_7','time_signature_6','time_signature_5','time_signature_4','time_signature_3','time_signature_2','time_signature_1',
                                   'valence_10','valence_9','valence_8','valence_7','valence_6','valence_5','valence_4','valence_3','valence_2','valence_1',
                                   'acousticness_target_10','acousticness_target_9','acousticness_target_8','acousticness_target_7','acousticness_target_6','acousticness_target_5','acousticness_target_4','acousticness_target_3','acousticness_target_2','acousticness_target_1',
                                   'bounciness_target_10','bounciness_target_9','bounciness_target_8','bounciness_target_7','bounciness_target_6','bounciness_target_5','bounciness_target_4','bounciness_target_3','bounciness_target_2','bounciness_target_1',
                                   'danceability_target_10','danceability_target_9','danceability_target_8','danceability_target_7','danceability_target_6','danceability_target_5','danceability_target_4','danceability_target_3','danceability_target_2','danceability_target_1',
                                   'dyn_range_mean_target_10','dyn_range_mean_target_9','dyn_range_mean_target_8','dyn_range_mean_target_7','dyn_range_mean_target_6','dyn_range_mean_target_5','dyn_range_mean_target_4','dyn_range_mean_target_3','dyn_range_mean_target_2','dyn_range_mean_target_1',
                                   'energy_target_10','energy_target_9','energy_target_8','energy_target_7','energy_target_6','energy_target_5','energy_target_4','energy_target_3','energy_target_2','energy_target_1',
                                   'flatness_target_10','flatness_target_9','flatness_target_8','flatness_target_7','flatness_target_6','flatness_target_5','flatness_target_4','flatness_target_3','flatness_target_2','flatness_target_1',
                                   'instrumentalness_target_10','instrumentalness_target_9','instrumentalness_target_8','instrumentalness_target_7','instrumentalness_target_6','instrumentalness_target_5','instrumentalness_target_4','instrumentalness_target_3','instrumentalness_target_2','instrumentalness_target_1',
                                   'liveness_target_10','liveness_target_9','liveness_target_8','liveness_target_7','liveness_target_6','liveness_target_5','liveness_target_4','liveness_target_3','liveness_target_2','liveness_target_1',
                                   'loudness_target_10','loudness_target_9','loudness_target_8','loudness_target_7','loudness_target_6','loudness_target_5','loudness_target_4','loudness_target_3','loudness_target_2','loudness_target_1',
                                   'mechanism_target_10','mechanism_target_9','mechanism_target_8','mechanism_target_7','mechanism_target_6','mechanism_target_5','mechanism_target_4','mechanism_target_3','mechanism_target_2','mechanism_target_1',
                                   'organism_target_10','organism_target_9','organism_target_8','organism_target_7','organism_target_6','organism_target_5','organism_target_4','organism_target_3','organism_target_2','organism_target_1',
                                   'speechiness_target_10','speechiness_target_9','speechiness_target_8','speechiness_target_7','speechiness_target_6','speechiness_target_5','speechiness_target_4','speechiness_target_3','speechiness_target_2','speechiness_target_1',
                                   'time_signature_target_10','time_signature_target_9','time_signature_target_8','time_signature_target_7','time_signature_target_6','time_signature_target_5','time_signature_target_4','time_signature_target_3','time_signature_target_2','time_signature_target_1',
                                   'valence_target_10','valence_target_9','valence_target_8','valence_target_7','valence_target_6','valence_target_5','valence_target_4','valence_target_3','valence_target_2','valence_target_1'
)]



x_train[is.na(x_train)] = 0
x_train <- keras_array(array(as.matrix(x_train), dim = c(train_length,10,119)))
y_train <- train1[, .(skip_target_1, skip_target_2, skip_target_3, skip_target_4, skip_target_5, skip_target_6, skip_target_7, skip_target_8, skip_target_9, skip_target_10)]
y_train[is.na(y_train)] = 0
y_train <- keras_array(array(as.matrix(y_train), dim = c(train_length,10)))
gc()
x_test <- train2[, .SD, .SDcols=c('skip_2_10','skip_2_9','skip_2_8','skip_2_7','skip_2_6','skip_2_5','skip_2_4','skip_2_3','skip_2_2','skip_2_1',
                                  'trackdone_end_10','trackdone_end_9','trackdone_end_8','trackdone_end_7','trackdone_end_6','trackdone_end_5','trackdone_end_4','trackdone_end_3','trackdone_end_2','trackdone_end_1',
                                  'skip_1_10','skip_1_9','skip_1_8','skip_1_7','skip_1_6','skip_1_5','skip_1_4','skip_1_3','skip_1_2','skip_1_1',
                                  'skip_3_10','skip_3_9','skip_3_8','skip_3_7','skip_3_6','skip_3_5','skip_3_4','skip_3_3','skip_3_2','skip_3_1',
                                  'context_switch_10','context_switch_9','context_switch_8','context_switch_7','context_switch_6','context_switch_5','context_switch_4','context_switch_3','context_switch_2','context_switch_1',
                                  'no_pause_before_play_10','no_pause_before_play_9','no_pause_before_play_8','no_pause_before_play_7','no_pause_before_play_6','no_pause_before_play_5','no_pause_before_play_4','no_pause_before_play_3','no_pause_before_play_2','no_pause_before_play_1',
                                  'short_pause_before_play_10','short_pause_before_play_9','short_pause_before_play_8','short_pause_before_play_7','short_pause_before_play_6','short_pause_before_play_5','short_pause_before_play_4','short_pause_before_play_3','short_pause_before_play_2','short_pause_before_play_1',
                                  'long_pause_before_play_10','long_pause_before_play_9','long_pause_before_play_8','long_pause_before_play_7','long_pause_before_play_6','long_pause_before_play_5','long_pause_before_play_4','long_pause_before_play_3','long_pause_before_play_2','long_pause_before_play_1',
                                  'hist_user_behavior_n_seekfwd_10','hist_user_behavior_n_seekfwd_9','hist_user_behavior_n_seekfwd_8','hist_user_behavior_n_seekfwd_7','hist_user_behavior_n_seekfwd_6','hist_user_behavior_n_seekfwd_5','hist_user_behavior_n_seekfwd_4','hist_user_behavior_n_seekfwd_3','hist_user_behavior_n_seekfwd_2','hist_user_behavior_n_seekfwd_1',
                                  'hist_user_behavior_n_seekback_10','hist_user_behavior_n_seekback_9','hist_user_behavior_n_seekback_8','hist_user_behavior_n_seekback_7','hist_user_behavior_n_seekback_6','hist_user_behavior_n_seekback_5','hist_user_behavior_n_seekback_4','hist_user_behavior_n_seekback_3','hist_user_behavior_n_seekback_2','hist_user_behavior_n_seekback_1',
                                  'hist_user_behavior_is_shuffle_10','hist_user_behavior_is_shuffle_9','hist_user_behavior_is_shuffle_8','hist_user_behavior_is_shuffle_7','hist_user_behavior_is_shuffle_6','hist_user_behavior_is_shuffle_5','hist_user_behavior_is_shuffle_4','hist_user_behavior_is_shuffle_3','hist_user_behavior_is_shuffle_2','hist_user_behavior_is_shuffle_1',
                                  'editorial_playlist_10','editorial_playlist_9','editorial_playlist_8','editorial_playlist_7','editorial_playlist_6','editorial_playlist_5','editorial_playlist_4','editorial_playlist_3','editorial_playlist_2','editorial_playlist_1',
                                  'user_collection_10','user_collection_9','user_collection_8','user_collection_7','user_collection_6','user_collection_5','user_collection_4','user_collection_3','user_collection_2','user_collection_1',
                                  'radio_10','radio_9','radio_8','radio_7','radio_6','radio_5','radio_4','radio_3','radio_2','radio_1',
                                  'personalized_playlist_10','personalized_playlist_9','personalized_playlist_8','personalized_playlist_7','personalized_playlist_6','personalized_playlist_5','personalized_playlist_4','personalized_playlist_3','personalized_playlist_2','personalized_playlist_1',
                                  'catalog_10','catalog_9','catalog_8','catalog_7','catalog_6','catalog_5','catalog_4','catalog_3','catalog_2','catalog_1',
                                  'charts_10','charts_9','charts_8','charts_7','charts_6','charts_5','charts_4','charts_3','charts_2','charts_1',
                                  'fwdbtn_end_10','fwdbtn_end_9','fwdbtn_end_8','fwdbtn_end_7','fwdbtn_end_6','fwdbtn_end_5','fwdbtn_end_4','fwdbtn_end_3','fwdbtn_end_2','fwdbtn_end_1',
                                  'backbtn_end_10','backbtn_end_9','backbtn_end_8','backbtn_end_7','backbtn_end_6','backbtn_end_5','backbtn_end_4','backbtn_end_3','backbtn_end_2','backbtn_end_1',
                                  'endplay_end_10','endplay_end_9','endplay_end_8','endplay_end_7','endplay_end_6','endplay_end_5','endplay_end_4','endplay_end_3','endplay_end_2','endplay_end_1',
                                  'logout_end_10','logout_end_9','logout_end_8','logout_end_7','logout_end_6','logout_end_5','logout_end_4','logout_end_3','logout_end_2','logout_end_1',
                                  'clickrow_end_10','clickrow_end_9','clickrow_end_8','clickrow_end_7','clickrow_end_6','clickrow_end_5','clickrow_end_4','clickrow_end_3','clickrow_end_2','clickrow_end_1',
                                  'appload_end_10','appload_end_9','appload_end_8','appload_end_7','appload_end_6','appload_end_5','appload_end_4','appload_end_3','appload_end_2','appload_end_1',
                                  'uriopen_end_10','uriopen_end_9','uriopen_end_8','uriopen_end_7','uriopen_end_6','uriopen_end_5','uriopen_end_4','uriopen_end_3','uriopen_end_2','uriopen_end_1',
                                  'clickside_end_10','clickside_end_9','clickside_end_8','clickside_end_7','clickside_end_6','clickside_end_5','clickside_end_4','clickside_end_3','clickside_end_2','clickside_end_1',
                                  'popup_end_10','popup_end_9','popup_end_8','popup_end_7','popup_end_6','popup_end_5','popup_end_4','popup_end_3','popup_end_2','popup_end_1',
                                  'remote_end_10','remote_end_9','remote_end_8','remote_end_7','remote_end_6','remote_end_5','remote_end_4','remote_end_3','remote_end_2','remote_end_1',
                                  'trackdone_start_10','trackdone_start_9','trackdone_start_8','trackdone_start_7','trackdone_start_6','trackdone_start_5','trackdone_start_4','trackdone_start_3','trackdone_start_2','trackdone_start_1',
                                  'fwdbtn_start_10','fwdbtn_start_9','fwdbtn_start_8','fwdbtn_start_7','fwdbtn_start_6','fwdbtn_start_5','fwdbtn_start_4','fwdbtn_start_3','fwdbtn_start_2','fwdbtn_start_1',
                                  'backbtn_start_10','backbtn_start_9','backbtn_start_8','backbtn_start_7','backbtn_start_6','backbtn_start_5','backbtn_start_4','backbtn_start_3','backbtn_start_2','backbtn_start_1',
                                  'clickrow_start_10','clickrow_start_9','clickrow_start_8','clickrow_start_7','clickrow_start_6','clickrow_start_5','clickrow_start_4','clickrow_start_3','clickrow_start_2','clickrow_start_1',
                                  'appload_start_10','appload_start_9','appload_start_8','appload_start_7','appload_start_6','appload_start_5','appload_start_4','appload_start_3','appload_start_2','appload_start_1',
                                  'playbtn_start_10','playbtn_start_9','playbtn_start_8','playbtn_start_7','playbtn_start_6','playbtn_start_5','playbtn_start_4','playbtn_start_3','playbtn_start_2','playbtn_start_1',
                                  'remote_start_10','remote_start_9','remote_start_8','remote_start_7','remote_start_6','remote_start_5','remote_start_4','remote_start_3','remote_start_2','remote_start_1',
                                  'trackerror_start_10','trackerror_start_9','trackerror_start_8','trackerror_start_7','trackerror_start_6','trackerror_start_5','trackerror_start_4','trackerror_start_3','trackerror_start_2','trackerror_start_1',
                                  'endplay_start_10','endplay_start_9','endplay_start_8','endplay_start_7','endplay_start_6','endplay_start_5','endplay_start_4','endplay_start_3','endplay_start_2','endplay_start_1',
                                  'clickside_start_10','clickside_start_9','clickside_start_8','clickside_start_7','clickside_start_6','clickside_start_5','clickside_start_4','clickside_start_3','clickside_start_2','clickside_start_1',
                                  'uriopen_start_10','uriopen_start_9','uriopen_start_8','uriopen_start_7','uriopen_start_6','uriopen_start_5','uriopen_start_4','uriopen_start_3','uriopen_start_2','uriopen_start_1',
                                  'popup_start_10','popup_start_9','popup_start_8','popup_start_7','popup_start_6','popup_start_5','popup_start_4','popup_start_3','popup_start_2','popup_start_1',
                                  'percent_total_10','percent_total_9','percent_total_8','percent_total_7','percent_total_6','percent_total_5','percent_total_4','percent_total_3','percent_total_2','percent_total_1',
                                  #'Monday_10','Monday_9','Monday_8','Monday_7','Monday_6','Monday_5','Monday_4','Monday_3','Monday_2','Monday_1',
                                  #'Tuesday_10','Tuesday_9','Tuesday_8','Tuesday_7','Tuesday_6','Tuesday_5','Tuesday_4','Tuesday_3','Tuesday_2','Tuesday_1',
                                  #'Wednesday_10','Wednesday_9','Wednesday_8','Wednesday_7','Wednesday_6','Wednesday_5','Wednesday_4','Wednesday_3','Wednesday_2','Wednesday_1',
                                  #'Thursday_10','Thursday_9','Thursday_8','Thursday_7','Thursday_6','Thursday_5','Thursday_4','Thursday_3','Thursday_2','Thursday_1',
                                  #'Friday_10','Friday_9','Friday_8','Friday_7','Friday_6','Friday_5','Friday_4','Friday_3','Friday_2','Friday_1',
                                  #'Saturday_10','Saturday_9','Saturday_8','Saturday_7','Saturday_6','Saturday_5','Saturday_4','Saturday_3','Saturday_2','Saturday_1',
                                  #'Sunday_10','Sunday_9','Sunday_8','Sunday_7','Sunday_6','Sunday_5','Sunday_4','Sunday_3','Sunday_2','Sunday_1',
                                  'hour_0_10','hour_0_9','hour_0_8','hour_0_7','hour_0_6','hour_0_5','hour_0_4','hour_0_3','hour_0_2','hour_0_1',
                                  'hour_1_10','hour_1_9','hour_1_8','hour_1_7','hour_1_6','hour_1_5','hour_1_4','hour_1_3','hour_1_2','hour_1_1',
                                  'hour_2_10','hour_2_9','hour_2_8','hour_2_7','hour_2_6','hour_2_5','hour_2_4','hour_2_3','hour_2_2','hour_2_1',
                                  'hour_3_10','hour_3_9','hour_3_8','hour_3_7','hour_3_6','hour_3_5','hour_3_4','hour_3_3','hour_3_2','hour_3_1',
                                  'hour_4_10','hour_4_9','hour_4_8','hour_4_7','hour_4_6','hour_4_5','hour_4_4','hour_4_3','hour_4_2','hour_4_1',
                                  'hour_5_10','hour_5_9','hour_5_8','hour_5_7','hour_5_6','hour_5_5','hour_5_4','hour_5_3','hour_5_2','hour_5_1',
                                  'hour_6_10','hour_6_9','hour_6_8','hour_6_7','hour_6_6','hour_6_5','hour_6_4','hour_6_3','hour_6_2','hour_6_1',
                                  'hour_7_10','hour_7_9','hour_7_8','hour_7_7','hour_7_6','hour_7_5','hour_7_4','hour_7_3','hour_7_2','hour_7_1',
                                  'hour_8_10','hour_8_9','hour_8_8','hour_8_7','hour_8_6','hour_8_5','hour_8_4','hour_8_3','hour_8_2','hour_8_1',
                                  'hour_9_10','hour_9_9','hour_9_8','hour_9_7','hour_9_6','hour_9_5','hour_9_4','hour_9_3','hour_9_2','hour_9_1',
                                  'hour_10_10','hour_10_9','hour_10_8','hour_10_7','hour_10_6','hour_10_5','hour_10_4','hour_10_3','hour_10_2','hour_10_1',
                                  'hour_11_10','hour_11_9','hour_11_8','hour_11_7','hour_11_6','hour_11_5','hour_11_4','hour_11_3','hour_11_2','hour_11_1',
                                  'hour_12_10','hour_12_9','hour_12_8','hour_12_7','hour_12_6','hour_12_5','hour_12_4','hour_12_3','hour_12_2','hour_12_1',
                                  'hour_13_10','hour_13_9','hour_13_8','hour_13_7','hour_13_6','hour_13_5','hour_13_4','hour_13_3','hour_13_2','hour_13_1',
                                  'hour_14_10','hour_14_9','hour_14_8','hour_14_7','hour_14_6','hour_14_5','hour_14_4','hour_14_3','hour_14_2','hour_14_1',
                                  'hour_15_10','hour_15_9','hour_15_8','hour_15_7','hour_15_6','hour_15_5','hour_15_4','hour_15_3','hour_15_2','hour_15_1',
                                  'hour_16_10','hour_16_9','hour_16_8','hour_16_7','hour_16_6','hour_16_5','hour_16_4','hour_16_3','hour_16_2','hour_16_1',
                                  'hour_17_10','hour_17_9','hour_17_8','hour_17_7','hour_17_6','hour_17_5','hour_17_4','hour_17_3','hour_17_2','hour_17_1',
                                  'hour_18_10','hour_18_9','hour_18_8','hour_18_7','hour_18_6','hour_18_5','hour_18_4','hour_18_3','hour_18_2','hour_18_1',
                                  'hour_19_10','hour_19_9','hour_19_8','hour_19_7','hour_19_6','hour_19_5','hour_19_4','hour_19_3','hour_19_2','hour_19_1',
                                  'hour_20_10','hour_20_9','hour_20_8','hour_20_7','hour_20_6','hour_20_5','hour_20_4','hour_20_3','hour_20_2','hour_20_1',
                                  'hour_21_10','hour_21_9','hour_21_8','hour_21_7','hour_21_6','hour_21_5','hour_21_4','hour_21_3','hour_21_2','hour_21_1',
                                  'hour_22_10','hour_22_9','hour_22_8','hour_22_7','hour_22_6','hour_22_5','hour_22_4','hour_22_3','hour_22_2','hour_22_1',
                                  'hour_23_10','hour_23_9','hour_23_8','hour_23_7','hour_23_6','hour_23_5','hour_23_4','hour_23_3','hour_23_2','hour_23_1',
                                  'premium_10','premium_9','premium_8','premium_7','premium_6','premium_5','premium_4','premium_3','premium_2','premium_1',
                                  'acoustic_vector_0_10','acoustic_vector_0_9','acoustic_vector_0_8','acoustic_vector_0_7','acoustic_vector_0_6','acoustic_vector_0_5','acoustic_vector_0_4','acoustic_vector_0_3','acoustic_vector_0_2','acoustic_vector_0_1',
                                  'acoustic_vector_1_10','acoustic_vector_1_9','acoustic_vector_1_8','acoustic_vector_1_7','acoustic_vector_1_6','acoustic_vector_1_5','acoustic_vector_1_4','acoustic_vector_1_3','acoustic_vector_1_2','acoustic_vector_1_1',
                                  'acoustic_vector_2_10','acoustic_vector_2_9','acoustic_vector_2_8','acoustic_vector_2_7','acoustic_vector_2_6','acoustic_vector_2_5','acoustic_vector_2_4','acoustic_vector_2_3','acoustic_vector_2_2','acoustic_vector_2_1',
                                  'acoustic_vector_3_10','acoustic_vector_3_9','acoustic_vector_3_8','acoustic_vector_3_7','acoustic_vector_3_6','acoustic_vector_3_5','acoustic_vector_3_4','acoustic_vector_3_3','acoustic_vector_3_2','acoustic_vector_3_1',
                                  'acoustic_vector_4_10','acoustic_vector_4_9','acoustic_vector_4_8','acoustic_vector_4_7','acoustic_vector_4_6','acoustic_vector_4_5','acoustic_vector_4_4','acoustic_vector_4_3','acoustic_vector_4_2','acoustic_vector_4_1',
                                  'acoustic_vector_5_10','acoustic_vector_5_9','acoustic_vector_5_8','acoustic_vector_5_7','acoustic_vector_5_6','acoustic_vector_5_5','acoustic_vector_5_4','acoustic_vector_5_3','acoustic_vector_5_2','acoustic_vector_5_1',
                                  'acoustic_vector_6_10','acoustic_vector_6_9','acoustic_vector_6_8','acoustic_vector_6_7','acoustic_vector_6_6','acoustic_vector_6_5','acoustic_vector_6_4','acoustic_vector_6_3','acoustic_vector_6_2','acoustic_vector_6_1',
                                  'acoustic_vector_7_10','acoustic_vector_7_9','acoustic_vector_7_8','acoustic_vector_7_7','acoustic_vector_7_6','acoustic_vector_7_5','acoustic_vector_7_4','acoustic_vector_7_3','acoustic_vector_7_2','acoustic_vector_7_1',
                                  'acoustic_vector_0_target_10','acoustic_vector_0_target_9','acoustic_vector_0_target_8','acoustic_vector_0_target_7','acoustic_vector_0_target_6','acoustic_vector_0_target_5','acoustic_vector_0_target_4','acoustic_vector_0_target_3','acoustic_vector_0_target_2','acoustic_vector_0_target_1',
                                  'acoustic_vector_1_target_10','acoustic_vector_1_target_9','acoustic_vector_1_target_8','acoustic_vector_1_target_7','acoustic_vector_1_target_6','acoustic_vector_1_target_5','acoustic_vector_1_target_4','acoustic_vector_1_target_3','acoustic_vector_1_target_2','acoustic_vector_1_target_1',
                                  'acoustic_vector_2_target_10','acoustic_vector_2_target_9','acoustic_vector_2_target_8','acoustic_vector_2_target_7','acoustic_vector_2_target_6','acoustic_vector_2_target_5','acoustic_vector_2_target_4','acoustic_vector_2_target_3','acoustic_vector_2_target_2','acoustic_vector_2_target_1',
                                  'acoustic_vector_3_target_10','acoustic_vector_3_target_9','acoustic_vector_3_target_8','acoustic_vector_3_target_7','acoustic_vector_3_target_6','acoustic_vector_3_target_5','acoustic_vector_3_target_4','acoustic_vector_3_target_3','acoustic_vector_3_target_2','acoustic_vector_3_target_1',
                                  'acoustic_vector_4_target_10','acoustic_vector_4_target_9','acoustic_vector_4_target_8','acoustic_vector_4_target_7','acoustic_vector_4_target_6','acoustic_vector_4_target_5','acoustic_vector_4_target_4','acoustic_vector_4_target_3','acoustic_vector_4_target_2','acoustic_vector_4_target_1',
                                  'acoustic_vector_5_target_10','acoustic_vector_5_target_9','acoustic_vector_5_target_8','acoustic_vector_5_target_7','acoustic_vector_5_target_6','acoustic_vector_5_target_5','acoustic_vector_5_target_4','acoustic_vector_5_target_3','acoustic_vector_5_target_2','acoustic_vector_5_target_1',
                                  'acoustic_vector_6_target_10','acoustic_vector_6_target_9','acoustic_vector_6_target_8','acoustic_vector_6_target_7','acoustic_vector_6_target_6','acoustic_vector_6_target_5','acoustic_vector_6_target_4','acoustic_vector_6_target_3','acoustic_vector_6_target_2','acoustic_vector_6_target_1',
                                  'acoustic_vector_7_target_10','acoustic_vector_7_target_9','acoustic_vector_7_target_8','acoustic_vector_7_target_7','acoustic_vector_7_target_6','acoustic_vector_7_target_5','acoustic_vector_7_target_4','acoustic_vector_7_target_3','acoustic_vector_7_target_2','acoustic_vector_7_target_1',
                                  'duration_10','duration_9','duration_8','duration_7','duration_6','duration_5','duration_4','duration_3','duration_2','duration_1',
                                  'duration_target_10','duration_target_9','duration_target_8','duration_target_7','duration_target_6','duration_target_5','duration_target_4','duration_target_3','duration_target_2','duration_target_1',
                                  'beat_strength_target_10','beat_strength_target_9','beat_strength_target_8','beat_strength_target_7','beat_strength_target_6','beat_strength_target_5','beat_strength_target_4','beat_strength_target_3','beat_strength_target_2','beat_strength_target_1',
                                  'beat_strength_10','beat_strength_9','beat_strength_8','beat_strength_7','beat_strength_6','beat_strength_5','beat_strength_4','beat_strength_3','beat_strength_2','beat_strength_1',
                                  'tempo_10','tempo_9','tempo_8','tempo_7','tempo_6','tempo_5','tempo_4','tempo_3','tempo_2','tempo_1',
                                  'tempo_target_10','tempo_target_9','tempo_target_8','tempo_target_7','tempo_target_6','tempo_target_5','tempo_target_4','tempo_target_3','tempo_target_2','tempo_target_1',
                                  'release_year_10','release_year_9','release_year_8','release_year_7','release_year_6','release_year_5','release_year_4','release_year_3','release_year_2','release_year_1',
                                  'release_year_target_10','release_year_target_9','release_year_target_8','release_year_target_7','release_year_target_6','release_year_target_5','release_year_target_4','release_year_target_3','release_year_target_2','release_year_target_1',
                                  'us_popularity_estimate_10','us_popularity_estimate_9','us_popularity_estimate_8','us_popularity_estimate_7','us_popularity_estimate_6','us_popularity_estimate_5','us_popularity_estimate_4','us_popularity_estimate_3','us_popularity_estimate_2','us_popularity_estimate_1',
                                  'us_popularity_estimate_target_10','us_popularity_estimate_target_9','us_popularity_estimate_target_8','us_popularity_estimate_target_7','us_popularity_estimate_target_6','us_popularity_estimate_target_5','us_popularity_estimate_target_4','us_popularity_estimate_target_3','us_popularity_estimate_target_2','us_popularity_estimate_target_1',
                                  'acousticness_10','acousticness_9','acousticness_8','acousticness_7','acousticness_6','acousticness_5','acousticness_4','acousticness_3','acousticness_2','acousticness_1',
                                  'bounciness_10','bounciness_9','bounciness_8','bounciness_7','bounciness_6','bounciness_5','bounciness_4','bounciness_3','bounciness_2','bounciness_1',
                                  'danceability_10','danceability_9','danceability_8','danceability_7','danceability_6','danceability_5','danceability_4','danceability_3','danceability_2','danceability_1',
                                  'dyn_range_mean_10','dyn_range_mean_9','dyn_range_mean_8','dyn_range_mean_7','dyn_range_mean_6','dyn_range_mean_5','dyn_range_mean_4','dyn_range_mean_3','dyn_range_mean_2','dyn_range_mean_1',
                                  'energy_10','energy_9','energy_8','energy_7','energy_6','energy_5','energy_4','energy_3','energy_2','energy_1',
                                  'flatness_10','flatness_9','flatness_8','flatness_7','flatness_6','flatness_5','flatness_4','flatness_3','flatness_2','flatness_1',
                                  'instrumentalness_10','instrumentalness_9','instrumentalness_8','instrumentalness_7','instrumentalness_6','instrumentalness_5','instrumentalness_4','instrumentalness_3','instrumentalness_2','instrumentalness_1',
                                  'liveness_10','liveness_9','liveness_8','liveness_7','liveness_6','liveness_5','liveness_4','liveness_3','liveness_2','liveness_1',
                                  'loudness_10','loudness_9','loudness_8','loudness_7','loudness_6','loudness_5','loudness_4','loudness_3','loudness_2','loudness_1',
                                  'mechanism_10','mechanism_9','mechanism_8','mechanism_7','mechanism_6','mechanism_5','mechanism_4','mechanism_3','mechanism_2','mechanism_1',
                                  'organism_10','organism_9','organism_8','organism_7','organism_6','organism_5','organism_4','organism_3','organism_2','organism_1',
                                  'speechiness_10','speechiness_9','speechiness_8','speechiness_7','speechiness_6','speechiness_5','speechiness_4','speechiness_3','speechiness_2','speechiness_1',
                                  'time_signature_10','time_signature_9','time_signature_8','time_signature_7','time_signature_6','time_signature_5','time_signature_4','time_signature_3','time_signature_2','time_signature_1',
                                  'valence_10','valence_9','valence_8','valence_7','valence_6','valence_5','valence_4','valence_3','valence_2','valence_1',
                                  'acousticness_target_10','acousticness_target_9','acousticness_target_8','acousticness_target_7','acousticness_target_6','acousticness_target_5','acousticness_target_4','acousticness_target_3','acousticness_target_2','acousticness_target_1',
                                  'bounciness_target_10','bounciness_target_9','bounciness_target_8','bounciness_target_7','bounciness_target_6','bounciness_target_5','bounciness_target_4','bounciness_target_3','bounciness_target_2','bounciness_target_1',
                                  'danceability_target_10','danceability_target_9','danceability_target_8','danceability_target_7','danceability_target_6','danceability_target_5','danceability_target_4','danceability_target_3','danceability_target_2','danceability_target_1',
                                  'dyn_range_mean_target_10','dyn_range_mean_target_9','dyn_range_mean_target_8','dyn_range_mean_target_7','dyn_range_mean_target_6','dyn_range_mean_target_5','dyn_range_mean_target_4','dyn_range_mean_target_3','dyn_range_mean_target_2','dyn_range_mean_target_1',
                                  'energy_target_10','energy_target_9','energy_target_8','energy_target_7','energy_target_6','energy_target_5','energy_target_4','energy_target_3','energy_target_2','energy_target_1',
                                  'flatness_target_10','flatness_target_9','flatness_target_8','flatness_target_7','flatness_target_6','flatness_target_5','flatness_target_4','flatness_target_3','flatness_target_2','flatness_target_1',
                                  'instrumentalness_target_10','instrumentalness_target_9','instrumentalness_target_8','instrumentalness_target_7','instrumentalness_target_6','instrumentalness_target_5','instrumentalness_target_4','instrumentalness_target_3','instrumentalness_target_2','instrumentalness_target_1',
                                  'liveness_target_10','liveness_target_9','liveness_target_8','liveness_target_7','liveness_target_6','liveness_target_5','liveness_target_4','liveness_target_3','liveness_target_2','liveness_target_1',
                                  'loudness_target_10','loudness_target_9','loudness_target_8','loudness_target_7','loudness_target_6','loudness_target_5','loudness_target_4','loudness_target_3','loudness_target_2','loudness_target_1',
                                  'mechanism_target_10','mechanism_target_9','mechanism_target_8','mechanism_target_7','mechanism_target_6','mechanism_target_5','mechanism_target_4','mechanism_target_3','mechanism_target_2','mechanism_target_1',
                                  'organism_target_10','organism_target_9','organism_target_8','organism_target_7','organism_target_6','organism_target_5','organism_target_4','organism_target_3','organism_target_2','organism_target_1',
                                  'speechiness_target_10','speechiness_target_9','speechiness_target_8','speechiness_target_7','speechiness_target_6','speechiness_target_5','speechiness_target_4','speechiness_target_3','speechiness_target_2','speechiness_target_1',
                                  'time_signature_target_10','time_signature_target_9','time_signature_target_8','time_signature_target_7','time_signature_target_6','time_signature_target_5','time_signature_target_4','time_signature_target_3','time_signature_target_2','time_signature_target_1',
                                  'valence_target_10','valence_target_9','valence_target_8','valence_target_7','valence_target_6','valence_target_5','valence_target_4','valence_target_3','valence_target_2','valence_target_1'
)]
x_test[is.na(x_test)] = 0
x_test <- keras_array(array(as.matrix(x_test), dim = c(test_length,10,119)))
#x_test <- keras_array(x_test)
y_test <- train2[, .(skip_target_1, skip_target_2, skip_target_3, skip_target_4, skip_target_5, skip_target_6, skip_target_7, skip_target_8, skip_target_9, skip_target_10)]
y_test[is.na(y_test)] = 0
y_test <- keras_array(array(as.matrix(y_test), dim = c(test_length,10)))
gc()

model <- keras_model_sequential()
model %>% #good
  bidirectional(layer_lstm(units = 119, dropout = 0.2, recurrent_dropout = 0.2, return_sequences = TRUE), merge_mode = 'concat') %>%
  bidirectional(layer_lstm(units = 119, dropout = 0.2, recurrent_dropout = 0.2), merge_mode = 'concat') %>% 
  layer_dense(units = 10, activation = 'sigmoid')

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'nadam',
  metrics = c('accuracy')
)

cat('Train...\n')
history <- model %>% fit(
  x_train, y_train,
  batch_size = 512,
  epochs = 10,
  validation_data = list(x_test, y_test)
)

#model %>% evaluate(x_test, y_test,verbose = 1)
#model %>% predict_classes(x_test)
preds = model %>% predict_proba(x_test)
setorder(test2, session_id, session_position) 
colnames(preds) = paste0('prediction_', 1:10)
test2_test = data.table(session_id = train2[, session_id],preds)
#setnames(test2_test, 'V1', 'session_id')
setnames(test2_test, old = paste0('prediction_', 1:10), new = as.character(c(1:10)))

test2_test = melt(test2_test, id.vars = c('session_id'), measure.vars = as.character(c(1:10)), variable.name = 'test_position', value.name = 'prediction')
set(test2_test, j = 'test_position', value = as.integer(as.character((test2_test[, test_position]))))

test2 = merge(x = test2, y = test2_test, by = c('session_id','test_position'), all.x = TRUE)
setorder(test2, session_id, session_position)
print(files)
print(ave_pre(test2))

test2[, prediction := (prediction.model1 * 1 + prediction.model2 * 4 + prediction.model4 * 2 + prediction.model5 * 1 + percent_total * 5)/13]
test2[, prediction := NULL]
test2[, prediction.model5 := prediction]

save_model_hdf5(object = model, filepath = paste0(model_dir,"model7.h5"))
load_model_hdf5(filepath = paste0(model_dir,"model1.h5"))


colMeans(cor(track2[,.(duration, us_popularity_estimate,release_year,acousticness,beat_strength,bounciness,danceability,dyn_range_mean,energy,flatness,instrumentalness,liveness,loudness,mechanism,organism,speechiness,tempo,time_signature,valence)]))
gc()
colnames(track2)
