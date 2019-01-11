rm(list = ls())
gc()

library('xgboost')
library('data.table')

train_dir = '/home/cddt/data-space/spotify-challenge/data/training_set/'
test_dir = '/home/cddt/data-space/spotify-challenge/data/test_set/'
sub_dir = '/home/cddt/data-space/spotify-challenge/data/submissions/sub19/'
model_dir = '/home/cddt/data-space/spotify-challenge/data/models/models19/'

filelist = list.files(path = train_dir, pattern = 'log_*', all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
filelist_models = list.files(path = model_dir, pattern = 'log_*', all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
#filelist_ans = list.files(path = test_dir, pattern = 'log_input*', all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

#track = fread(file = '/home/cddt/data-space/spotify-challenge/data/data_augmentation/track_train1', select = c('track_id','percent_total'))

# flatten user behaviour categorical data
# train on this to produce a model, the predictions from which will become an engineered feature

track = fread(file = '/home/cddt/data-space/spotify-challenge/data/data_augmentation/track_train1', select = c('track_id','percent_total'))

#track = fread(file = '/home/cddt/data-space/spotify-challenge/data/data_augmentation/track_train1', select = c('track_id','percent_total'))

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
#for (i in 1:length(filelist)) {
  #train1 <- fread(file = paste0(train_dir, filelist[411]), select = c('session_id','session_position','skip_2','session_length','context_switch','no_pause_before_play','short_pause_before_play','long_pause_before_play','hist_user_behavior_n_seekfwd','hist_user_behavior_n_seekback','hist_user_behavior_is_shuffle','hour_of_day','premium','date','context_type','hist_user_behavior_reason_start','hist_user_behavior_reason_end','track_id_clean'))
  
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
  #train1 = merge(x = train1, y = track[, .(track_id, percent_total)], by.x = 'track_id_clean', by.y = 'track_id', all.x = TRUE)
  test1 = merge(x = test1, y = track[, .(track_id, percent_total)], by.x = 'track_id_clean', by.y = 'track_id', all.x = TRUE)
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
  
  context_type = dcast(train1, session_id + train_position ~ context_type, value.var = c('temp_1'))
  hist_user_behavior_reason_end = dcast(train1, session_id + train_position ~ hist_user_behavior_reason_end, value.var = c('temp_1'))
  hist_user_behavior_reason_start = dcast(train1, session_id + train_position ~ hist_user_behavior_reason_start, value.var = c('temp_1'))
  
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
  
  context_type = dcast(context_type, session_id ~ train_position, value.var = c(cols1))
  hist_user_behavior_reason_end = dcast(hist_user_behavior_reason_end, session_id ~ train_position, value.var = c(cols2))
  hist_user_behavior_reason_start = dcast(hist_user_behavior_reason_start, session_id ~ train_position, value.var = c(cols3))
  
  train1 = dcast(train1, session_id + session_length ~ train_position, value.var = c("skip_2","skip_1","skip_3",'context_switch','no_pause_before_play','short_pause_before_play','long_pause_before_play','hist_user_behavior_n_seekfwd','hist_user_behavior_n_seekback','hist_user_behavior_is_shuffle'))
  train1 = merge(x = train1, y = context_type, by = 'session_id', all.x = TRUE)
  train1 = merge(x = train1, y = hist_user_behavior_reason_end, by = 'session_id', all.x = TRUE)
  train1 = merge(x = train1, y = hist_user_behavior_reason_start, by = 'session_id', all.x = TRUE)
  #colnames(train1[, -c('session_id','session_length')])
  
  test1_test = dcast(test1, session_id ~ test_position, value.var = c('skip_2','percent_total'))
  colnames(test1_test)[2:11] = paste0('skip_target_',1:10)
  train1 = merge(x = train1, y = test1_test, by = 'session_id', all.x = TRUE)
  
  l <- list(train1, test1, test1_test)
  return(l)
}

iteration = 9

for (iteration in 5:7) {
  files = sample(x = c(1:650)[-96], size = 4, replace = FALSE)
  #files = c(85, 295, 476)
  prepared = prepare_file(filelist[files])
  train1 = prepared[[1]]
  test1 = prepared[[2]]
  test1_test = prepared[[3]]
  rm(prepared)
  prepared2 = prepare_file(filelist[96])
  train2 = prepared2[[1]]
  test2 = prepared2[[2]]
  test2_test = prepared2[[3]]
  rm(prepared2)
  gc()
  
  prepare_model = function(xtrain, target, test_data){
    #xtrain = train1; target = 1; test_data = train2;
    target_col = paste0('skip_target_',target)
    target_col_prev = paste0('skip_target_',target-1)
    temp_session_id = na.omit(test_data[, c('session_id', target_col), with = FALSE], cols = target_col)
    not_target = c(1:10)[which(c(1:10) != target)]
    not_target_col = paste0('percent_total_', not_target)
    temp_data = na.omit(xtrain, cols = target_col)
    dtrain1 = data.matrix(temp_data[, -c('session_id','session_length','skip_target_1','skip_target_2','skip_target_3','skip_target_4','skip_target_5','skip_target_6','skip_target_7','skip_target_8','skip_target_9','skip_target_10', not_target_col), with = FALSE])
    dlabel1 = data.matrix(temp_data[is.na(c(target_col)) == FALSE, c(target_col), with = FALSE])
    
    # bst.cv <- xgb.cv(data = dtrain1,
    #                label = dlabel1,
    #                max.depth = 11,
    #                eta = .05,
    #                nthread = 12,
    #                nrounds = 50,
    #                objective = "binary:logistic",
    #                nfold = 3,
    #                early_stopping_rounds = 5,
    #                tree_method = 'exact')
    bst.nn <- xgboost(data = dtrain1,
                      label = dlabel1, 
                      max.depth = 11, 
                      eta = .05, 
                      nthread = 12, 
                      nrounds = 50, 
                      objective = 'binary:logistic',
                      tree_method = 'exact')
    temp_test_data = na.omit(test_data, cols = target_col)
    xtest_data = data.matrix(temp_test_data[, -c('session_id','session_length','skip_target_1','skip_target_2','skip_target_3','skip_target_4','skip_target_5','skip_target_6','skip_target_7','skip_target_8','skip_target_9','skip_target_10', not_target_col), with = FALSE])
    ifelse(!dir.exists(file.path(model_dir, paste0('v',iteration,'/'))), dir.create(file.path(model_dir, paste0('v',iteration,'/'))), FALSE)
    save(bst.nn, file = paste0(model_dir,'v',iteration,'/', target_col))
    temp_session_id[, paste0('prediction_',target) := predict(object = bst.nn, newdata = xtest_data, type = 'response')]
    temp_session_id[, c(target_col) := NULL]
    l <- list(bst.nn, temp_session_id)
    return(l)
  }
  for (target in 1:10){
    model1 = prepare_model(train1, target, train2)
    model = model1[[1]]
    predictions = model1[[2]]
    test2_test = merge(x = test2_test, y = predictions, by = 'session_id', all.x = TRUE)
    gc()
  }
  #or only to test
  # model_dir = '/home/cddt/data-space/spotify-challenge/data/models/models19/v5/'
  # for (target in 1:10){
  #   not_target = c(1:10)[which(c(1:10) != target)]
  #   not_target_col = paste0('percent_total_', not_target)
  #   xtest_data = data.matrix(train2[, -c('session_id','session_length', 'skip_target_1', 'skip_target_2', 'skip_target_3', 'skip_target_4', 'skip_target_5', 'skip_target_6', 'skip_target_7', 'skip_target_8', 'skip_target_9', 'skip_target_10', not_target_col), with = FALSE])
  #   target_col = paste0('skip_target_',target)
  #   load(file = paste0(model_dir, target_col))
  #   test2_test[, paste0('prediction_',target) := predict(object = bst.nn, newdata = xtest_data, type = 'response')]
  # }
  
  setnames(test2_test, old = paste0('prediction_', 1:10), new = as.character(c(1:10)))
  test2_test = melt(test2_test, id.vars = c('session_id'), measure.vars = as.character(c(1:10)), variable.name = 'test_position', value.name = 'prediction')
  set(test2_test, j = 'test_position', value = as.integer(as.character((test2_test[, test_position]))))
  
  test2 = merge(x = test2, y = test2_test, by = c('session_id','test_position'), all.x = TRUE)
  setorder(test2, session_id, session_position)
  print(files)
  print(ave_pre(test2))
}
################
load(file = paste0(model_dir, 'v2/skip_target_2'))
importance <- xgb.importance(model = bst.nn)
print(xgb.plot.importance(importance_matrix = importance[1:20]))
setorder(test2, session_id, session_position) 
hist(test2[,prediction])
#ensemble = test2[, c('prediction','session_id','session_position')]
ensemble[, prediction5 := prediction]
ensemble[, prediction := NULL]
#ensemble = test2[, c('skip_2','session_id','session_position', 'prediction', 'session_length','N')]

ensemble = merge(x = ensemble, y = test2[, c('session_id','session_position','prediction')], by = c('session_id', 'session_position'), all.x = TRUE)
ensemble[, prediction := ifelse((ifelse(prediction1 >= 0.5, 1, 0) + ifelse(prediction2 >= 0.5, 1, 0) + ifelse(prediction3 >= 0.5, 1, 0) + ifelse(prediction4 >= 0.5, 1, 0) + ifelse(prediction5 >= 0.5, 1, 0)) / 5 > 0.5, 1, 0)]
setorder(ensemble, session_id, session_position) 
ave_pre(ensemble)
################

# create submission
filelist2 = list.files(path = test_dir, pattern = 'log_pre*', all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
filelist2_ans = list.files(path = test_dir, pattern = 'log_input*', all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

for (i in 1:length(filelist2)) {
  train1 <- fread(file = paste0(test_dir, filelist2[i]), select = c('session_id','session_position','skip_1','skip_2','skip_3','session_length','context_switch','no_pause_before_play','short_pause_before_play','long_pause_before_play','hist_user_behavior_n_seekfwd','hist_user_behavior_n_seekback','hist_user_behavior_is_shuffle','hour_of_day','premium','date','context_type','hist_user_behavior_reason_start','hist_user_behavior_reason_end','track_id_clean'))
  test1 <- fread(file = paste0(test_dir, filelist2_ans[i]), select = c('session_id','session_length','track_id_clean','session_position'))
  
  train1 = merge(x = train1, y = train1[, .N, by = session_id], by = 'session_id', all.x = TRUE)
  test1 = merge(x = test1, y = test1[, .N, by = session_id], by = 'session_id', all.x = TRUE)
  #train1 = merge(x = train1, y = track[, .(track_id, percent_total)], by.x = 'track_id_clean', by.y = 'track_id', all.x = TRUE)
  test1 = merge(x = test1, y = track[, .(track_id, percent_total)], by.x = 'track_id_clean', by.y = 'track_id', all.x = TRUE)
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
  
  context_type = dcast(train1, session_id + train_position ~ context_type, value.var = c('temp_1'))
  hist_user_behavior_reason_end = dcast(train1, session_id + train_position ~ hist_user_behavior_reason_end, value.var = c('temp_1'))
  hist_user_behavior_reason_start = dcast(train1, session_id + train_position ~ hist_user_behavior_reason_start, value.var = c('temp_1'))
  
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
  
  context_type = dcast(context_type, session_id ~ train_position, value.var = c(cols1))
  hist_user_behavior_reason_end = dcast(hist_user_behavior_reason_end, session_id ~ train_position, value.var = c(cols2))
  hist_user_behavior_reason_start = dcast(hist_user_behavior_reason_start, session_id ~ train_position, value.var = c(cols3))
  
  train1 = dcast(train1, session_id + session_length ~ train_position, value.var = c("skip_2","skip_1","skip_3",'context_switch','no_pause_before_play','short_pause_before_play','long_pause_before_play','hist_user_behavior_n_seekfwd','hist_user_behavior_n_seekback','hist_user_behavior_is_shuffle'))
  train1 = merge(x = train1, y = context_type, by = 'session_id', all.x = TRUE)
  train1 = merge(x = train1, y = hist_user_behavior_reason_end, by = 'session_id', all.x = TRUE)
  train1 = merge(x = train1, y = hist_user_behavior_reason_start, by = 'session_id', all.x = TRUE)
  #colnames(train1[, -c('session_id','session_length')])
  
  #test1_test = data.table(session_id = unique(test1[, session_id]))
  test1_test = dcast(test1, session_id ~ test_position, value.var = c('percent_total'))
  colnames(test1_test)[2:11] = paste0('percent_total_',1:10)
  train1 = merge(x = train1, y = test1_test, by = 'session_id', all.x = TRUE)

  for (target in 1:10) {test1_test[, paste0('prediction_',target) := 0]}
  model_count = 5
  for (model in 5:9){
    #colnames(test1_test)[2:11] = paste0('skip_target_',1:10)
    #train1 = merge(x = train1, y = test1_test, by = 'session_id', all.x = TRUE)
    for (target in 1:10){
      #target = 1; model = 1;
      target_col = paste0('skip_target_',target)
      load(file = paste0(model_dir,'v',model,'/',target_col))
      not_target = c(1:10)[which(c(1:10) != target)]
      not_target_col = paste0('percent_total_', not_target)
      #temp_test_data = na.omit(test2_test, cols = target_col)
      xtest_data = data.matrix(train1[, -c('session_id','session_length',not_target_col), with = FALSE])
      #test1_test[, prediction_new := predict(object = bst.nn, newdata = xtest_data, type = 'response')]
      oldcol = test1_test[, c(paste0('prediction_',target)), with = FALSE]
      test1_test[, paste0('prediction_',target) := oldcol + predict(object = bst.nn, newdata = xtest_data, type = 'response')]
      gc()
    }
    gc()
  }
  setnames(test1_test, old = paste0('prediction_', 1:10), new = as.character(c(1:10)))
  test1_test = melt(test1_test, id.vars = c('session_id'), measure.vars = as.character(c(1:10)), variable.name = 'test_position', value.name = 'prediction')
  set(test1_test, j = 'test_position', value = as.integer(as.character((test1_test[, test_position]))))
    
  test1 = merge(x = test1, y = test1_test, by = c('session_id','test_position'), all.x = TRUE)
  test1 = merge(x = test1, y = track, by.x = 'track_id_clean', by.y = 'track_id', all.x = TRUE)
  
  setorder(test1, session_id, session_position)
    
  test1[, ans := ifelse((prediction) / model_count >= 0.5, 1, 0)]
  
  final_ans = test1[, lapply(.SD, paste, collapse = ''), .SDcols = c('ans'), by = 'session_id']
  
  write.table(test1[, c('session_id','session_position','prediction')], paste0(sub_dir,filelist2_ans[i],'3raw-5'), row.names=FALSE, col.names=FALSE, quote = FALSE) 
  write.table(final_ans[,ans], paste0(sub_dir,filelist2_ans[i],3), row.names=FALSE, col.names=FALSE, quote = FALSE) 
  rm(list = c('test1','train1','test1_test','final_ans','xtest_data','oldcol','hist_user_behavior_reason_end','hist_user_behavior_reason_start','context_type','bst.nn'))
  gc()
}
