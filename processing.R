df_full <- data.frame()
ACS <- "ACS_Map/ACS_simple.shp"
arcpy$MakeFeatureLayer_management(in_features = ACS,
                                  out_layer = 'ACS_lyr')
arcpy$MakeFeatureLayer_management(in_features = "/Bus_Stops/Bus_Clusters.shp",
                                  out_layer = "temp_lyr")
num_units <- arcpy$GetCount_management(in_rows = "temp_lyr")

for (i in 0:num_units[0]) {
  # for (i in 40:50){
  print(i)
  single_stop <- arcpy$SelectLayerByAttribute_management("temp_lyr",
                                                         "NEW_SELECTION",
                                                         paste('"FID" =',i,sep = ''))
  
  dist.single.stop <- arcpy$sa$EucDistance(in_source_data = single_stop,
                                           maximum_distance = maximum_distance,
                                           cell_size = cell_size)
  
  
  #Select features close to the point
  ACS_select <- arcpy$SelectLayerByLocation_management(in_layer = 'ACS_lyr',
                                                       overlap_type ='WITHIN_A_DISTANCE',
                                                       select_features = single_stop,
                                                       search_distance = maximum_distance,
                                                       selection_type = "NEW_SELECTION")
  success <- FALSE
  # while (!success) {
  #   tryCatch(
  #     {
  # 
  #     extract <- arcpy$ExtractValuesToTable_ga(in_features = ACS_select,
  #                                 in_rasters = dist.single.stop,
  #                                 out_table = 'temp_dist_table.dbf')
  #     success <- TRUE
  #     },
  #     error = {success <- FALSE
  #     print('Failed Extract. Repeating...')}
  #   )
  # }
  
  extract <- arcpy$ExtractValuesToTable_ga(in_features = ACS_select,
                                           in_rasters = dist.single.stop,
                                           out_table = 'temp_dist_table.dbf')
  
  
  #Remove raster
  arcpy$Delete_management(in_data = dist.single.stop)
  
  df_zonal <- arc.open(path = paste(workspace,'/temp_dist_table.dbf',sep='')) %>% 
    arc.select(fields = c('SrcID_Feat','Value')) %>% 
    mutate(unit=i) %>% 
    group_by(SrcID_Feat,unit) %>% 
    summarize(mean_dist=mean(Value))
  
  
  df_full <- df_full %>% bind_rows(df_zonal)
}

write.csv(df_full, file=paste(workspace,"bus_distances.csv",sep = ''))

df <- df_full %>% 
  group_by(SrcID_Feat) %>% 
  summarise(score=sum(1/mean_dist))

bus_lyr <- arc.open('temp_lyr') %>% arc.select('STOPID')