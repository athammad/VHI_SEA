

var MyFolder='cambodia' /// The folder is created automatically

var StartDate='2000-04-01' 
var EnDate='2021-07-31'


var cropland = ee.Image("USGS/GFSAD1000_V1")
var cropland = cropland.mask(cropland.gt(0))


///=====================================================
/// ============== COVARS  DOWNLOAD ====================
///=====================================================

///=======
//RAIN 1981-01-01T00:00:00 - 2021-07-31
///=======
var dataset =  ee.ImageCollection('UCSB-CHG/CHIRPS/DAILY')
                 .filterBounds(table)
                 .filter(ee.Filter.date(StartDate,EnDate));
 
// GET cropland
               
var RAIN = dataset.select('precipitation')
            .map(function(image){
               return image.mask(cropland)
            });

//// get mean values 
RAIN = RAIN.map(function(image){
var  dict = image.reduceRegion({reducer: ee.Reducer.median(), geometry: table, scale: 1000, maxPixels: 10e13})
return image.set(dict)
  
});

/// chnage date format
RAIN = RAIN.map(function(image){
  return image.set('timeFormat', image.date().format('dd-MM-yyyy'))
});

print(RAIN)

Export.table.toDrive({collection: RAIN, 
                      description: 'RAIN',
                      folder:MyFolder,
                      fileFormat: 'CSV',
                      selectors: ['timeFormat', 'precipitation']});


///=======
//TEMPERATURE 2000-03-05T00:00:00 -  2021-08-20
///=======
var dataset =  ee.ImageCollection('MODIS/006/MOD11A1')
                 .filterBounds(table)
                .filter(ee.Filter.date(StartDate,EnDate));

var TEMP = dataset.select('LST_Day_1km') 


///CHNAGE FROM F TO C
var TEMP =TEMP.map(function(image){
  return image.multiply(0.02).subtract(273.15).copyProperties(image, ['system:time_start']);
});

//// get mean values 
TEMP = TEMP.map(function(image){
var  dict = image.reduceRegion({reducer: ee.Reducer.median(), geometry: table, scale: 1000, maxPixels: 10e13})
return image.set(dict)
  
});

// GET cropland
var TEMP = TEMP.map(function(image){
               return image.mask(cropland)
            });


/// chnage date format
TEMP = TEMP.map(function(image){
  return image.set('timeFormat', image.date().format('dd-MM-yyyy'))
});

Export.table.toDrive({collection: TEMP, 
                      description: 'TEMP',
                      folder:MyFolder,
                      fileFormat: 'CSV',
                      selectors: ['timeFormat', 'LST_Day_1km']});


print(TEMP)
///=======
//HUMIDITY  1979-01-01T00:00:00 -  2021-08-22
///=======
var dataset = ee.ImageCollection('NOAA/CFSV2/FOR6H')
                 .filterBounds(table)
                .filter(ee.Filter.date(StartDate,EnDate));
 
// GET cropland
               
var HUM = dataset.select('Specific_humidity_height_above_ground')
            .map(function(image){
               return image.mask(cropland)
            });

//// get mean values 
HUM = HUM.map(function(image){
var  dict = image.reduceRegion({reducer: ee.Reducer.median(), geometry: table, scale: 1000, maxPixels: 10e13})
return image.set(dict)
  
});


/// chnage date format
HUM = HUM.map(function(image){
  return image.set('timeFormat', image.date().format('dd-MM-yyyy'))
});


print(HUM)

Export.table.toDrive({collection: HUM, 
                      description: 'HUM',
                      folder:MyFolder,
                      fileFormat: 'CSV',
                      selectors: ['timeFormat', 'Specific_humidity_height_above_ground']});




///=======
//SOLAR  2001-01-01T00:00:00 -  2021-08-22
///=======
var dataset = ee.ImageCollection('NOAA/CFSV2/FOR6H')
                 .filterBounds(table)
                 //.filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', CloudCoverMax))
                .filter(ee.Filter.date(StartDate,EnDate));
 
// GET cropland
               
var SOL = dataset.select('Downward_Short-Wave_Radiation_Flux_surface_6_Hour_Average')
            .map(function(image){
               return image.mask(cropland)
            });

//// get mean values 
SOL = SOL.map(function(image){
var  dict = image.reduceRegion({reducer: ee.Reducer.median(), geometry: table, scale: 1000, maxPixels: 10e13})
return image.set(dict)
  
});


/// chnage date format
SOL = SOL.map(function(image){
  return image.set('timeFormat', image.date().format('dd-MM-yyyy'))
});


print(SOL)
Export.table.toDrive({collection: SOL, 
                      description: 'SOL',
                      folder:MyFolder,
                      fileFormat: 'CSV',
                      selectors: ['timeFormat', 'Downward_Short-Wave_Radiation_Flux_surface_6_Hour_Average']});

///=======
//CLOUDS  2001-01-01T00:00:00 - 2021-08-15
///=======

var dataset =  ee.ImageCollection('NOAA/CDR/PATMOSX/V53')
                 .filterBounds(table)
                 .filterDate(StartDate,EnDate);
 
// GET cropland
               
var CLOUD = dataset.select('cloud_fraction')
            .map(function(image){
               return image.mask(cropland)
            });

//// get mean values 
CLOUD = CLOUD.map(function(image){

var  dict = image.reduceRegion({reducer: ee.Reducer.median(), geometry: table, scale: 1000, maxPixels: 10e13})
return image.set(dict)
  
});

/// chnage date format
CLOUD = CLOUD.map(function(image){
  return image.set('timeFormat', image.date().format('dd-MM-yyyy'))
});

print(CLOUD)

Export.table.toDrive({collection: CLOUD, 
                      description: 'CLOUD',
                      folder:MyFolder,
                      fileFormat: 'CSV',
                      selectors: ['timeFormat', 'cloud_fraction']});



