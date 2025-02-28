//Adding to GEE the powiat sample shapefile
var powiat = ee.FeatureCollection('projects/ee-ynryara-cpsys/assets/base_clip_wgs84');

var runoff_month = ee.FeatureCollection([]);

var diario= 31;
var mes= 1;
var year= '2025';
var fechaIni= '';
var fechaFin= '';

var dia= ['', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31'];
var meses= ['', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'];

for (var i = 1; i <= diario; i++) {
  fechaIni= year + '-' + meses[mes] + '-' + dia[i];
  if (dia[i]== diario){
    fechaFin= year + '-' + meses[mes+1] + '-' + dia[1];
    if (meses[mes]== '12') {
      var fechaFin = ee.Number.parse(year).add(1).format('%d').cat('-').cat(meses[1]).cat('-').cat(dia[1]);
    };
  } else {
    fechaFin= year + '-' + meses[mes] + '-' + dia[i+1];    
  };
  
//Charging the imagery service selecting only the superficial runoff band
  var runoff = ee.ImageCollection('ECMWF/ERA5_LAND/DAILY_AGGR')
    .filterDate(fechaIni, fechaFin) 
                .select('surface_runoff_sum')
                .filterBounds(powiat)
                .first()
                .clip(powiat);
  
  var runoffValue = runoff.sampleRegions({
    collection: powiat, 
    geometries: true
  });
  
  var latLonRunoff = runoffValue.map(function(feature) {
    var coordinates = feature.geometry().coordinates(); // Get the coordinates of the geometry
    var lat = coordinates.get(1);  // Latitude
    var lon = coordinates.get(0);  // Longitude
    var runoffSum = feature.get('surface_runoff_sum'); // Get the sum of the runoff
    var date= year + meses[mes] + dia[i]
    return feature.set({'latitude': lat, 'longitude': lon, 'runoff_sum': runoffSum, 'date': date}); // Set new properties
  });
  
  runoff_month=runoff_month.merge(latLonRunoff);
  console.log("done for " + fechaIni)
};

Export.table.toDrive({
  collection: runoff_month, 
  description: "RO_"+ year + meses[mes],
  fileFormat: "CSV",
  selectors: ['latitude', 'longitude', 'surface_runoff_sum', 'date'],
  folder: 'RO_points'
});