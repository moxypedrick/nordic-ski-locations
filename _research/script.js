(function(){

var bounds = [
    [-87.010452, 29.537327],
    [-79.771171, 36.210589] 
  ];

mapboxgl.accessToken = 'pk.eyJ1IjoibW94eXBlZCIsImEiOiJjaWgydGpwdmYweHJydnFtMzZzOXpmNjg3In0.5TXWYv0Z7nsOZHneIQOhxg';

var map = new mapboxgl.Map({
    container: 'map',
    style: 'mapbox://styles/moxyped/cjn6fq1br115i2snaywee8zce',
    center: [-103.59179687498357, 40.66995747013945],
    zoom: 3,
    customAttribution: ['<a href=https://atlantaregional.org/ target=_blank>Atlanta Regional Commission</a>','<a href=https://www.epa.gov/eco-research/ecoregions target=_blank>EPA Eco-Regions</a>','<a href=https://hdsc.nws.noaa.gov/hdsc/pfds/pfds_map_cont.html?bkmrk=ga target=_blank>NOAA - Storm Data</a>', '<a href=https://www.epa.gov/watersense/water-budget-data-finder target=_blank>EPA - ETP</a>']
});


map.on('load', function() {
  

  map.addSource('nordic-ski-areas', {
    'type' : 'geojson',
    'data' : 'data/nordic-ski-areas.geojson' ,
    'cluster' : true,
    'clusterMaxZoom' : 14,
    'clusterRadius':50
  });

  map.addLayer({
    'id' : 'clusters',
    'type' : 'circle',
    'source': 'nordic-ski-areas', 
    'filter' : ['has','point_count'],
    'paint':{
      'circle-color': [
        'step',
        ['get', 'point_count'],
        '#51bbd6',
        100,
        '#f1f075',
        750,
        '#f28cb1'
        ],
        'circle-radius': [
        'step',
        ['get', 'point_count'],
        20,
        100,
        30,
        750,
        40
        ]
    }
    
  });

  map.addLayer({
    id: 'cluster-count',
    type: 'symbol',
    source: 'nordic-ski-areas',
    filter: ['has', 'point_count'],
    layout: {
    'text-field': '{point_count_abbreviated}',
    'text-font': ['DIN Offc Pro Medium', 'Arial Unicode MS Bold'],
    'text-size': 12
    }
    });
     
    map.addLayer({
    id: 'unclustered-point',
    type: 'circle',
    source: 'nordic-ski-areas',
    filter: ['!', ['has', 'point_count']],
    paint: {
    'circle-color': '#11b4da',
    'circle-radius': 4,
    'circle-stroke-width': 1,
    'circle-stroke-color': '#fff'
    }
    });

  // inspect a cluster on click
  map.on('click', 'clusters', function (e) {
    var features = map.queryRenderedFeatures(e.point, {
    layers: ['clusters']
    });
    var clusterId = features[0].properties.cluster_id;
    map.getSource('nordic-ski-areas').getClusterExpansionZoom(
    clusterId,
    function (err, zoom) {
    if (err) return;
    
    map.easeTo({
    center: features[0].geometry.coordinates,
    zoom: zoom
    });
    }
    );
    });
  
    map.on('click', 'unclustered-point', function (e) {
      var coordinates = e.features[0].geometry.coordinates.slice();
      //var mag = e.features[0].properties.mag;
      //var tsunami;
      var nordic_name = e.features[0].properties.Name;
      var groomed = e.features[0].properties.Groomed;
      var length = e.features[0].properties.Length;
      var units = e.features[0].properties.Units;
  
       
      console.log(e.features[0].properties);
      while (Math.abs(e.lngLat.lng - coordinates[0]) > 180) {
        coordinates[0] += e.lngLat.lng > coordinates[0] ? 360 : -360;
        }

        new mapboxgl.Popup()
          .setLngLat(coordinates)
          .setHTML(
          'Name: ' + nordic_name + '<br>Grooming: ' + groomed + '<br>Length: ' + length + ' ' + units 
          )
          .addTo(map);
     
      });


      
    map.on('mouseenter', 'clusters', function () {
      map.getCanvas().style.cursor = 'pointer';
      });

    map.on('mouseleave', 'clusters', function () {
      map.getCanvas().style.cursor = '';
      });

      map.on('mouseenter', 'unclustered-point', function () {
        map.getCanvas().style.cursor = 'pointer';
        });
  
      map.on('mouseleave', 'unclustered-point', function () {
        map.getCanvas().style.cursor = '';
        });

});



var draw = new MapboxDraw({
    displayControlsDefault: false,

    controls: {
      point: true,
      polygon: true,
      trash: true
    }
});

map.on('click', 'clusters', function (e) {
  var features = map.queryRenderedFeatures(e.point, {
  layers: ['clusters']
  });
  var clusterId = features[0].properties.cluster_id;
  map.getSource('nordic-ski-areas').getClusterExpansionZoom(
  clusterId,
  function (err, zoom) {
  if (err) return;
   
  map.easeTo({
  center: features[0].geometry.coordinates,
  zoom: zoom
  });
  }
  );
  });

map.addControl(new MapboxGeocoder({
    accessToken: mapboxgl.accessToken
}), 'top-right');


var scale = new mapboxgl.ScaleControl({
    maxWidth: 100,
    unit: 'imperial'
});
map.addControl(scale, 'bottom-right');
scale.setUnit('imperial');
map.addControl(new mapboxgl.NavigationControl(), 'top-right');
map.addControl(draw, 'top-right');



})();
