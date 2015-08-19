# tzworld-builder

This package contains a utility for creating a database that is used by tzworld-api

The data is from the tz_world data created by Eric Muller.  The last update of the data was made on November 26, 2013.

You can find the data here: [http://efele.net/maps/tz/world/]()

The data from efele.net is in a GIS shapefile.

The builder uses a geoJson file that is created by GDAL from the tz_world shape file

The geoJson file is parsed and converted into a SQLite database.


