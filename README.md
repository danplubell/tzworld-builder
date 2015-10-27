# tzworld-builder

This package contains a utility for creating a database that is used by tzworld-api

The data is from the tz_world data created by Eric Muller.  The last update of the data was made on November 26, 2013.

You can find the data here: [http://efele.net/maps/tz/world/]()

The data from efele.net is in a GIS shapefile.

The builder uses a geoJson file that is created by GDAL from the tz_world shape file

The geoJson file is parsed and converted into a SQLite database.


# Usage
Build the tzworld.db file by doing the following:
Change directory to the tzworld-builder directory
Issue the following command:
make run

This will convert the tz_world.json file into the tzworld.db file.

# License
The MIT License (MIT)

Copyright (c) 2015 Dan Plubell <danplubell@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

