# GIS scripts for generating tax district polygons

Each county has its own script, written to handle their unique data formats.
All of the scripts were designed to be run *interactively* within ArcGIS Pro
(i.e. copy and paste each chunk of code into the Python console within ArcGIS,
and pay special attention to the comments -- occasionally some manual steps are
required).

Paths to inputs and outputs are hardcoded and will need to be updated in future
years.

Each script assumes the existence of the following folders/geodatabases (the
root folder for these can be anywhere):

* **data/**
  * **assessor/**
    * **AssessorData_*{year}*.gdb** -- geodatabase containing copies of each
      county's `AssessorData_{county}_{year}` table from the Data Depot
  * **output/**
    * **Results.gdb** -- empty geodatabase in which the final polygons are saved
  * **parcels/** -- folder containing copies of each county's
    `Parcels_{county}_{year}.gdb` geodatabase from the Data Depot
  * **tax_codes/** -- folder containing the spreadsheets produced by
    `../2_process_taxcodes.R`
* **proc/**
  * **GIS.gdb** -- an empty geodatabase for storing some intermediate data
  * **proc.gdb** -- an empty "scratch" geodatabase
