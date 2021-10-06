CPR Data Provenance
================
Adam A. Kemberling
2021-10-06

<style type="text/css">/********** GMRI Rmarkdown Core Style Sheet - Do Not Modify!!! **********/


/********** Begin Style Sheet **********/

/* Avenir Font from Fonts.com for GMRI Branding */

@import url("http://fast.fonts.net/t/1.css?apiType=css&projectid=806f61f6-d695-4965-a878-820b50bc0269");
    @font-face{
        font-family:"Avenir LT W01_35 Light1475496";
        src:url("Fonts/0078f486-8e52-42c0-ad81-3c8d3d43f48e.woff2") format("woff2"),url("Fonts/908c4810-64db-4b46-bb8e-823eb41f68c0.woff") format("woff");
    }
    @font-face{
        font-family:"Avenir LT W01_35 Light_1475502";
        src:url("Fonts/a59168c1-917d-4de9-a244-0316c057c357.woff2") format("woff2"),url("Fonts/6dc0e7d8-9284-44e1-8f05-984a41daa3a4.woff") format("woff");
    }
    @font-face{
        font-family:"Avenir LT W01_45 Book1475508";
        src:url("Fonts/065a6b14-b2cc-446e-9428-271c570df0d9.woff2") format("woff2"),url("Fonts/65d75eb0-2601-4da5-a9a4-9ee67a470a59.woff") format("woff");
    }
    @font-face{
        font-family:"Avenir LT W01_45 Book O1475514";
        src:url("Fonts/476612d9-282d-4f76-95cd-b4dd31e7ed21.woff2") format("woff2"),url("Fonts/f1ebae2b-5296-4244-8771-5f40e60a564a.woff") format("woff");
    }
    @font-face{
        font-family:"Avenir LT W01_55 Roman1475520";
        src:url("Fonts/b290e775-e0f9-4980-914b-a4c32a5e3e36.woff2") format("woff2"),url("Fonts/4b978f72-bb48-46c3-909a-2a8cd2f8819c.woff") format("woff");
    }
    @font-face{
        font-family:"Avenir LT W01_55 Obliqu1475526";
        src:url("Fonts/1a7173fa-062b-49ad-9915-bc57d3bfc1f5.woff2") format("woff2"),url("Fonts/cdda031e-26e9-4269-83d1-5a218caa10db.woff") format("woff");
    }
    @font-face{
        font-family:"Avenir LT W01_65 Medium1475532";
        src:url("Fonts/17b90ef5-b63f-457b-a981-503bb7afe3c0.woff2") format("woff2"),url("Fonts/c9aeeabd-dd65-491d-b4be-3e0db9ae47a0.woff") format("woff");
    }
    @font-face{
        font-family:"Avenir LT W01_65 Medium1475538";
        src:url("Fonts/deb5e718-7abb-4df3-9365-edfa95317090.woff2") format("woff2"),url("Fonts/04801919-17ee-4c6b-8b17-eb1965cb3ed6.woff") format("woff");
    }
    @font-face{
        font-family:"Avenir LT W01_85 Heavy1475544";
        src:url("Fonts/d513e15e-8f35-4129-ad05-481815e52625.woff2") format("woff2"),url("Fonts/61bd362e-7162-46bd-b67e-28f366c4afbe.woff") format("woff");
    }
    @font-face{
        font-family:"Avenir LT W01_85 Heavy_1475550";
        src:url("Fonts/3c210c80-960f-4684-850b-25390b4d08af.woff2") format("woff2"),url("Fonts/cb5c71ad-e582-4d00-929c-67fbfaeb1c27.woff") format("woff");
    }
    @font-face{
        font-family:"Avenir LT W01_95 Black1475556";
        src:url("Fonts/c78eb7af-a1c8-4892-974b-52379646fef4.woff2") format("woff2"),url("Fonts/75b36c58-2a02-4057-a537-09af0832ae46.woff") format("woff");
    }
    @font-face{
        font-family:"Avenir LT W01_95 Black_1475562";
        src:url("Fonts/a2477e08-09d9-4d4b-97a9-23a1e22cb44c.woff2") format("woff2"),url("Fonts/19d12bba-92b1-43ad-9bab-cd36a4195c2a.woff") format("woff");
    }





/* PRE-Avenir Fonts: Lato + Raleway font import from google fonts */
@import url('https://fonts.googleapis.com/css?family=Lato');
@import url('https://fonts.googleapis.com/css?family=Raleway&display=swap');

/* add font families as needed: font-family: 'Lato', sans-serif; */


/* Level 1 Headers */
h1 { text-align: left;
     margin: 10px 0 15px 0; 
     font-size: 38px; 
     font-family: Avenir;
} 


/* Headers 2 - 6 */
h2, h3, h4, h5, h6 { 
    color: #333333; 
    margin: 20px 0 5px 0; 
    text-align: left;
    font-family: Avenir;}


/* Sizing/font For Each Header Type */
h2 { font-size: 24px; }
h3 { font-size: 20px; }
h4 { font-size: 18px; }
h5 { font-size: 16px; font-weight: normal; color: #3069aa; text-decoration: underline;}
h6 { font-size: 14px; font-weight: normal; color: #3069aa; }


/* Paragraph Text */
p, ol { margin-top: 10px; 
    font-family: 'Raleway', sans-serif;}


/* Title Author and Date Headers */
h1.title.toc-ignore {margin-top: 10px;}
h4.author, h4.date {
    color: rgb(0,115,109);
    margin-top: 0px; 
    margin-bottom: 5px; 
    font-size: 12px;}



/* Add Spacing Above Headers */
h1, .h1, h2, .h2, h3, .h3 {
    margin-top: 40px;
}


/* Links */
a {
    color: rgb(234,79,18)
}


/***********************************************/


/********  Table of Contents  **********/

/* Highlighted TOC Element */
.list-group-item.active, .list-group-item.active:focus, .list-group-item.active:hover {
    z-index: 2;
    color: #fff;
    background-color: rgb(0,96,138);
    border-color: rgb(0,96,138);
}

/* Default TOC Elements */
.list-group-item, .list-group-item:focus, .list-group-item:hover {
    z-index: 2;
    color: rgb(0,96,138);
    background-color: #fff;
    border-color: rgb(0,96,138);
}


/********  Tab Panels  **********/

/* Navigation Tabs - Highlighted Tabset Pills */
.nav-pills > li.active > a, .nav-pills > li.active > a:hover, .nav-pills > li.active > a:focus {
    color: #fff;
    background-color: rgb(0,115,109) ;
    }

/* Navigation Tabs - Default Tabset Pills */
.nav-pills > li > a, .nav-pills > li > a:hover, .nav-pills > li > a:focus {
    color: rgb(0,115,109);
    background-color: #fff;
    }


/* Second Level Tabs - Active */
.nav.nav-tabs > li.active  > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
    color: #fff;
    background-color: rgb(83,83,83) ;
    }

/* Second Level Tabs - Inactive */
.nav.nav-tabs > li  > a, .nav-tabs > li > a:hover, .nav-tabs > li > a:focus {
    color: rgb(83,83,83);
    background-color: #fff;
    }



/********** End Core Style Sheet **********/
/********** End Core Style Sheet **********/



</style>

# CPR Data Provenance

The data for the continuous plankton recorder survey (CPR) has been
collected and maintained by multiple government and non-government
organizations. The methods for data collection and sampling processes
have remained consistent, however the data processing/storage has
revealed some differences across research entities.

This documentation seeks to document the different data available to us
here, and the data contained within each starting point. Once the
starting points are clear, the data processing steps will be detailed to
provide clarity on how these different resources can be used together.

All final processing code has been moved to the {targets} pipeline, with
all processing steps written as functions in:  
&gt; **R/support/gom\_cpr\_pipeline\_support.R**

# Starting Points:

There are two transects of the CPR survey that we have access to the
data for. The Gulf of Maine and the Mid-Atlantic transects.

## Gulf of Maine Transect

Data for the Guf of Maine transect was transferred to us from two
sources. The Sir Alister Hardy Foundation, and the Northeast Fisheries
Science Center.

The formatting of the original data files from each source is as
follows:

### Single Taxa Files:

**Root File Location**

CPR data on individual taxa were delivered through email and placed on
box in the following directory on box. All derived data from these
starting files are under this directory. This is not a permanent home,
but is the location the `cpr_boxpath` refers to in the code:

> **Box/Adam Kemberling/Box\_Projects/continuous\_plankton\_recorder**

When initially engaging with the CPR data, requests for CPR data on
specific taxa were requested and received. These came in two groups that
contain both annual averages and some within-year averages (bi-monthly
or quarterly).

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#jyoswehzsx .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#jyoswehzsx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jyoswehzsx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#jyoswehzsx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#jyoswehzsx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jyoswehzsx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jyoswehzsx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#jyoswehzsx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#jyoswehzsx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jyoswehzsx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jyoswehzsx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#jyoswehzsx .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#jyoswehzsx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#jyoswehzsx .gt_from_md > :first-child {
  margin-top: 0;
}

#jyoswehzsx .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jyoswehzsx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#jyoswehzsx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#jyoswehzsx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jyoswehzsx .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#jyoswehzsx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jyoswehzsx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jyoswehzsx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jyoswehzsx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jyoswehzsx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jyoswehzsx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#jyoswehzsx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jyoswehzsx .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#jyoswehzsx .gt_left {
  text-align: left;
}

#jyoswehzsx .gt_center {
  text-align: center;
}

#jyoswehzsx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jyoswehzsx .gt_font_normal {
  font-weight: normal;
}

#jyoswehzsx .gt_font_bold {
  font-weight: bold;
}

#jyoswehzsx .gt_font_italic {
  font-style: italic;
}

#jyoswehzsx .gt_super {
  font-size: 65%;
}

#jyoswehzsx .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="jyoswehzsx" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Folder</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">File Name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Description</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Temporal Frequency</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX</td>
      <td class="gt_row gt_left">GOMx.Calanus_finmarchicus.txt</td>
      <td class="gt_row gt_left">Late stage C. Finmarchicus densities</td>
      <td class="gt_row gt_left">Quarterly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX</td>
      <td class="gt_row gt_left">GOMx.Calanus_I-IV.txt</td>
      <td class="gt_row gt_left">Early-stage C. finmarchicus densities</td>
      <td class="gt_row gt_left">Quarterly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX</td>
      <td class="gt_row gt_left">GOMx.Centropages_typicus.txt</td>
      <td class="gt_row gt_left">Centropages typicus densities</td>
      <td class="gt_row gt_left">Quarterly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX</td>
      <td class="gt_row gt_left">GOMx.Chaetognatha_eyecount.txt</td>
      <td class="gt_row gt_left">Chaetognatha spp. densities</td>
      <td class="gt_row gt_left">Quarterly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX</td>
      <td class="gt_row gt_left">GOMx.Euphausiacea_Total.txt</td>
      <td class="gt_row gt_left">Euphausiacea densities</td>
      <td class="gt_row gt_left">Quarterly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX</td>
      <td class="gt_row gt_left">GOMx.Metridia_lucens.txt</td>
      <td class="gt_row gt_left">Metridia lucens densities</td>
      <td class="gt_row gt_left">Quarterly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX</td>
      <td class="gt_row gt_left">GOMx.Oithona_spp..txt</td>
      <td class="gt_row gt_left">Oithona spp. densities</td>
      <td class="gt_row gt_left">Quarterly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX</td>
      <td class="gt_row gt_left">GOMx.Para-Pseudocalanus_spp..txt</td>
      <td class="gt_row gt_left">Paracalanus &amp; Pseudocalanus densities</td>
      <td class="gt_row gt_left">Quarterly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX</td>
      <td class="gt_row gt_left">GOMx.Paraeuchaeta_norvegica.txt</td>
      <td class="gt_row gt_left">Paraeuchaeta norvegica densities</td>
      <td class="gt_row gt_left">Quarterly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX</td>
      <td class="gt_row gt_left">GOMx.Temora_longicornis.txt</td>
      <td class="gt_row gt_left">Temora longicornis densities</td>
      <td class="gt_row gt_left">Quarterly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX6</td>
      <td class="gt_row gt_left">Calanus_finmarchicus.txt</td>
      <td class="gt_row gt_left">Late stage C. Finmarchicus densities</td>
      <td class="gt_row gt_left">Bi-monthly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX6</td>
      <td class="gt_row gt_left">Calanus_I-IV.txt</td>
      <td class="gt_row gt_left">Early-stage C. finmarchicus densities</td>
      <td class="gt_row gt_left">Bi-monthly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX6</td>
      <td class="gt_row gt_left">Centropages_typicus.txt</td>
      <td class="gt_row gt_left">Centropages typicus densities</td>
      <td class="gt_row gt_left">Bi-monthly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX6</td>
      <td class="gt_row gt_left">Chaetognatha_eyecount.txt</td>
      <td class="gt_row gt_left">Chaetognatha spp. densities</td>
      <td class="gt_row gt_left">Bi-monthly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX6</td>
      <td class="gt_row gt_left">Euphausiacea_Total.txt</td>
      <td class="gt_row gt_left">Euphausiacea densities</td>
      <td class="gt_row gt_left">Bi-monthly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX6</td>
      <td class="gt_row gt_left">Metridia_lucens.txt</td>
      <td class="gt_row gt_left">Metridia lucens densities</td>
      <td class="gt_row gt_left">Bi-monthly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX6</td>
      <td class="gt_row gt_left">Oithona_spp..txt</td>
      <td class="gt_row gt_left">Oithona spp. densities</td>
      <td class="gt_row gt_left">Bi-monthly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX6</td>
      <td class="gt_row gt_left">Para-Pseudocalanus_spp..txt</td>
      <td class="gt_row gt_left">Paracalanus &amp; Pseudocalanus densities</td>
      <td class="gt_row gt_left">Bi-monthly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX6</td>
      <td class="gt_row gt_left">Paraeuchaeta_norvegica.txt</td>
      <td class="gt_row gt_left">Paraeuchaeta norvegica densities</td>
      <td class="gt_row gt_left">Bi-monthly periods</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPRtimeseries_textX6</td>
      <td class="gt_row gt_left">Temora_longicornis.txt</td>
      <td class="gt_row gt_left">Temora longicornis densities</td>
      <td class="gt_row gt_left">Bi-monthly periods</td>
    </tr>
  </tbody>
  
  
</table></div>

These collections are each processed into two additional files, one for
the yearly densities and the second for quarterly densities. These two
intermediate/processed files are:

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ddwapelmdj .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ddwapelmdj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ddwapelmdj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ddwapelmdj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ddwapelmdj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ddwapelmdj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ddwapelmdj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ddwapelmdj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ddwapelmdj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ddwapelmdj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ddwapelmdj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ddwapelmdj .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ddwapelmdj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ddwapelmdj .gt_from_md > :first-child {
  margin-top: 0;
}

#ddwapelmdj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ddwapelmdj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ddwapelmdj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ddwapelmdj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ddwapelmdj .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ddwapelmdj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ddwapelmdj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ddwapelmdj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ddwapelmdj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ddwapelmdj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ddwapelmdj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ddwapelmdj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ddwapelmdj .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ddwapelmdj .gt_left {
  text-align: left;
}

#ddwapelmdj .gt_center {
  text-align: center;
}

#ddwapelmdj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ddwapelmdj .gt_font_normal {
  font-weight: normal;
}

#ddwapelmdj .gt_font_bold {
  font-weight: bold;
}

#ddwapelmdj .gt_font_italic {
  font-style: italic;
}

#ddwapelmdj .gt_super {
  font-size: 65%;
}

#ddwapelmdj .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="ddwapelmdj" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Folder</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">File Name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Description</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">processed_data</td>
      <td class="gt_row gt_left">cpr_allspecies_long.csv</td>
      <td class="gt_row gt_left">reshaping of bi-monthly single species taxa</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">processed_data</td>
      <td class="gt_row gt_left">cpr_allspecies_long_quarters.csv</td>
      <td class="gt_row gt_left">reshaping of quarterly single species taxa</td>
    </tr>
  </tbody>
  
  
</table></div>

# Starting Points for All Taxa

For questions relating to the full zooplankton community we also
requested and received data on all taxa collected as part of the CPR
survey as well as the phytoplankton color index (PCI). These were
sourced from the two different management entities, NEFSC and SAHFOS,
which were responsible for data collection and management for different
periods of time.

These files were placed within the Climate Change Ecology Lab directory
on Box, referenced as `ccel_boxpath` in the code:

> **Box/Climate Change Ecology Lab**

## NOAA/NEFSC Starting Points

Data obtained from NOAA/NEFSC spans the period of: **1961-2013**, and
was delivered in the following excel file, that is divided into **2**
sheets:

> **Data/NOAA\_1961-2013/Gulf of Maine CPR (Feb 14, 2014 update).xlsx**

Of the two sheets, one contains data on phytoplankton and the other
contains records on zooplankton.

Each excel sheet has a non-standard layout with two or more additional
rows in the header explaining the identification and development stage
of certain taxa. In the R script `13_allspecies_cpr_cleanup.R` these
headers are stripped, and the different taxa and their stages are
appended as the new column names.

Taxonomic codes are also checked against MARMAP codes to ensure accuracy
<https://www.nefsc.noaa.gov/nefsc/Narragansett/taxcodesA.html>

Through email correspondence it was communicated that the units were
abundance / 100 meters cubed.

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#vorjzzcbed .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#vorjzzcbed .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vorjzzcbed .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#vorjzzcbed .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#vorjzzcbed .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vorjzzcbed .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vorjzzcbed .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#vorjzzcbed .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#vorjzzcbed .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vorjzzcbed .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vorjzzcbed .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#vorjzzcbed .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#vorjzzcbed .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#vorjzzcbed .gt_from_md > :first-child {
  margin-top: 0;
}

#vorjzzcbed .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vorjzzcbed .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#vorjzzcbed .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#vorjzzcbed .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vorjzzcbed .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#vorjzzcbed .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vorjzzcbed .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vorjzzcbed .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vorjzzcbed .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vorjzzcbed .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vorjzzcbed .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#vorjzzcbed .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vorjzzcbed .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#vorjzzcbed .gt_left {
  text-align: left;
}

#vorjzzcbed .gt_center {
  text-align: center;
}

#vorjzzcbed .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vorjzzcbed .gt_font_normal {
  font-weight: normal;
}

#vorjzzcbed .gt_font_bold {
  font-weight: bold;
}

#vorjzzcbed .gt_font_italic {
  font-style: italic;
}

#vorjzzcbed .gt_super {
  font-size: 65%;
}

#vorjzzcbed .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="vorjzzcbed" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Sheet Number</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Description</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Units</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">Sheet 1</td>
      <td class="gt_row gt_left">Phytoplankton taxa densities by silk transect</td>
      <td class="gt_row gt_left"># / 100m3</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Sheet 2</td>
      <td class="gt_row gt_left">Zooplankton taxa densities by silk transect</td>
      <td class="gt_row gt_left"># / 100m3</td>
    </tr>
  </tbody>
  
  
</table></div>

At the end of the reshaping of these NOAA sheets, a pair of files are
exported for both the phytoplankton and the zooplankton sets:

## SAHFOS Starting Points

Data obtained from SAHFOS spans the period of: **2013-2017**, and was
delivered in the following files:

> **Gulf of Maine CPR/SAHFOS-MBA\_2013-2017/MC part1.xlsx**  
> **Gulf of Maine CPR/SAHFOS-MBA\_2013-2017/MC part 2.xlsx**

Each of these files contain three sheets containing the different
counting scales of the CPR survey, the phytoplankton, the traverse, and
the eyecount scales. These three scales are based on the size of
organisms counted, and have different amounts of the transect used when
counting, which are then scaled to the entire silk transect.

These three measurement increments correspond with the following
sub-sampling protocols to save time when counting very small organisms:

1.  **Phyto** - 1/8000th of transect counted  
2.  **Traverse** - 1/40th of transect counted  
3.  **Eyecount** - full transect counted

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#qiuaqoaecp .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#qiuaqoaecp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qiuaqoaecp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#qiuaqoaecp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#qiuaqoaecp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qiuaqoaecp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qiuaqoaecp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#qiuaqoaecp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#qiuaqoaecp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#qiuaqoaecp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#qiuaqoaecp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#qiuaqoaecp .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#qiuaqoaecp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#qiuaqoaecp .gt_from_md > :first-child {
  margin-top: 0;
}

#qiuaqoaecp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#qiuaqoaecp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#qiuaqoaecp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#qiuaqoaecp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qiuaqoaecp .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#qiuaqoaecp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qiuaqoaecp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#qiuaqoaecp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#qiuaqoaecp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qiuaqoaecp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qiuaqoaecp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#qiuaqoaecp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qiuaqoaecp .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#qiuaqoaecp .gt_left {
  text-align: left;
}

#qiuaqoaecp .gt_center {
  text-align: center;
}

#qiuaqoaecp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qiuaqoaecp .gt_font_normal {
  font-weight: normal;
}

#qiuaqoaecp .gt_font_bold {
  font-weight: bold;
}

#qiuaqoaecp .gt_font_italic {
  font-style: italic;
}

#qiuaqoaecp .gt_super {
  font-size: 65%;
}

#qiuaqoaecp .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="qiuaqoaecp" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Sheet Number</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Description</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Units</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">Sheet 1</td>
      <td class="gt_row gt_left">Phytoplankton taxa densities by silk transect</td>
      <td class="gt_row gt_left">?</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Sheet 2</td>
      <td class="gt_row gt_left">Zooplankton taxa densities by silk transect</td>
      <td class="gt_row gt_left">?</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Sheet 3</td>
      <td class="gt_row gt_left">Eyecount taxa densities by silk transect</td>
      <td class="gt_row gt_left">?</td>
    </tr>
  </tbody>
  
  
</table></div>

At the end of the reshaping of these SAHFOS sheets, a file for each
measurement scale and a single taxon key are exported for both the MC1
and MC2 excel sheet sets:

## Combining NOAA/SAHFOS

The steps taken to join files that began as the NOAA starting points and
the counterparts that relate to the SAHFOS starting points involves
finding parsimony among the different column names that are a
combination of **Taxa** and **Development Stage**.

Across the two sources different groupings have been used that sometimes
overlap and include some or all of another group. Example: Calanus 1-3 &
Calanus 1-4 or Calanus 4-6…

Before these tables can be appended together these groupings need to be
sorted out. This is done using the following two scripts which were
later written as independent function steps for the targets pipeline:  
&gt; **15\_NOAA\_CPR\_Cleanup.R**  
&gt; **16\_SAHFOS\_CPR\_Cleanup.R**

For the SAHFOS data, it was determined that the abundances recorded are
in \#/transect and not \#/100*m*<sup>3</sup> as recorded in the NOAA
data. These values are converted in this step to the **common unit of
density in 100 cubic meters of water**.

To convert from the volume of water covered by a silk transect, the
following constants are used:

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#tznewzhmqj .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#tznewzhmqj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tznewzhmqj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#tznewzhmqj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#tznewzhmqj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tznewzhmqj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tznewzhmqj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#tznewzhmqj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#tznewzhmqj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#tznewzhmqj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#tznewzhmqj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#tznewzhmqj .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#tznewzhmqj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#tznewzhmqj .gt_from_md > :first-child {
  margin-top: 0;
}

#tznewzhmqj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#tznewzhmqj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#tznewzhmqj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#tznewzhmqj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tznewzhmqj .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#tznewzhmqj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tznewzhmqj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#tznewzhmqj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#tznewzhmqj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tznewzhmqj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tznewzhmqj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#tznewzhmqj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tznewzhmqj .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#tznewzhmqj .gt_left {
  text-align: left;
}

#tznewzhmqj .gt_center {
  text-align: center;
}

#tznewzhmqj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#tznewzhmqj .gt_font_normal {
  font-weight: normal;
}

#tznewzhmqj .gt_font_bold {
  font-weight: bold;
}

#tznewzhmqj .gt_font_italic {
  font-style: italic;
}

#tznewzhmqj .gt_super {
  font-size: 65%;
}

#tznewzhmqj .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="tznewzhmqj" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Constant</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">Distance Traveled by CPR Device for Transect</td>
      <td class="gt_row gt_left">10 Nautical Miles</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPR Opening Aperture Size</td>
      <td class="gt_row gt_left">1.27 square cm</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">CPR Aperture Size (square meters)</td>
      <td class="gt_row gt_left">0.0016129</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Meters in 10 Nautical Miles</td>
      <td class="gt_row gt_left">18520</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Volume Sampled in 10 Nautical Mile Transect</td>
      <td class="gt_row gt_left">2.987091 cubic meters</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Transect to 100m3 Equation</td>
      <td class="gt_row gt_left">(1 / 2.987091) * 100</td>
    </tr>
  </tbody>
  
  
</table></div>

Once data from these two sources have been converted to common units of
measurement and common taxonomic groupings they are able to be appended
for one consistent timeseries.

------------------------------------------------------------------------

# Mid-Atlantic Transect

The Mid-Atlantic transect is currently not processed to the extent of
the Gulf of Maine data.
