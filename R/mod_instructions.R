#' instructions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_instructions_ui <- function(id) {
  ns <- NS(id)

  bslib::page_fixed(

    h3("Using this Calculator"),
    HTML("This calculator will produce a table of aliquot volume values, a hydrograph, and, if pollutant data is provided, pollutograph(s) for the given data set of flow rate measurements and sample timestamps of <strong>a single storm event</strong>."),
    HTML("
        <ol>
          <li>
            Download the Excel template file ",), downloadLink("download_template2", label = "here"), HTML(" and overwrite it with your data. See the Data Requirements section below. <br><strong>NOTE</strong>: a 'download.htm' file may be downloaded instead of the template Excel file if the link is clicked too soon after launching the application. This is a known issue with the 'shiny' R package which was used to develop this application. Please allow a few minutes before downloading the template.
          </li>
          <li>
            Upload your data by clicking the 'Browse' button, selecting the updated Excel spreadsheet, and clicking the 'Submit' button. The calculator will generate the aliquot volume table as well as the hydrograph and pollutograph(s), depending on the uploaded data. If pollutant data is provided, the calculator will also provide the Event Mean Concentration for each of the specified pollutants.
          </li>
          <li>
            Use the 'Start Date/Time' and 'End Date/Time' inputs to filter the data to the appropriate time range. The grayed-out sections of the graph will <i>not</i> be included in the aliquot volume and event mean concentration calculations.
          </li>
          <li>
            After changing the 'Start Date/Time' and 'End Date/Time' inputs, click the 'Draw Graph(s)' button to regenerate the aliquot volume table, hydrograph, and pollutograph(s), filtered to the provided times.
          </li>
          <li>
            The 'Composite Vol.' input is used in the aliquot volume calculation such that the sum of the aliquot volumes will be equal to the composite volume value entered here, measured in mL. The minimum and maximum supported values are 500 mL and 10,000 mL, respectively.
          </li>
          <li>
            The 'Flow Units of Submitted Data' input is used to label and calculate the 'Total Hydrograph Volume' output. This input does not function as a unit conversion.
          </li>
          <li>
            Use the 'Reload App' button to submit a new data set.
          </li>
        </ol>
           "),
    hr(),
    h3("Data Requirements"),
    HTML("The uploaded Excel spreadsheet must conform to the following requirements:
        <ul>
          <li>
            Must contain data for exactly one storm event.
          </li>
          <li>
            Must contain exactly three sheets, in the following order:
          </li>
          <ul>
            <li>
              Instructions: instructions for using the calculator
            </li>
            <li>
              Sheet 1: flow rate measurement data
            </li>
            <li>
              Sheet 2: sample collection timestamps and pollutant measurement data (where applicable)
            </li>
          </ul>
          <li>
            The flow rate measurement data sheet (Sheet 1) must have exactly two columns:
          </li>
          <ul>
            <li>
              Col 1: timestamps in 'mm/dd/yy hh:mm:ss' format. Date and time must be provided.
            </li>
            <ul>
              <li>
                The 'Datetime' column in the provided template file is already in the correct format.
              </li>
            </ul>
            <li>
              Col 2: flow rate measurements. <strong>Flow rates must be entered as L/s, gpm, or cfs.</strong>
            </li>
          </ul>
          <li>
            The sample collection timestamps and pollutant measurement sheet (Sheet 2) may have any number of columns:
          </li>
          <ul>
            <li>
              Col 1: timestamps when water quality samples were collected in 'mm/dd/yy hh:mm:ss' format. Date and time must be provided.
            </li>
            <ul>
              <li>
                The 'Datetime' column in the provided template file is already in the correct format.
              </li>
            </ul>
            <li>
              Col 2...n: pollutant concentrations, if/where available
            </li>
            <li>
              Any number of pollutant columns in the second sheet are supported.
            </li>
            <li>
              <strong>If you do not have pollutant data, delete the 'Pollutant' columns entirely before uploading the template. Do not delete Sheet 2.</strong>
            </li>
          </ul>
          <li>
            The column headers are required and can be renamed as needed, but cannot be exclusively numeric characters [0-9].
          </li>
          <li>
            All flow rate and pollutant measurements must be greater than or equal to zero.
          </li>
          <li>
            There may not be any missing values in the spreadsheet.
          </li>
        </ul>")
  )
}

#' instructions Server Functions
#'
#' @noRd
mod_instructions_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_instructions_ui("instructions_1")

## To be copied in the server
# mod_instructions_server("instructions_1")
