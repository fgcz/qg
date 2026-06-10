Feature: Local app happy path — upload CSV, generate, download
  As a core operator without B-Fabric access
  I want to upload a sample CSV, configure the queue, and download the output file
  So that I can generate a valid instrument queue without any LIMS dependency.

  Scenario: Upload vial CSV, generate Proteomics Vial queue, download
    Given the local queue app is open
    When I upload the sample file "vial_samples_5x5.csv"
    And I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I set "Queue Type" to "Vial"
    And I set the date to "2026-01-15"
    Then the queue preview is visible
    And the "Download Queue File" link is enabled
