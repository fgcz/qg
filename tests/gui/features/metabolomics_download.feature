Feature: Metabolomics queue download with polarity expansion
  As a metabolomics core operator
  I want to download a Metabolomics queue that runs each sample in both polarities
  So that I can acquire matched positive and negative spectra in one batch.

  Scenario: Metabolomics queue expands each sample into a pos and neg injection
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I set "Instrument" to "EXPLORIS_3"
    And I set "Sampler" to "Vanquish"
    And I select order 37195
    And I set "Queue Type" to "Plate"
    And I upload to B-Fabric
    Then I download the queue CSV
    And every Metabolomics sample appears in both polarities
