Feature: Lipidomics queue with polarity expansion
  As a lipidomics core operator
  I want to download a Lipidomics queue that runs each sample in both polarities
  So that I can acquire matched positive and negative spectra in one batch.

  Scenario: Lipidomics queue expands each sample into a pos and neg injection
    Given the queue app is open as an employee
    When I set "Tech Area" to "Lipidomics"
    And I set "Instrument" to "EXPLORIS_3"
    And I set "Sampler" to "Vanquish"
    And I select order 37190
    And I set "Queue Type" to "Plate"
    And I upload to B-Fabric
    Then I download the queue CSV
    And every Lipidomics sample appears in both polarities
