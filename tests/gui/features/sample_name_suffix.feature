Feature: Batch-appending a suffix to sample names
  As a core operator preparing a queue for a specific prep type
  I want to append a controlled suffix to every sample name in one action
  So that I can tag a whole queue as enriched/total/lip without renaming each row.

  Scenario: Appending a suffix renames every user sample but not QC injections
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    And I append the "enriched" sample name suffix
    And I upload to B-Fabric
    Then I download the queue CSV
    And every user sample name ends with "_enriched"
    And no sample name ends with "_enriched_enriched"
    And QC sample names are unchanged
