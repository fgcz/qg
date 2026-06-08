Feature: QC Positions sidebar shows per-well visit counts
  As a core operator inspecting where QC injections land
  I want the QC Positions table to count how many times each well is visited
  So that I can spot wells that are over- or under-used by the chosen pattern.

  Scenario: Generated queue annotates each QC well with its visit count
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    Then the QC Positions sidebar table shows a visits column
    And at least one well has a positive visit count
