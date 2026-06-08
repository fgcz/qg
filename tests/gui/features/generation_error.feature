Feature: Generation error surfaces a danger callout
  As a core operator who has selected a misconfigured combination
  I want a clear, prominent error in the queue preview
  So that I notice the failure before exporting an invalid queue.

  Scenario: More plates than the sampler has trays raises a danger callout
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37200
    And I set "Queue Type" to "Plate"
    Then a danger callout reports the generation error
