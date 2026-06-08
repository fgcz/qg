Feature: Plate-mode tray-capacity validation
  As a queue app user
  I want the app to warn me when I select more plates than the sampler has trays
  So that I can fix the selection before generating the queue.

  Scenario: Single plate on a multi-tray sampler does not warn about tray capacity
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    Then the validation panel does not warn about tray capacity

  Scenario: Selecting more plates than the sampler holds warns about tray capacity
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37200
    And I set "Queue Type" to "Plate"
    Then the validation panel warns about tray capacity
