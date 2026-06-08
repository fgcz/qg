Feature: Starting tray and well prompts follow the queue mode
  As a queue-app user
  I want the starting-well prompt only when running individual vials, while the
  starting-tray prompt is available in both modes
  So that plate-mode operators can relocate their plate off the default tray
  without being shown the irrelevant per-well start choice.

  Scenario: Vial mode asks for a starting tray and well
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37182
    And I set "Queue Type" to "Vial"
    Then the user is prompted for the starting tray
    And the user is prompted for the starting well

  Scenario: Plate mode asks for a starting tray but not a starting well
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    Then the user is prompted for the starting tray
    And the user is not prompted for the starting well
