Feature: Show Plate tab — plate-layout visualization
  As a core operator preparing a plate run
  I want to see the generated queue positions laid out on the plate
  So that I can sanity-check well assignments before submitting to the instrument.

  Scenario: Show Plate tab renders the plate layout for a generated queue
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    And I upload to B-Fabric
    And I switch to the "Show Plate" tab
    Then the plate layout visualization is visible
