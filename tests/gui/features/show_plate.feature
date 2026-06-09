Feature: Visualizations tab — plate layout & acquisition timeline
  As a core operator preparing a run
  I want to see the generated queue laid out on the plate and along acquisition order
  So that I can sanity-check well assignments and run balance before submitting.

  Scenario: Visualizations tab renders the plate layout, score, and timeline
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    And I upload to B-Fabric
    And I switch to the "Visualizations" tab
    Then the plate layout visualization is visible
    And the plate balance score is shown
    When I switch to the "Acquisition Timeline" tab
    Then the acquisition timeline with its balance score is visible
