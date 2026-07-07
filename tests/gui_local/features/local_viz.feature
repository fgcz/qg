Feature: Local app visualizations — plate map and acquisition timeline
  As a core operator without B-Fabric access
  I want the Visualizations tab to render after generating a plate queue
  So that I can inspect plate layout and acquisition-order balance locally.

  Scenario: Generate a plate queue, then view the plate map and timeline
    Given the local queue app is open
    When I load the bundled plate example
    And I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I set "Queue Type" to "Plate"
    And I set the date to "2026-01-15"
    And I switch to the "Visualizations" tab
    Then the plate layout visualization is visible
    And the plate balance score is shown
    When I switch to the "Acquisition Timeline" tab
    Then the acquisition timeline with its balance score is visible
