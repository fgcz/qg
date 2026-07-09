Feature: The no_layout option (plate as-is) hides the Pattern picker
  As a core operator whose plate is full of biological samples
  I want a "no_layout" QC option that adds no QC and reserves no wells
  So that I can queue the plate exactly as provided, with no Pattern to choose.

  # `no_layout` is a code-level "plate as-is" option (Plate mode, all tech areas):
  # no QC layout, no injections, nothing reserved. It replaces the old placeholder
  # `noqc` QC layout. Selecting it makes the Pattern picker irrelevant, so it hides.

  Scenario: Plate mode offers no_layout and hides the Pattern picker when chosen
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I set "Instrument" to "EXPLORIS_3"
    And I set "Sampler" to "Vanquish"
    And I select order 37195
    And I set "Queue Type" to "Plate"
    Then the "QC Layout" picker offers "no_layout"
    When I set "QC Layout" to "no_layout"
    Then the Pattern picker is hidden
