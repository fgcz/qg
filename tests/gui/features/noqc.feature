Feature: The no_layout option (queue as-is) hides the Pattern picker
  As a core operator whose plate or vial set is full of biological samples
  I want a "no_layout" QC option that adds no QC and reserves no wells
  So that I can queue the samples exactly as provided, with no Pattern to choose.

  # `no_layout` is a code-level "as-is" option: no QC layout, no injections, nothing
  # reserved. It is offered for both Plate and Vial queues, but only for tech areas
  # that opt in via tech_area_defaults.allow_no_layout — Proteomics opts out.
  # Selecting it makes the Pattern picker irrelevant, so it hides.

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

  Scenario: Vial mode also offers no_layout and hides the Pattern picker when chosen
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I set "Instrument" to "EXPLORIS_3"
    And I set "Sampler" to "Vanquish"
    And I select order 37196
    And I set "Queue Type" to "Vial"
    Then the "QC Layout" picker offers "no_layout"
    When I set "QC Layout" to "no_layout"
    Then the Pattern picker is hidden

  Scenario: Proteomics does not offer no_layout
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    Then the "QC Layout" picker does not offer "no_layout"
