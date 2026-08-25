Feature: Queue Type offered follows the selected B-Fabric sample source
  As a core operator configuring a queue
  I want the Queue Type choice to reflect what the selected source produces
  So that Plate and Vial choices are not hidden by unrelated GUI selections.

  # Order items preserve plate/vial placement. All container samples are exposed
  # as Vial. The selected sampler does not alter these choices.

  Scenario: A plate-only order can only be run as a Plate queue
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    Then the "Queue Type" picker offers "Plate"
    And the "Queue Type" picker does not offer "Vial"

  Scenario: A vial-only order can only be run as a Vial queue
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37182
    Then the "Queue Type" picker offers "Vial"
    And the "Queue Type" picker does not offer "Plate"

  # 37183 is a Metabolomics container so that adding it does not push the
  # Proteomics happy-path order (37180) off the 5-row project-table page.
  Scenario: A mixed order (plates and vials) offers both and defaults to Vial
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I set "Instrument" to "EXPLORIS_3"
    And I set "Sampler" to "Vanquish"
    And I select order 37183
    Then the "Queue Type" picker offers "Vial"
    And the "Queue Type" picker offers "Plate"
    And the "Queue Type" dropdown shows "Vial"

  # The marquee behavioural change: for a mixed order, Vial mode loads only the
  # off-plate samples (4 of 37183's 8), never the plate-resident ones.
  Scenario: A mixed order run as Vial loads only its off-plate samples
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I set "Instrument" to "EXPLORIS_3"
    And I set "Sampler" to "Vanquish"
    And I select order 37183
    And I set "Queue Type" to "Vial"
    Then the selection banner reports 4 samples

  Scenario: Combining a plate-only and a vial-only order offers both queue types
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I select order 37182
    Then the "Queue Type" picker offers "Vial"
    And the "Queue Type" picker offers "Plate"

  Scenario: A Vial-only sampler does not hide a Plate order's queue type
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I select order 37180
    And I set "Instrument" to "LUMOS_2"
    Then the "Queue Type" picker offers "Plate"
    And the "Queue Type" dropdown shows "Plate"
    And no sampler-incompatibility warning is shown
    And no empty-order warning is shown

  Scenario: A compatible sampler shows no incompatibility warning
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    Then no sampler-incompatibility warning is shown
