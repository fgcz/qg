Feature: Queue Type offered follows the selected B-Fabric sample source
  As a core operator configuring a queue
  I want the Queue Type choice to reflect what the selected source produces
  So that Plate and Vial choices are not hidden by unrelated GUI selections.

  # Order items preserve plate/vial placement. All container samples are exposed
  # as Vial. Queue Type then limits Sampler and Instrument to valid choices.

  Scenario: A plate-only order can only be run as a Plate queue
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I select order 37180
    Then the "Queue Type" picker offers "Plate"
    And the "Queue Type" picker does not offer "Vial"
    And the "Sampler" picker offers "Vanquish"
    And the "Sampler" picker does not offer "MClass"
    When I set "Sampler" to "Vanquish"
    Then the "Instrument" picker offers "ASTRAL_1"
    And the "Instrument" picker does not offer "LUMOS_2"

  Scenario: A vial-only order can only be run as a Vial queue
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I select order 37182
    Then the "Queue Type" picker offers "Vial"
    And the "Queue Type" picker does not offer "Plate"
    And the "Sampler" picker offers "MClass"
    When I set "Sampler" to "MClass"
    Then the "Instrument" picker offers "LUMOS_2"

  # 37183 is a Metabolomics container so that adding it does not push the
  # Proteomics happy-path order (37180) off the 5-row project-table page.
  Scenario: A mixed order (plates and vials) offers both and defaults to Vial
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I select order 37183
    Then the "Queue Type" picker offers "Vial"
    And the "Queue Type" picker offers "Plate"
    And the "Queue Type" dropdown shows "Vial"

  # The marquee behavioural change: for a mixed order, Vial mode loads only the
  # off-plate samples (4 of 37183's 8), never the plate-resident ones.
  Scenario: A mixed order run as Vial loads only its off-plate samples
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I select order 37183
    And I set "Queue Type" to "Vial"
    And I set "Sampler" to "Vanquish"
    And I set "Instrument" to "EXPLORIS_3"
    Then the selection banner reports 4 samples

  Scenario: Combining a plate-only and a vial-only order offers both queue types
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I select order 37180
    And I select order 37182
    Then the "Queue Type" picker offers "Vial"
    And the "Queue Type" picker offers "Plate"

  Scenario: Changing a mixed order to Plate removes Vial-only samplers and instruments
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I select order 37180
    And I select order 37182
    Then the "Queue Type" dropdown shows "Vial"
    And the "Sampler" picker offers "MClass"
    When I set "Queue Type" to "Plate"
    Then the "Sampler" picker does not offer "MClass"
    When I set "Sampler" to "Vanquish"
    Then the "Instrument" picker does not offer "LUMOS_2"
    And the "Instrument" picker offers "ASTRAL_1"
