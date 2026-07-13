Feature: Queue Type offered follows the order's composition and the sampler
  As a core operator configuring a queue
  I want the Queue Type choice restricted to what the order actually holds and
  what the sampler can run
  So that I cannot run a plate-only order as vials (or vice versa), and I am
  warned when the sampler cannot run the order at all.

  # The Queue Type dropdown offers the intersection of (queue types the sampler
  # supports) and (what the container physically holds — plates, vials, or both),
  # Vial first. An empty intersection offers nothing and shows a warning. This is
  # the browser-side proof of the make_queue_type_field truth table unit-tested in
  # tests/test_queue_app_shared.py::TestMakeQueueTypeField.

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

  Scenario: Combining a plate-only and a vial-only order offers both queue types
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I select order 37182
    Then the "Queue Type" picker offers "Vial"
    And the "Queue Type" picker offers "Plate"

  Scenario: A sampler that cannot run the order's samples shows a warning
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I select order 37180
    And I set "Instrument" to "LUMOS_2"
    Then a warning reads "incompatible with this order's samples"

  Scenario: A compatible sampler shows no incompatibility warning
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    Then no sampler-incompatibility warning is shown
