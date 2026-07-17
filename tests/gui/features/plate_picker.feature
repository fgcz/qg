Feature: The plate picker appears only where it has an effect
  As a queue-app operator
  I want the plate-subset picker shown only in Plate mode, plus a heads-up when
  an order mixes plates and standalone samples
  So that I am not misled by a control that silently does nothing in Vial mode.

  # The picker only affects the Plate load path; in Vial mode get_samples ignores
  # the selected plates (bfabric_utils.py), so it is hidden there. Gating lives in
  # queue_app.py; the note is make_mixed_order_note in queue_app_shared.py. Fixtures:
  # 37183 mixed (plates + off-plate), 37180 plate-only, 37182 vial-only.

  Scenario: Plate mode shows the plate picker for an order with plates
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I set "Instrument" to "EXPLORIS_3"
    And I set "Sampler" to "Vanquish"
    And I select order 37183
    And I set "Queue Type" to "Plate"
    Then the plate picker is shown

  Scenario: Vial mode hides the plate picker
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I set "Instrument" to "EXPLORIS_3"
    And I set "Sampler" to "Vanquish"
    And I select order 37183
    And I set "Queue Type" to "Vial"
    Then the plate picker is not shown

  Scenario: A mixed order shows the composition note
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I select order 37183
    Then the mixed-order note is shown

  Scenario: A plate-only order shows no composition note
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I select order 37180
    Then the mixed-order note is not shown

  Scenario: A vial-only order shows no composition note
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I select order 37182
    Then the mixed-order note is not shown
