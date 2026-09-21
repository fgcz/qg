Feature: Operators choose which B-Fabric samples enter the queue
  As a queue-app operator
  I want to choose order items or all container samples
  So that the queue contains the intended B-Fabric records.

  Scenario: Order items are the default and all container samples can be restored
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I select order 37182
    Then the selection banner reports 1 samples
    And the sample type summary reads "1 × Unspecified"
    When I choose all container samples
    Then the selection banner reports 6 samples
    And the sample type summary reads "6 × Unspecified"

  Scenario: All container samples keep a plate order's placement
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I select order 37180
    Then the "Queue Type" dropdown shows "Plate"
    When I choose all container samples
    Then the "Queue Type" dropdown shows "Plate"

  # 37170 mirrors an order whose processed (child) samples sit on their own plate:
  # ordered samples 2151/2152 on a Storage plate, their children on plate 50216,
  # a grandchild off-plate, plus one unordered facility QC sample. The order is
  # opened as the launching order; its ID sorts below the Metabolomics orders
  # already on page 1 of the 5-row project table, so those stay in place.
  Scenario: Derived samples on a plate are offered as a Plate queue under order items
    Given an employee session launched from order 37170
    When I open the queue app
    And I set "Queue Type" to "Plate"
    And I set "Sampler" to "Vanquish"
    And I set "Instrument" to "EXPLORIS_3"
    Then the plate picker is shown
    And the selection banner reports 2 samples
    And the sample placement reads "2 on injection plates · 2 in storage boxes · 1 loose"
    And the lineage note reports 3 derived samples
    And the sample generation picker is shown

  Scenario: Deselecting a generation narrows the queue in Vial mode
    Given an employee session launched from order 37170
    When I open the queue app
    And I set "Queue Type" to "Vial"
    And I set "Sampler" to "Vanquish"
    And I set "Instrument" to "EXPLORIS_3"
    Then the selection banner reports 3 samples
    When I deselect sample generation "Original (2)"
    Then the selection banner reports 1 samples
    And the sample placement reads "2 on injection plates · 0 in storage boxes · 1 loose · 3 derived"

  Scenario: Order items admit derived samples but not unordered container samples
    Given an employee session launched from order 37170
    When I open the queue app
    And I set "Queue Type" to "Vial"
    And I set "Sampler" to "Vanquish"
    And I set "Instrument" to "EXPLORIS_3"
    Then the selection banner reports 3 samples
    When I choose all container samples
    Then the selection banner reports 4 samples
    And the sample placement reads "2 on injection plates · 2 in storage boxes · 2 loose"
    And no lineage note is shown

  Scenario: The project fallback notice belongs only to the order-item source
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I select order 37196
    Then the no-order-items fallback is shown for container 37196
    When I choose all container samples
    Then no no-order-items fallback is shown
