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

  Scenario: All container samples present a plate order as vials
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I select order 37180
    Then the "Queue Type" dropdown shows "Plate"
    When I choose all container samples
    Then the "Queue Type" dropdown shows "Vial"

  Scenario: The project fallback notice belongs only to the order-item source
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I select order 37196
    Then the no-order-items fallback is shown for container 37196
    When I choose all container samples
    Then no no-order-items fallback is shown
