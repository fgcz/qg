Feature: Launching-order pre-load
  As a user who opened the queue app from a specific B-Fabric order
  I want the app to align the Tech Area to that order and pre-select it
  So that the launching order's samples load immediately without manual picking.

  Scenario: Employee launched from a Metabolomics order aligns Tech Area and pre-selects the order
    Given an employee session launched from order 37190
    When I open the queue app
    Then the "Tech Area" dropdown shows "Metabolomics"
    And the selected-orders banner shows order 37190

  Scenario: Non-employee pinned to a Metabolomics order aligns Tech Area
    Given a non-employee session pinned to order 37190
    When I open the queue app
    Then the "Tech Area" dropdown shows "Metabolomics"
