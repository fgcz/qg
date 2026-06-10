Feature: Employee selecting an order with no samples sees a warning callout
  As an employee user who selects an order with no samples
  I want the app to show a warning callout
  So that I understand why no queue can be generated and can pick a different order.

  Scenario: Selecting an empty order shows the "No samples found" warning
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I select order 99999
    Then a warning callout reads "No samples found in the selected order(s)."
