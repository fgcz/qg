Feature: Validation messages
  As a queue app user
  I want the app to tell me which required field is missing
  So that I know what to do next.

  Scenario: Opening the app with no order selected shows "Please select an order"
    Given the queue app is open as an employee
    Then the validation callout warns "Please select an order"
