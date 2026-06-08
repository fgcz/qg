Feature: Valid-Combinations reference table
  As a core operator exploring which combinations are supported
  I want a reference table of every valid tech-area/instrument/sampler tuple
  So that I can browse alternatives without trial-and-error in the form.

  Scenario: Opening the Valid Combinations tab shows the master combinations table
    Given the queue app is open as an employee
    When I switch to the "Valid Combinations" tab
    Then the combinations table is visible
    And the table reports how many combinations match the current selection
