Feature: Non-employee with empty container halts with a danger callout
  As a non-employee user pinned to a container with no samples
  I want the app to halt with a clear danger callout
  So that I understand why no queue can be generated.

  Scenario: Empty pinned container shows the "No samples found" callout
    Given a non-employee session pinned to container 99999
    When I open the queue app
    Then a danger callout reads "No samples found in this container."
