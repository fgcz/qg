Feature: Polarity selection drives the method picker
  As a core operator setting up an acquisition
  I want one method picker per active polarity
  So that Proteomics shows one method while Metabolomics and Lipidomics show two.

  Background:
    Given the queue app is open as an employee

  Scenario: Proteomics shows only the positive-polarity method picker
    When I set "Tech Area" to "Proteomics"
    Then a method picker for the positive polarity is not visible
    And a method picker for the negative polarity is not visible
    And the positive polarity is selected
    And the negative polarity is not selected

  Scenario: Metabolomics shows both polarities and both method pickers
    When I set "Tech Area" to "Metabolomics"
    Then the positive polarity is selected
    And the negative polarity is selected
    And a method picker for the positive polarity is visible
    And a method picker for the negative polarity is visible

  Scenario: Deselecting the negative polarity hides its method picker
    When I set "Tech Area" to "Metabolomics"
    And the user deselects the negative polarity
    Then a method picker for the negative polarity is not visible
