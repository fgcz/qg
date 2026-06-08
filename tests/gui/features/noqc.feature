Feature: The noqc QC layout hides the Pattern picker
  As a metabolomics core operator running without QC injections
  I want the Pattern picker to disappear when I pick the noqc layout
  So that I am not offered a meaningless pattern choice.

  Background:
    Given the queue app is open for a vial-only container

  Scenario: Picking noqc removes the Pattern picker
    When I set "Tech Area" to "Metabolomics"
    And I set "QC Layout" to "noqc"
    Then the Pattern picker is hidden

  Scenario: Switching back from noqc restores the Pattern picker
    When I set "Tech Area" to "Metabolomics"
    And I set "QC Layout" to "noqc"
    And I set "QC Layout" to "standard"
    Then the Pattern picker is visible
