Feature: Happy path — Proteomics queue download
  As a proteomics core operator
  I want to generate and download a Proteomics queue end-to-end
  So that I can submit the deterministic CSV to the instrument scheduler.

  Scenario: Generate and download a deterministic Proteomics queue
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    And I set "Randomization" to "no"
    And the date is set to "2026-05-20"
    And I upload to B-Fabric
    Then I can download the queue CSV
    And the downloaded queue CSV matches the golden reference
