Feature: Sample randomization
  As a core operator running quality control across many samples
  I want to randomize the injection order
  So that batch effects (carry-over, drift) are decorrelated from sample identity.

  Scenario: Random ordering reshuffles samples but keeps every sample exactly once
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    And I set "Randomization" to "random"
    And I upload to B-Fabric
    Then I download the queue CSV
    And every input sample appears exactly once
    And the sample order differs from the input order
