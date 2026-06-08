Feature: Excluding samples from a queue
  As a core operator working from a B-Fabric order
  I want to omit individual samples from the generated queue
  So that I can skip samples that failed QC without editing the B-Fabric order.

  Scenario: Unchecking a sample in the selection table excludes it from the queue
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    And I uncheck sample "Sample_05" from the selection table
    And I upload to B-Fabric
    Then I download the queue CSV
    And "Sample_05" is absent from the queue
    And 11 user samples are present
