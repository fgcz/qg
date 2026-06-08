Feature: Multi-container order selection
  As a proteomics core operator
  I want to combine samples from multiple containers into a single queue
  So that one queue run can cover related orders without manual merging.

  Scenario: Selecting two orders aggregates samples and filenames embed both IDs
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I select order 37181
    And I set "Queue Type" to "Plate"
    And I upload to B-Fabric
    Then I download the queue CSV
    And the downloaded filename contains "37180"
    And the downloaded filename contains "37181"
    And the queue contains all 12 samples from container 37180
    And the queue contains all 4 samples from container 37181
    And samples from container 37181 appear after samples from container 37180
