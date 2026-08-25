Feature: Hystar XML output for TIMSTOF instruments
  As a proteomics core operator using a TIMSTOF instrument
  I want the queue download to be a Hystar-compatible XML file
  So that the queue can be imported by the Hystar scheduler.

  Scenario: TIMSTOF_1 download yields an XML file
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    And I set "Sampler" to "Evosep"
    And I set "Instrument" to "TIMSTOF_1"
    Then the "Instrument" dropdown shows "TIMSTOF_1"
    And the "Sampler" dropdown shows "Evosep"
    When I upload to B-Fabric
    Then I download the queue file
    And the downloaded filename ends with ".xml"
    And the downloaded file is a valid Hystar XML sequence
