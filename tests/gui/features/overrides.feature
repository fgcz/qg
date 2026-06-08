Feature: Per-run parameter overrides
  As a core operator running a non-standard batch
  I want to override the default injection volume and QC frequency for a single run
  So that I can adapt to a method's needs without editing the shared config.

  Scenario: Injection volume override is applied to every user sample
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    And I set the injection volume to "5"
    And I upload to B-Fabric
    Then I download the queue CSV
    And every user-sample row uses injection volume "5.0"

  Scenario: QC frequency override changes how often QC injections appear
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    And I set the QC frequency to "4"
    And I upload to B-Fabric
    Then I download the queue CSV
    And the number of autoQC01 injections is at least 3
