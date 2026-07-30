Feature: Queue-specific data-path folder
  As a queue operator
  I want every queue to have a safe, visible folder name
  So that repeated runs do not overwrite each other's acquisition files.

  Scenario: A blank queue name defaults to the queue seed
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    Then the queue name field shows the automatic seed placeholder
    When I download the params JSON
    Then the params queue name equals the hexadecimal seed

  Scenario: A path-unsafe queue name is visibly normalized
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    And I set the queue name to "Hello World"
    Then the sidebar shows the normalized queue name "Hello_World"
    When I download the params JSON
    Then the params queue name is "Hello_World"
