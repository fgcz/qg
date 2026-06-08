Feature: Download Params JSON round-trip
  As a queue-app user
  I want the Download Params JSON button to emit my current selections
  So that I can reproduce or share the same run later.

  Scenario: The downloaded params JSON reflects the user's selections
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    And I download the params JSON
    Then the params JSON has tech_area "Proteomics" and instrument "ASTRAL_1" and sampler "Vanquish"
    And the params JSON has at least one sample with container_id 37180
    And the params JSON parameters include queue_type, queue_pattern, randomization, polarity, date, and user
    And the params JSON parameters include method, inj_vol_override, and qc_frequency_override
