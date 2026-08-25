Feature: Project containers enter the queue without losing plate placement

  As a core operator
  I want a project container (no order items) to preserve its plate placement when queued
  So that a project holding plates is not silently degraded to a Vial queue.

  A **project** has no order items, so it falls back to all of its container
  samples — but that fallback must still preserve plate placement
  (fixtures 37210/37196). Order containers (with order items) are
  covered separately in order_queue_type.feature.

  Scenario: A project container (no order items) holding plates offers only Plate
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I select order 37210
    And I set "Sampler" to "Vanquish"
    And I set "Instrument" to "EXPLORIS_3"
    Then the "Queue Type" picker offers "Plate"
    And the "Queue Type" picker does not offer "Vial"

  Scenario: A project container (no order items) holding only vials offers only Vial
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I select order 37196
    And I set "Sampler" to "Vanquish"
    And I set "Instrument" to "EXPLORIS_3"
    Then the "Queue Type" picker offers "Vial"
    And the "Queue Type" picker does not offer "Plate"
