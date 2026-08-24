Feature: Project and order containers enter the queue at their own entrypoint

  As a core operator
  I want a project container (no order items) and an order container (with order
  items) each to preserve their plate placement when queued
  So that a project holding plates is not silently degraded to a Vial queue,
  and an order's plate placement is preserved by its order items.

  Containers enter via one of two B-Fabric entrypoints:
  - An **order** has billable order items, so its plate/vial placement is driven
    by those items (fixtures 37180/37182).
  - A **project** has no order items, so it falls back to all of its container
    samples — but that fallback must still preserve plate placement
    (fixtures 37210/37196).

  Scenario: A project container (no order items) holding plates offers only Plate
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I set "Instrument" to "EXPLORIS_3"
    And I set "Sampler" to "Vanquish"
    And I select order 37210
    Then the "Queue Type" picker offers "Plate"
    And the "Queue Type" picker does not offer "Vial"

  Scenario: A project container (no order items) holding only vials offers only Vial
    Given the queue app is open as an employee
    When I set "Tech Area" to "Metabolomics"
    And I set "Instrument" to "EXPLORIS_3"
    And I set "Sampler" to "Vanquish"
    And I select order 37196
    Then the "Queue Type" picker offers "Vial"
    And the "Queue Type" picker does not offer "Plate"

  Scenario: An order container holding plates offers only Plate
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    Then the "Queue Type" picker offers "Plate"
    And the "Queue Type" picker does not offer "Vial"

  Scenario: An order container holding only vials offers only Vial
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37182
    Then the "Queue Type" picker offers "Vial"
    And the "Queue Type" picker does not offer "Plate"
