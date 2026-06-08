Feature: Non-employee pinned-container workflow
  As a non-employee B-Fabric user pinned to a single container
  I want the project-selection UI to be hidden
  So that I cannot accidentally pick a different container.

  Background:
    Given a non-employee session pinned to container 37180
    When I open the queue app

  Scenario: Non-employee sees no project table and no refresh button
    Then the project-selection table is not visible
    And the "Refresh Projects" button is not visible

  Scenario: Non-employee can still configure tech/instrument/sampler
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    Then the "Queue Type" selector is visible
