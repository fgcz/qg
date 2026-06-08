Feature: Warn when the sampler cannot run the order's samples
  As a core operator configuring a queue
  I want a clear warning when my sampler is incompatible with the order
  So that I do not silently configure an unrunnable queue.

  Background:
    Given a non-employee session pinned to container 37180
    When I open the queue app
    And I set "Tech Area" to "Proteomics"

  Scenario: Incompatible sampler shows the warning callout
    When I set "Instrument" to "LUMOS_2"
    Then a warning reads "incompatible with this order's samples"

  Scenario: Compatible sampler does not show the warning callout
    When I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    Then no sampler-incompatibility warning is shown
