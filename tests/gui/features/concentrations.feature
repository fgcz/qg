Feature: Calibration concentration inputs
  As a metabolomics core operator running a dilution series
  I want a concentration grid to appear when I pick the cal_series QC layout
  So that I can record the concentration of each calibration standard.

  Background:
    Given a non-employee session pinned to a Metabolomics container
    When I open the queue app

  Scenario: The cal_series layout reveals one input per calibration level
    When I set "Tech Area" to "Metabolomics"
    And I set "QC Layout" to "cal_series"
    Then one row per calibration level is shown
    And each row offers a configurable concentration value and unit

  Scenario: A non-cal QC layout hides the concentration grid
    When I set "Tech Area" to "Metabolomics"
    And I set "QC Layout" to "cal_series"
    And I set "QC Layout" to "standard"
    Then the concentration grid is not visible
