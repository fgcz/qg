Feature: Upload to B-Fabric (mocked)
  As a queue-app user
  I want the Upload-to-B-Fabric button to surface the mock workunit message
  So that I can confirm the upload flow wired the right container.

  Scenario: Clicking Upload shows the mock workunit-creation message
    Given the queue app is open as an employee
    When I set "Tech Area" to "Proteomics"
    And I set "Instrument" to "ASTRAL_1"
    And I set "Sampler" to "Vanquish"
    And I select order 37180
    And I set "Queue Type" to "Plate"
    And I upload to B-Fabric
    Then the upload-result panel mentions a mock workunit in container 37180
    And the upload bundles the queue file and parameters.json (2 resources)
