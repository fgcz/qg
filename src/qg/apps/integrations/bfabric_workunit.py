"""B-Fabric workunit sink for the portal queue app.

Builds the ``CreateWorkunitParams`` payload (queue file + params.json resources)
the portal uploads via the feeder. Importing this module requires the
``qg[bfabric]`` extra.
"""

from __future__ import annotations

import base64
from typing import TYPE_CHECKING

import yaml
from bfabric_rest_proxy.feeder_operations.create_workunit import CreateWorkunitParams

if TYPE_CHECKING:
    from qg.params_models import QueueInput


def gather_workunit_parameters(
    queue_input: QueueInput,
    *,
    app_version: str,
    application_id: int,
    target_container_id: int,
    queue_output_filename: str,
    queue_output_str: str,
) -> CreateWorkunitParams:
    """Assemble the workunit payload for a generated queue.

    Parameter values are stringified (lists/dicts as flow-style YAML) to match the
    B-Fabric workunit-parameter schema; the queue file and the full params JSON are
    attached as base64 resources. Callers must only invoke this once a queue exists.
    """
    parameters = queue_input.parameters.model_dump()
    for key in parameters:
        if isinstance(parameters[key], list | dict):
            parameters[key] = yaml.safe_dump(parameters[key], default_flow_style=True).strip()
        else:
            parameters[key] = str(parameters[key])

    return CreateWorkunitParams(
        container_id=target_container_id,
        application_id=application_id,
        workunit_name=queue_output_filename.split(".")[0],
        parameters=parameters,
        resources={
            queue_output_filename: base64.b64encode(queue_output_str.encode("utf8")),
            "parameters.json": base64.b64encode(queue_input.model_dump_json(indent=2).encode("utf8")),
        },
        links={},
        input_resource_ids=[],
        description=f"Queue configuration generated with qg version {app_version}.",
    )
