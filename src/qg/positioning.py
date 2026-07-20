"""Public orchestration for assigning and validating physical sample positions."""

from __future__ import annotations

from qg.config_models.loader import QGConfiguration
from qg.params_models import (
    PlateQueueInput,
    PositionedQueueInput,
    QueueInput,
    QueueParameters,
    VialQueueInput,
)
from qg.positionV2 import create_assembled_sampler
from qg.utils import LayoutMode


def _create_sampler(
    config: QGConfiguration,
    parameters: QueueParameters,
    layout_mode: LayoutMode,
):
    pattern = config.queue_patterns.get_pattern(
        parameters.tech_area,
        parameters.queue_pattern,
    )
    sampler = config.samplers.get_sampler(parameters.sampler)
    start_tray = parameters.start_tray if parameters.start_tray != "" else sampler.trays[0]
    return create_assembled_sampler(
        sampler_name=parameters.sampler,
        layout_mode=layout_mode,
        config=config,
        tech_area=parameters.tech_area,
        pattern_sample_ids=pattern.get_all_sample_ids(),
        plate_layout_name=parameters.plate_layout,
        qc_layout_name=parameters.qc_layout_name,
        start_position=parameters.start_position,
        start_tray=start_tray,
    )


def position_queue(queue_input: QueueInput) -> PositionedQueueInput:
    """Assign or validate sample positions and return generation-ready input."""
    config = queue_input.resolved_config.to_configuration()
    parameters = queue_input.parameters

    if isinstance(queue_input, VialQueueInput):
        assigner = _create_sampler(config, parameters, LayoutMode.VIAL)
        plate_queue = assigner.assign(
            queue_input.queue,
            one_container_per_tray=parameters.one_container_per_tray,
        )
    elif isinstance(queue_input, PlateQueueInput):
        plate_queue = queue_input.queue
    else:
        raise TypeError(f"Unsupported queue input type: {type(queue_input).__name__}")

    validator = _create_sampler(config, parameters, LayoutMode.PLATE)
    plate_queue = validator.assign(plate_queue)

    return PositionedQueueInput(
        parameters=parameters,
        queue=plate_queue,
        qg_version=queue_input.qg_version,
        resolved_config=queue_input.resolved_config,
    )
