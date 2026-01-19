"""Strategy protocols and implementations for pipeline steps.

Note: Position assignment is now handled by Sampler classes in positions.py.
This module is kept for potential future strategy implementations.
"""

# Position assignment was moved to positions.py:
# - Sampler classes handle position generation and QC merging
# - create_sampler() factory replaces create_position_assigner()
