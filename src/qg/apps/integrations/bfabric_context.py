"""B-Fabric session context for the portal queue app.

Thin re-export so ``queue_app.py`` sources its authenticated session from the
integrations package (symmetric with the ``local_*`` modules). Importing this
module requires the ``qg[bfabric]`` extra.
"""

from qg.bfabric_utils import SessionError, resolve_app_session

__all__ = ["SessionError", "resolve_app_session"]
