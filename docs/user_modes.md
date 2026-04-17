# User Modes — Requirements

Status: **ready for implementation**
Scope: `src/qg/apps/queue_app.py`

## Authentication

The app requires an authenticated user in the request. An unauthenticated session is permitted only when `QG_ALLOW_UNAUTHENTICATED=1` is set in the environment; otherwise the app refuses to render. Production deployments do not set this variable.

## Inputs

- `is_employee: bool` — obtained by calling `POST /user/is_employee` on `bfabric_rest_proxy` with the authenticated user's credentials. If the call fails (network error, non-2xx, etc.) the app fails closed and refuses to render. In the `QG_ALLOW_UNAUTHENTICATED=1` dev path, the call is skipped and `is_employee` is `True`.
- `container_id` (non-employees only) — supplied by the request (`entity_id`). Because B-Fabric addresses Container / Order / Project by the same `containerid` key, this one input covers all three entity classes. Auth, existence and permission are guaranteed upstream; the app does not re-check.

## Non-employee preconditions

Before any B-Fabric query runs: `entity_class ∈ {Container, Order, Project}` and `entity_id is not None`. If either fails, the app halts with no queries issued and no data loaded.

## Non-employee experience

- The container from the request is fixed and shown read-only. There is no order table, no refresh button, and no access to other containers' metadata.
- The container cache (`projects_df`) is never loaded.
- All other queue-building controls and the upload button behave the same as for employees.
- If the container has no samples, the app halts — no partial data is loaded. UX of the halted state is left to implementation.
- Tech area is freely selectable. It may optionally be restricted to options matching the container's Area, if the existing Area→tech_area mapping can be reused without adding new config.

## Employee experience

Full order table with multi-select, refresh button, and all controls freely selectable. No preselection from the request.

---

## Summary of changes (tentative)

- Add `QG_ALLOW_UNAUTHENTICATED` opt-in; remove silent unauthenticated fallback.
- Call `bfabric_rest_proxy` `/user/is_employee` during the auth cell to obtain `is_employee`.
- Read `container_id` (from `entity_id`) for non-employees.
- Enforce non-employee preconditions (`entity_class`, `entity_id`) at entry; halt otherwise.
- Replace the order table with a read-only banner in non-employee mode; suppress the refresh button.
- Stop loading the container cache for non-employees.
- Halt the app when a non-employee's container has no samples.
