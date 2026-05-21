"""B-Fabric authenticated config editor."""

from pathlib import Path

from qg.apps._bfabric_auth import create_bfabric_fastapi_app

app = create_bfabric_fastapi_app(
    Path(__file__).parent / "config_editor.py",
    app_name="qg-editor",
    mount_path="/qg-editor",
)

if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="localhost", port=8000)
