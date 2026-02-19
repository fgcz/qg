"""B-Fabric authenticated queue app (original entry point, no git pull)."""

from pathlib import Path

from qg.apps._bfabric_auth import create_bfabric_fastapi_app

app = create_bfabric_fastapi_app(Path(__file__).parent / "queue_app.py")

if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="localhost", port=8000)
