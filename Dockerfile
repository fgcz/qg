FROM astral/uv:python3.14-trixie-slim
RUN apt-get update && apt-get install -y git && rm -rf /var/lib/apt/lists/*
COPY . /app
WORKDIR /app
RUN --mount=type=cache,target=/root/.cache/uv uv sync
ENTRYPOINT ["/app/.venv/bin/uvicorn", "qg.apps.bfabric_app:app", "--host", "0.0.0.0", "--port", "8000"]
EXPOSE 8000
