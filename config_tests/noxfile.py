import nox
import os

nox.options.default_venv_backend = "uv|venv"


@nox.session(python="3.13")
def tests(session):
    session.install(".")

    # Pass through UPDATE_EXPECTED environment variable if set
    env = {}
    if "UPDATE_EXPECTED" in os.environ:
        env["UPDATE_EXPECTED"] = os.environ["UPDATE_EXPECTED"]

    session.run("pytest", "-vv", env=env)
