import nox

nox.options.default_venv_backend = "uv|venv"

@nox.session(python="3.13")
def tests(session):
    session.install(".")
    session.run("pytest")