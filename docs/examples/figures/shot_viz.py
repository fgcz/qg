#!/usr/bin/env python3
"""Capture the qg local-app visualizations as committed figures/screenshots.

This is the *single renderer* for the acquisition-timeline figures: the GUI's
``qg.viz.timeline.build_timeline_figure`` is screenshotted directly, so the
manuscript figure and the live app can never drift. It produces:

* ``fig_lipidomics_queue.png`` — the SI "worked lipidomics queue" figure. Loads
  the bundled, self-contained ``lipidomics_standard.json`` through the app's
  *reproduce mode* (the "bundled self-contained run" dropdown), then screenshots
  the **Acquisition Timeline** in "Injection class" mode — the QC-by-type colours
  plus the pos/neg polarity track. This is exactly the view a reader reproduces by
  hand (see the SI "worked lipidomics queue" section).
* ``qg_gui_41786_visualizations_*.png`` — doc screenshots of the plate-layout and
  timeline views for a multi-container proteomics example.

Self-contained: spawns ``marimo run --headless --no-token`` on a free port
(mirroring ``tests/gui_local/conftest.py``), so no app needs to be running.
``device_scale_factor=3`` gives a publication-crisp raster.

Run (needs playwright from the ``dev`` group; kaleido/pillow are not required)::

    cd qg && uv run --group dev python docs/examples/figures/shot_viz.py
"""

from __future__ import annotations

import asyncio
import os
import socket
import subprocess
import sys
import time
from pathlib import Path
from urllib.error import URLError
from urllib.request import urlopen

from playwright.async_api import async_playwright

HERE = Path(__file__).resolve().parent
REPO_ROOT = HERE.parents[2]  # qg/
APP = REPO_ROOT / "src" / "qg" / "apps" / "queue_app_local.py"

LIPID_RUN = "Lipidomics — dual-polarity standard run"  # bundled self-contained run label
PROTEO_EXAMPLE = "Vial — 3 projects (multi-container)"  # sample-source example label


def _free_port() -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def _wait_for(url: str, timeout: float = 60.0) -> None:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        try:
            with urlopen(url, timeout=1.0) as resp:  # noqa: S310 — localhost only
                if resp.status < 500:
                    return
        except (URLError, ConnectionError, TimeoutError):
            time.sleep(0.25)
    raise RuntimeError(f"local app at {url} did not become ready in {timeout}s")


async def _click(page, label: str) -> bool:
    for loc in (page.get_by_role("tab", name=label, exact=True), page.get_by_text(label, exact=True)):
        try:
            await loc.first.click(timeout=6000)
            return True
        except Exception:
            continue
    return False


async def _shoot_plot(page, path: Path) -> None:
    figs = page.locator(".js-plotly-plot")
    if await figs.count() == 0:
        print("  no plotly plot for", path.name)
        return
    await figs.first.scroll_into_view_if_needed()
    await page.wait_for_timeout(1200)
    await figs.first.screenshot(path=str(path))
    print("wrote", path.relative_to(REPO_ROOT))


async def _select_option_containing(page, option_text: str) -> bool:
    """Select ``option_text`` in whichever visible <select> offers it."""
    n = await page.locator("select").count()
    for i in range(n):
        sel = page.locator("select").nth(i)
        opts = await sel.locator("option").all_inner_texts()
        if any(option_text in o for o in opts):
            await sel.select_option(label=next(o for o in opts if option_text in o))
            return True
    return False


async def _capture_lipidomics_figure(context, url: str) -> None:
    """SI figure: reproduce the bundled lipidomics run and screenshot its timeline."""
    page = await context.new_page()
    await page.goto(url, wait_until="networkidle", timeout=60000)
    await page.wait_for_timeout(9000)
    # Reproduce mode: load the self-contained lipidomics params from the bundled-run dropdown.
    try:
        await page.get_by_label("bundled self-contained run").select_option(label=LIPID_RUN)
    except Exception:
        await _select_option_containing(page, LIPID_RUN)
    await page.wait_for_timeout(8000)  # regenerate from the embedded resolved_config
    await _click(page, "Visualizations")
    await page.wait_for_timeout(2500)
    await _click(page, "Acquisition Timeline")
    await page.wait_for_timeout(4500)
    await _shoot_plot(page, HERE / "fig_lipidomics_queue.png")
    await page.close()


async def _capture_doc_screenshots(context, url: str) -> None:
    """Doc screenshots of the plate-layout + timeline views for a multi-container run."""
    page = await context.new_page()
    await page.goto(url, wait_until="networkidle", timeout=60000)
    await page.wait_for_timeout(9000)
    if not await _select_option_containing(page, PROTEO_EXAMPLE):
        print("  proteomics example not found — skipping doc screenshots")
        await page.close()
        return
    await page.wait_for_timeout(8000)
    await _click(page, "Visualizations")
    await page.wait_for_timeout(3000)
    await _click(page, "Plate Layout")
    await page.wait_for_timeout(1500)
    await _select_option_containing(page, "Group (grouping_var)")
    await page.wait_for_timeout(3500)
    await _shoot_plot(page, HERE / "qg_gui_41786_visualizations_plate_layout_group.png")
    await _click(page, "Acquisition Timeline")
    await page.wait_for_timeout(4000)
    await _shoot_plot(page, HERE / "qg_gui_41786_visualizations_acquisition_timeline.png")
    await page.close()


async def _run(url: str) -> None:
    async with async_playwright() as p:
        browser = await p.chromium.launch()
        context = await browser.new_context(viewport={"width": 1600, "height": 1100}, device_scale_factor=3)
        await _capture_lipidomics_figure(context, url)
        await _capture_doc_screenshots(context, url)
        await browser.close()


def main() -> int:
    port = _free_port()
    url = f"http://127.0.0.1:{port}"
    proc = subprocess.Popen(
        [
            sys.executable,
            "-m",
            "marimo",
            "run",
            "--headless",
            "--no-token",
            "--host",
            "127.0.0.1",
            "--port",
            str(port),
            str(APP),
        ],
        cwd=REPO_ROOT,
        env={**os.environ, "PYTHONPATH": str(REPO_ROOT)},
        stdout=subprocess.DEVNULL,
        stderr=subprocess.STDOUT,
    )
    try:
        _wait_for(url)
        asyncio.run(_run(url))
    finally:
        proc.terminate()
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            proc.kill()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
