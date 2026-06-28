import asyncio
from pathlib import Path

from playwright.async_api import async_playwright

IMG = Path("/Users/wolski/projects/queue_generator/img")
URL = "http://localhost:2721"
EXAMPLE = "Vial — 3 projects (multi-container)"


async def click(page, label):
    for loc in (page.get_by_role("tab", name=label, exact=True), page.get_by_text(label, exact=True)):
        try:
            await loc.first.click(timeout=6000)
            return True
        except Exception:
            continue
    return False


async def shoot_plot(page, fname):
    figs = page.locator(".js-plotly-plot")
    if await figs.count() == 0:
        print("  no plotly for", fname)
        return
    await figs.first.scroll_into_view_if_needed()
    await page.wait_for_timeout(1200)
    await figs.first.screenshot(path=str(IMG / fname))
    print("wrote", fname)


async def main():
    async with async_playwright() as p:
        b = await p.chromium.launch()
        page = await b.new_page(viewport={"width": 1700, "height": 1100})
        await page.goto(URL, wait_until="networkidle", timeout=60000)
        await page.wait_for_timeout(9000)
        n = await page.locator("select").count()
        for i in range(n):
            opts = await page.locator("select").nth(i).locator("option").all_inner_texts()
            if EXAMPLE in opts:
                await page.locator("select").nth(i).select_option(label=EXAMPLE)
                break
        await page.wait_for_timeout(8000)
        await click(page, "Visualizations")
        await page.wait_for_timeout(4000)

        # Plate Layout view, colored by group
        await click(page, "Plate Layout")
        await page.wait_for_timeout(1500)
        ns = await page.locator("select").count()
        for i in range(ns):
            if not await page.locator("select").nth(i).is_visible():
                continue
            opts = await page.locator("select").nth(i).locator("option").all_inner_texts()
            if "Group (grouping_var)" in opts:
                await page.locator("select").nth(i).select_option(label="Group (grouping_var)")
                print("set color-by = Group")
                break
        await page.wait_for_timeout(4000)
        await shoot_plot(page, "qg_gui_41786_visualizations_plate_layout_group.png")

        # Acquisition Timeline view
        await click(page, "Acquisition Timeline")
        await page.wait_for_timeout(4500)
        await shoot_plot(page, "qg_gui_41786_visualizations_acquisition_timeline.png")
        await b.close()


asyncio.run(main())
