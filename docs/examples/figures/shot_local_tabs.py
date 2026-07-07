import asyncio
from pathlib import Path

from playwright.async_api import async_playwright

IMG = Path("/Users/wolski/projects/queue_generator/img")
IMG.mkdir(parents=True, exist_ok=True)
URL = "http://localhost:2721"
EXAMPLE = "Vial — 3 projects (multi-container)"


async def click(page, label):
    for loc in (page.get_by_role("tab", name=label, exact=True), page.get_by_text(label, exact=True)):
        try:
            await loc.first.scroll_into_view_if_needed(timeout=3000)
            await loc.first.click(timeout=6000)
            return True
        except Exception:
            continue
    return False


async def main():
    async with async_playwright() as p:
        b = await p.chromium.launch()
        page = await b.new_page(viewport={"width": 1600, "height": 1050})
        await page.goto(URL, wait_until="networkidle", timeout=60000)
        await page.wait_for_timeout(9000)
        n = await page.locator("select").count()
        for i in range(n):
            opts = await page.locator("select").nth(i).locator("option").all_inner_texts()
            if EXAMPLE in opts:
                await page.locator("select").nth(i).select_option(label=EXAMPLE)
                break
        await page.wait_for_timeout(9000)

        for label, fname in [
            ("✎ Edit Samples", "qg_gui_41786_sample_selection.png"),
            ("Parameters", "qg_gui_41786_parameters.png"),
            ("Queue Preview", "qg_gui_41786_queue_preview.png"),
        ]:
            ok = await click(page, label)
            await page.wait_for_timeout(3000)
            await page.evaluate("window.scrollTo(0,0)")
            await page.wait_for_timeout(300)
            await page.screenshot(path=str(IMG / fname), full_page=True)
            print(("ok  " if ok else "FAIL ") + fname)

        # Visualizations tab -> two plotly figures
        await click(page, "Visualizations")
        await page.wait_for_timeout(4500)
        figs = page.locator(".js-plotly-plot")
        cf = await figs.count()
        print("plotly figs in viz:", cf)
        names = [
            "qg_gui_41786_visualizations_plate_layout_group.png",
            "qg_gui_41786_visualizations_acquisition_timeline.png",
        ]
        for j in range(min(cf, 2)):
            await figs.nth(j).scroll_into_view_if_needed()
            await page.wait_for_timeout(900)
            await figs.nth(j).screenshot(path=str(IMG / names[j]))
            print("wrote", names[j])

        # Sample editor = the editable table on the ✎ Edit Samples tab (element crop)
        await click(page, "✎ Edit Samples")
        await page.wait_for_timeout(3000)
        tbls = page.locator("table")
        tc = await tbls.count()
        target = None
        best = 0
        for j in range(tc):
            box = await tbls.nth(j).bounding_box()
            if box and box["height"] * box["width"] > best:
                best, target = box["height"] * box["width"], j
        if target is not None:
            await tbls.nth(target).scroll_into_view_if_needed()
            await page.wait_for_timeout(600)
            await tbls.nth(target).screenshot(path=str(IMG / "qg_gui_41786_sample_editor.png"))
            print("wrote qg_gui_41786_sample_editor.png (table crop)")
        await b.close()


asyncio.run(main())
