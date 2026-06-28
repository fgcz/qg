import asyncio
from pathlib import Path

from playwright.async_api import async_playwright

IMG = Path("/Users/wolski/projects/queue_generator/img")
IMG.mkdir(parents=True, exist_ok=True)
URL = "http://localhost:2720"

TABS = [
    ("Overview", "editor_overview.png"),
    ("Queue Patterns", "editor_queue_patterns.png"),
    ("Samples", "editor_samples.png"),
    ("QC Layouts Well", "editor_qc_layouts_well.png"),
    ("Instrument Config", "editor_instrument_config.png"),
    ("Methods", "editor_methods.png"),
    ("All Combinations", "editor_all_combinations.png"),
]


async def click_tab(page, label):
    # Prefer the radio by accessible name; fall back to the radio's label text.
    for loc in (
        page.get_by_role("radio", name=label, exact=True),
        page.locator(f'label:has-text("{label}")').locator('input[type=radio]'),
    ):
        try:
            await loc.first.scroll_into_view_if_needed(timeout=4000)
            await loc.first.click(timeout=6000)
            return True
        except Exception:
            continue
    return False


async def main():
    async with async_playwright() as p:
        browser = await p.chromium.launch()
        page = await browser.new_page(viewport={"width": 1500, "height": 1000})
        await page.goto(URL, wait_until="networkidle", timeout=60000)
        await page.wait_for_timeout(9000)
        for label, fname in TABS:
            await page.evaluate("window.scrollTo(0, 0)")
            await page.wait_for_timeout(400)
            ok = await click_tab(page, label)
            await page.wait_for_timeout(3500)
            await page.evaluate("window.scrollTo(0, 0)")
            # hide the small dev "Running unauthenticated ... QG_ALLOW_UNAUTHENTICATED" banner
            await page.evaluate("""() => {
              for (const e of document.querySelectorAll('p, div, span, small, em, strong')) {
                const t = (e.textContent||'').trim();
                if (t.includes('QG_ALLOW_UNAUTHENTICATED') && t.length < 220 && !e.querySelector('input,button,select')) {
                  e.style.visibility='hidden';
                }
              }
            }""")
            await page.wait_for_timeout(400)
            await page.screenshot(path=str(IMG / fname), full_page=True)
            print(f"{'ok ' if ok else 'CLICK-FAIL'} {fname}")
        await browser.close()


asyncio.run(main())
