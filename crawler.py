from playwright.sync_api import sync_playwright
import pandas as pd
import time

def get_rows(page):
    rows_locator = page.locator("div[role='row']")
    rows_count = rows_locator.count()
    extracted = []

    for i in range(rows_count):
        row = rows_locator.nth(i)

        try:
            # Extract row-index attribute as new column
            row_index = row.get_attribute("row-index")
            cells = row.locator("div[role='gridcell']").all_inner_texts()
            if cells:
                extracted.append((row_index, *cells))  # prepend row-index to data
        except Exception as e:
            print(f"⚠️ Skipped row {i} due to error: {e}")
            continue

    return extracted

with sync_playwright() as p:
    browser = p.chromium.launch(headless=False)
    page = browser.new_page()

    # Open Power BI dashboard
    page.goto("https://app.powerbi.com/view?r=eyJrIjoiMDYxMTYyYzMtN2FiMi00NWE4LTgzZTMtNzk0ZTRlYTA3MGMyIiwidCI6Ijc3NDEwMTk1LTE0ZTEtNGZiOC05MDRiLWFiMTg5MjAyMzY2NyIsImMiOjh9&pageName=6f4d8ec726210fb39bbe")
    print("✅ Power BI dashboard page opened")

    # Click "Data Centre"
    page.wait_for_selector("div[role='button']:has-text('Data Centre')", timeout=30000)
    page.click("div[role='button']:has-text('Data Centre')")
    print("✅ Clicked Data Centre button")

    # Wait for table grid to load
    page.wait_for_selector("div[role='grid']", timeout=30000)
    print("✅ Table loaded")

    all_rows = {}
    all_row_indices_seen = set()
    prev_total = 0

    try:
        while True:
            new_rows = get_rows(page)
            for row in new_rows:
                row_index = row[0]
                all_rows[row_index] = row  # update dict to deduplicate by row-index
                if row_index is not None:
                    all_row_indices_seen.add(int(row_index))

            print(f"✅ Total unique rows collected so far: {len(all_rows)}")

            # Show current visible batch indices for tracking
            indices_in_batch = [int(row[0]) for row in new_rows if row[0] is not None]
            if indices_in_batch:
                min_idx = min(indices_in_batch)
                max_idx = max(indices_in_batch)
                print(f"🧭 Current batch indices: {min_idx} to {max_idx}")

            # Check for missing rows in overall seen range
            if all_row_indices_seen:
                expected_indices = set(range(min(all_row_indices_seen), max(all_row_indices_seen)+1))
                missing_indices = sorted(expected_indices - all_row_indices_seen)

                if missing_indices:
                    print("⚠️ MISSING ROW INDICES:", missing_indices)
                    print("💡 TIP: Scroll back up or down to load missing rows.")
                else:
                    print("✅ No missing rows detected in current covered range.")

            time.sleep(0.5)  # scrape every 1 second

    except KeyboardInterrupt:
        print("🛑 Script interrupted by user, saving collected data...")

    # Convert dict values to list of lists
    all_rows_list = list(all_rows.values())

    # Sort by row_index
    all_rows_list.sort(key=lambda x: int(x[0]) if x[0] is not None else -1)

    # Filter rows where col_2 == 'Select Row' (because row_index is now col_1)
    filtered_rows = [row for row in all_rows_list if len(row) > 1 and row[1] == "Select Row"]

    # Generate headers including row_index
    if filtered_rows:
        header_texts = ["row_index"] + [f"col_{i}" for i in range(1, len(filtered_rows[0]))]
    else:
        header_texts = []
        print("⚠️ No matching data rows found")

    # Build DataFrame
    df = pd.DataFrame(filtered_rows, columns=header_texts)
    print("✅ Filtered DataFrame created with shape:", df.shape)

    # Save as CSV
    df.to_csv("powerbi_data_centre_filtered_table_with_rowindex.csv", index=False, encoding="utf-8-sig")
    print("✅ Saved as powerbi_data_centre_filtered_table_with_rowindex.csv")

    browser.close()
    print("✅ Browser closed")
