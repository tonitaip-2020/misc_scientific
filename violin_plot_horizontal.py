#!/usr/bin/env python3
"""
Horizontal, top-to-bottom stacked violin plots from multiple columns in
'd8_for_analyses.csv', saved as a PDF. Optimized for two-column layouts.

Columns expected (customize in CONFIG):
- mongodb_design_difficulty
- cassandra_design_difficulty
- mongodb_query_difficulty
- cassandra_query_difficulty
"""

import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

import shutil
from matplotlib import rcParams

# Auto-detect LaTeX. If found, use true TeX text rendering.
USE_TEX = shutil.which("latex") is not None

rcParams.update({
    "text.usetex": USE_TEX,             # True only if 'latex' is available
    "font.family": "serif",
    # Prefer CMU/CM if installed; otherwise falls back to DejaVu Serif cleanly
    "font.serif": ["CMU Serif", "Computer Modern Roman", "DejaVu Serif"],
    "mathtext.fontset": "cm",           # Computer Modern for math (no LaTeX needed)
    "axes.unicode_minus": False,
    "pdf.fonttype": 42,                 # Embed as TrueType (good for publishers)
    "ps.fonttype": 42,
})

if not USE_TEX:
    print("[INFO] LaTeX not found — using Computer Modern–style fonts without LaTeX.")

# ============================== CONFIG =======================================
CSV_FILE = "d8_for_analyses.csv"
OUTPUT_FILE = "fig-violin_difficulties_horizontal.pdf"

# Columns to plot (ordered top -> bottom); add/remove as needed
COLUMNS = [
    "mongodb_design_difficulty",
    "cassandra_design_difficulty",
    "mongodb_query_difficulty",
    "cassandra_query_difficulty",
]

# Y-axis labels (same order/length as COLUMNS)
YTICK_LABELS = [
    "MongoDB\n(Design)",
    "Cassandra\n(Design)",
    "MongoDB\n(Query)",
    "Cassandra\n(Query)",
]

# Figure/layout (portrait for 2-col papers: ~3.4 in wide)
FIGSIZE = (3.4, 5.2)     # width, height (inches)
TOP_TITLE = ""
X_LABEL = "Difficulty"
X_LIM = (0, 11)          # x-axis range to cover 1–10 plus headroom

# Violin appearance
VIOLIN_WIDTH = 0.85      # thickness along y
VIOLIN_ALPHA = 0.75
EDGE_COLOR = "black"
EDGE_WIDTH = 1.0

# Colors per violin (cycled if fewer than columns)
PALETTE = ["#F6EFF5", "#D4CED4"]

# Grid
SHOW_GRID = True
GRID_STYLE = "--"
GRID_ALPHA = 0.4

# Raw points overlay (jitter vertically around each category row)
SHOW_POINTS = False
POINT_SIZE = 18
POINT_ALPHA = 0.35
JITTER_SCALE_Y = 0.08    # vertical jitter around the category position
RANDOM_SEED = 7          # reproducible jitter

# Baseline (vertical reference line at x = BASELINE_X)
BASELINE_X = 5
BASELINE_COLOR = "gray"
BASELINE_STYLE = "--"
BASELINE_WIDTH = 1.5
BASELINE_LABEL = ""
BASELINE_LABEL_DX = 0.1  # label x offset to the right of the line
BASELINE_LABEL_DY = 0.5  # label y offset above the top category

# Font sizes
TITLE_FONTSIZE = 6
LABEL_FONTSIZE = 6
TICK_FONTSIZE = 6
# ============================================================================

def main():
    # Load
    df = pd.read_csv(CSV_FILE)

    # Validate & collect data (numeric only)
    data_list = []
    missing = []
    for col in COLUMNS:
        if col not in df.columns:
            missing.append(col)
            continue
        vals = pd.to_numeric(df[col], errors="coerce").dropna()
        if len(vals) == 0:
            missing.append(col)
        else:
            data_list.append(vals.values)

    if not data_list:
        msg = "No valid data found."
        if missing:
            msg += " Missing/empty columns: " + ", ".join(missing)
        raise ValueError(msg)

    # Reduce labels to only those with data
    labels = [lab for col, lab in zip(COLUMNS, YTICK_LABELS)
              if col in df.columns and df[col].notna().sum() > 0]

    n = len(data_list)

    # Positions along Y (1..n). We'll invert y-axis to get top→bottom order.
    positions = np.arange(1, n + 1)

    # Figure/axes
    fig, ax = plt.subplots(figsize=FIGSIZE)

    # Horizontal violins (vert=False). Positions are on Y.
    parts = ax.violinplot(
        dataset=data_list,
        positions=positions,
        vert=False,                 # <-- key: horizontal violins
        widths=VIOLIN_WIDTH,
        showmeans=False,
        showmedians=True,
        showextrema=True,
    )

    # Style violins
    for i, body in enumerate(parts["bodies"]):
        body.set_facecolor(PALETTE[i % len(PALETTE)])
        body.set_edgecolor(EDGE_COLOR)
        body.set_linewidth(EDGE_WIDTH)
        body.set_alpha(VIOLIN_ALPHA)

    # Style medians/extrema if present
    if "cmedians" in parts:
        parts["cmedians"].set_color("black")
        parts["cmedians"].set_linewidth(1.8)
    for key in ("cmins", "cmaxes", "cbars"):
        if key in parts:
            parts[key].set_color("black")
            parts[key].set_linewidth(1.0)

    # Optional: overlay raw points (x=data, y=jitter around category row)
    if SHOW_POINTS:
        rng = np.random.default_rng(RANDOM_SEED)
        for y_pos, vals in zip(positions, data_list):
            y_jitter = rng.normal(loc=0.0, scale=JITTER_SCALE_Y, size=len(vals))
            ax.scatter(
                vals,                                   # x = actual values
                np.full_like(vals, y_pos, dtype=float) + y_jitter,  # y jitter
                s=POINT_SIZE,
                alpha=POINT_ALPHA,
                edgecolors="none",
            )

    # Baseline: vertical reference line at x = BASELINE_X
    ax.axvline(
        x=BASELINE_X,
        color=BASELINE_COLOR,
        linestyle=BASELINE_STYLE,
        linewidth=BASELINE_WIDTH,
        alpha=0.85,
    )

    # --- GROUP SEPARATOR (subtle horizontal rule) -------------------------------
    # draw a line between the 2nd and 3rd violins (i.e., between Design and Query)
    sep_index = 1  # 0-based index; draws between positions[1] and positions[2]
    sep_y = 0.5 * (positions[sep_index] + positions[sep_index + 1])

    ax.axhline(
        y=sep_y,
        color="0.4",        # gray
        linewidth=0.8,
        linestyle="-",
        alpha=0.25,         # subtle
        zorder=0,           # behind violins/points
    )

    # Baseline label (inside axes; does not change figure width)
    # place near the top category, slightly above and to the right of the line
    ax.text(
        BASELINE_X + BASELINE_LABEL_DX,
        positions[0] - BASELINE_LABEL_DY,
        BASELINE_LABEL,
        color=BASELINE_COLOR,
        fontsize=TICK_FONTSIZE,
        fontstyle="italic",
        ha="left",
        va="top",
        alpha=0.9,
    )

    # Axes, labels, ticks
    ax.set_title(TOP_TITLE, fontsize=TITLE_FONTSIZE, fontweight="bold")
    ax.set_xlabel(X_LABEL, fontsize=LABEL_FONTSIZE)
    ax.set_xlim(*X_LIM)
    ax.set_yticks(positions)
    ax.set_yticklabels(labels, fontsize=TICK_FONTSIZE)
    ax.tick_params(axis="x", labelsize=TICK_FONTSIZE)

    # Put first item at the top (top→bottom stacking)
    ax.invert_yaxis()

    # Grid on x for readability in narrow width
    if SHOW_GRID:
        ax.xaxis.grid(True, linestyle=GRID_STYLE, alpha=GRID_ALPHA)

    fig.tight_layout()
    fig.savefig(OUTPUT_FILE, format="pdf", dpi=300)
    plt.close(fig)

    print(f"Saved: {OUTPUT_FILE}")
    if missing:
        print("Note: Missing/empty columns skipped ->", ", ".join(missing))

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(f"[ERROR] {e}", file=sys.stderr)
        sys.exit(1)
