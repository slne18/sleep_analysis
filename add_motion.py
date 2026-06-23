#!/usr/bin/env python3
from __future__ import annotations

import argparse
import sys
from pathlib import Path
from typing import Iterable

import pandas as pd


def normalize_name(name: str) -> str:
    return "".join(ch.lower() for ch in str(name) if ch.isalnum())


def pick_column(columns: Iterable[str], candidates: list[str]) -> str | None:
    normalized_to_original = {normalize_name(col): col for col in columns}
    for candidate in candidates:
        key = normalize_name(candidate)
        if key in normalized_to_original:
            return normalized_to_original[key]
    return None


def read_table(path: Path) -> pd.DataFrame:
    suffix = path.suffix.lower()
    if suffix in {".xlsx", ".xls"}:
        return pd.read_excel(path)
    if suffix == ".csv":
        return pd.read_csv(path)
    raise ValueError(f"Unsupported file type: {suffix}. Use .xlsx/.xls/.csv")


def write_table(df: pd.DataFrame, path: Path) -> None:
    suffix = path.suffix.lower()
    path.parent.mkdir(parents=True, exist_ok=True)
    if suffix in {".xlsx", ".xls"}:
        df.to_excel(path, index=False)
        return
    if suffix == ".csv":
        df.to_csv(path, index=False)
        return
    raise ValueError(f"Unsupported output file type: {suffix}. Use .xlsx/.xls/.csv")


def get_pid_night_columns(df: pd.DataFrame, pid_col: str | None, night_col: str | None) -> tuple[str | None, str | None]:
    picked_pid = pid_col or pick_column(df.columns, ["pid", "participant_id", "user_id"])
    picked_night = night_col or pick_column(
        df.columns, ["night", "night_number", "nightnum", "night_index", "nightid"]
    )
    return picked_pid, picked_night


def print_available_columns(df: pd.DataFrame) -> None:
    print("Available columns:", file=sys.stderr)
    for col in df.columns:
        print(f"- {col}", file=sys.stderr)


def check_duplicates(
    df: pd.DataFrame, pid_col: str, night_col: str
) -> tuple[pd.DataFrame, pd.DataFrame]:
    keys = [pid_col, night_col]
    duplicate_mask = df.duplicated(subset=keys, keep=False)
    duplicates = df.loc[duplicate_mask].copy()
    duplicate_groups = (
        duplicates.groupby(keys, dropna=False).size().reset_index(name="row_count").sort_values(
            "row_count", ascending=False
        )
    )
    return duplicates, duplicate_groups


def main() -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Check duplicate rows for (pid, night), and optionally merge motion columns "
            "into another dataset."
        )
    )
    parser.add_argument(
        "--input",
        default="data/motion_summary.csv",
        help="Input data file (.xlsx/.xls/.csv). Default: data/motion_summary.csv",
    )
    parser.add_argument(
        "--pid-col",
        default=None,
        help="Optional explicit pid column name. If omitted, autodetects.",
    )
    parser.add_argument(
        "--night-col",
        default=None,
        help="Optional explicit night column name. If omitted, autodetects.",
    )
    parser.add_argument(
        "--save-duplicates",
        default=None,
        help="Optional path to save duplicate rows as CSV.",
    )
    parser.add_argument(
        "--merge-target",
        default=None,
        help="Optional target file (.xlsx/.xls/.csv) to receive motion columns by (pid, night).",
    )
    parser.add_argument(
        "--output",
        default=None,
        help="Optional output path for merged target. Default: overwrite --merge-target.",
    )
    parser.add_argument(
        "--target-pid-col",
        default=None,
        help="Optional explicit pid column in merge target. If omitted, autodetects.",
    )
    parser.add_argument(
        "--target-night-col",
        default=None,
        help="Optional explicit night column in merge target. If omitted and missing, infer by pid order.",
    )
    parser.add_argument(
        "--infer-target-night",
        action="store_true",
        default=True,
        help="Infer target night number per pid with cumcount if target night column is missing (default: enabled).",
    )
    args = parser.parse_args()

    input_path = Path(args.input)
    if not input_path.exists():
        print(f"Error: input file not found: {input_path}", file=sys.stderr)
        return 1

    df = read_table(input_path)
    if df.empty:
        print("Input file has no rows.")
        return 0

    pid_col, night_col = get_pid_night_columns(df, args.pid_col, args.night_col)

    if pid_col is None or night_col is None:
        print("Could not detect required columns for duplicate check.", file=sys.stderr)
        print(f"Detected pid column: {pid_col}", file=sys.stderr)
        print(f"Detected night column: {night_col}", file=sys.stderr)
        print_available_columns(df)
        return 2

    duplicates, duplicate_groups = check_duplicates(df, pid_col, night_col)

    print(f"Input file: {input_path}")
    print(f"Using pid column: {pid_col}")
    print(f"Using night column: {night_col}")
    print(f"Total rows: {len(df)}")
    print(f"Rows part of duplicate (pid, night) groups: {len(duplicates)}")
    print(f"Number of duplicate (pid, night) groups: {len(duplicate_groups)}")

    if len(duplicate_groups) == 0:
        print("No duplicates found for (pid, night).")
    else:
        print("\nTop duplicate groups:")
        print(duplicate_groups.head(20).to_string(index=False))

    if args.save_duplicates:
        out_path = Path(args.save_duplicates)
        out_path.parent.mkdir(parents=True, exist_ok=True)
        duplicates.to_csv(out_path, index=False)
        print(f"\nSaved duplicate rows to: {out_path}")

    if args.merge_target:
        cue_delta_col = pick_column(
            df.columns,
            [
                "cue_delta_variability_std",
                "cue_delta_variability",
                "cued_delta_variability",
                "cue_delta variability",
                "cued_delta variability",
            ],
        )
        if cue_delta_col is None:
            print(
                "Missing required cue-delta variability column in input. "
                "Tried common names like cue_delta_variability_std / cue_delta_variability.",
                file=sys.stderr,
            )
            return 3

        fitbit_overall_col = pick_column(
            df.columns,
            ["fitbit_overall_motion", "fitbit_wrist_overall_motion"],
        )
        fitbit_stim_col = pick_column(
            df.columns,
            ["fitbit_stim_motion", "fitbit_wrist_stim_motion"],
        )
        high_freq_col = pick_column(
            df.columns,
            ["high_freq_std", "high_freq"],
        )
        low_freq_col = pick_column(
            df.columns,
            ["low_freq_std", "low_freq"],
        )
        if fitbit_overall_col is None or fitbit_stim_col is None:
            print(
                "Missing required Fitbit motion columns in input. "
                "Tried fitbit_overall_motion / fitbit_wrist_overall_motion and "
                "fitbit_stim_motion / fitbit_wrist_stim_motion.",
                file=sys.stderr,
            )
            return 3
        if high_freq_col is None or low_freq_col is None:
            print(
                "Missing required frequency columns in input. "
                "Tried high_freq_std / high_freq and low_freq_std / low_freq.",
                file=sys.stderr,
            )
            return 3

        motion_cols = [
            "overall_motion",
            "stimulation_motion",
            cue_delta_col,
            fitbit_overall_col,
            fitbit_stim_col,
            high_freq_col,
            low_freq_col,
        ]
        for col in motion_cols:
            if col not in df.columns:
                print(f"Missing required motion column in input: {col}", file=sys.stderr)
                return 4

        motion_df = df[[pid_col, night_col] + motion_cols].copy()
        if cue_delta_col != "cue_delta_var":
            motion_df = motion_df.rename(columns={cue_delta_col: "cue_delta_var"})
        if fitbit_overall_col != "fitbit_overall_motion":
            motion_df = motion_df.rename(columns={fitbit_overall_col: "fitbit_overall_motion"})
        if fitbit_stim_col != "fitbit_stim_motion":
            motion_df = motion_df.rename(columns={fitbit_stim_col: "fitbit_stim_motion"})
        if high_freq_col != "high_freq":
            motion_df = motion_df.rename(columns={high_freq_col: "high_freq"})
        if low_freq_col != "low_freq":
            motion_df = motion_df.rename(columns={low_freq_col: "low_freq"})
        if len(duplicate_groups) > 0:
            # Keep one row per (pid, night) if duplicates exist.
            motion_df = motion_df.drop_duplicates(subset=[pid_col, night_col], keep="first")
            print(
                "Warning: duplicate motion keys found; kept first row for each (pid, night) during merge."
            )

        target_path = Path(args.merge_target)
        if not target_path.exists():
            print(f"Merge target file not found: {target_path}", file=sys.stderr)
            return 5
        target_df = read_table(target_path)

        target_pid_col, target_night_col = get_pid_night_columns(
            target_df, args.target_pid_col, args.target_night_col
        )
        if target_pid_col is None:
            print("Could not detect pid column in merge target.", file=sys.stderr)
            print_available_columns(target_df)
            return 6

        inferred_night_col = None
        if target_night_col is None:
            if not args.infer_target_night:
                print(
                    "No target night column found and inference disabled. "
                    "Provide --target-night-col.",
                    file=sys.stderr,
                )
                print_available_columns(target_df)
                return 7
            inferred_night_col = "__inferred_night_number__"
            target_df[inferred_night_col] = target_df.groupby(target_pid_col).cumcount()
            target_night_col = inferred_night_col
            print(
                "Target night column not found; using inferred night number per pid "
                "(row order within each pid)."
            )

        motion_for_merge = motion_df.rename(
            columns={pid_col: "__motion_pid__", night_col: "__motion_night__"}
        )
        # Avoid suffix collisions by dropping old motion columns before merge.
        target_df = target_df.drop(
            columns=[
                "overall_motion",
                "stimulation_motion",
                "cue_delta_var",
                "cue_delta_variability_std",
                "fitbit_overall_motion",
                "fitbit_stim_motion",
                "high_freq",
                "low_freq",
                "high_frequency_motion",
                "low_frequency_motion",
            ],
            errors="ignore",
        )
        merged_df = target_df.merge(
            motion_for_merge,
            left_on=[target_pid_col, target_night_col],
            right_on=["__motion_pid__", "__motion_night__"],
            how="left",
        )
        merged_df = merged_df.drop(columns=["__motion_pid__", "__motion_night__"], errors="ignore")
        if inferred_night_col is not None:
            merged_df = merged_df.drop(columns=[inferred_night_col], errors="ignore")

        out_path = Path(args.output) if args.output else target_path
        write_table(merged_df, out_path)

        matched_rows = int(merged_df["overall_motion"].notna().sum())
        print("\nMerge completed.")
        print(f"Merge target: {target_path}")
        print(f"Merge output: {out_path}")
        print(f"Rows with matched overall_motion: {matched_rows}/{len(merged_df)}")
        matched_cue_delta_rows = int(merged_df["cue_delta_var"].notna().sum())
        matched_fitbit_overall_rows = int(merged_df["fitbit_overall_motion"].notna().sum())
        matched_fitbit_stim_rows = int(merged_df["fitbit_stim_motion"].notna().sum())
        print(f"Rows with matched cue_delta_var: {matched_cue_delta_rows}/{len(merged_df)}")
        print(f"Rows with matched fitbit_overall_motion: {matched_fitbit_overall_rows}/{len(merged_df)}")
        print(f"Rows with matched fitbit_stim_motion: {matched_fitbit_stim_rows}/{len(merged_df)}")
        high_freq_rows = int(merged_df["high_freq"].notna().sum())
        low_freq_rows = int(merged_df["low_freq"].notna().sum())
        print(f"Rows with matched high_freq: {high_freq_rows}/{len(merged_df)}")
        print(f"Rows with matched low_freq: {low_freq_rows}/{len(merged_df)}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
