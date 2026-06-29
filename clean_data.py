#!/usr/bin/env python3
from __future__ import annotations

import re
from pathlib import Path
from typing import Iterable

import pandas as pd


INPUT_FILE = Path("/Users/solenenoize/Desktop/sleep_analysis/data/merge_data.xlsx")
OUTPUT_FILE = Path("/Users/solenenoize/Desktop/sleep_analysis/data/merge_data_cleaned.xlsx")


def normalize_text(value: object) -> str:
    text = str(value).strip().lower()
    text = re.sub(r"[\r\n\t]+", " ", text)
    text = re.sub(r"\s+", " ", text)
    return text


def find_single_column(columns: Iterable[str], patterns: list[str], label: str) -> str:
    cols = list(columns)
    normalized = [normalize_text(c) for c in cols]
    matches: list[str] = []

    for col_name, col_norm in zip(cols, normalized):
        if any(re.search(pat, col_norm) for pat in patterns):
            matches.append(col_name)

    if not matches:
        raise ValueError(f"Could not find required column for '{label}'.")
    if len(matches) > 1:
        joined = " | ".join(matches)
        raise ValueError(f"Multiple columns matched '{label}': {joined}")
    return matches[0]


def find_optional_column(columns: Iterable[str], patterns: list[str]) -> str | None:
    cols = list(columns)
    normalized = [normalize_text(c) for c in cols]
    matches: list[str] = []

    for col_name, col_norm in zip(cols, normalized):
        if any(re.search(pat, col_norm) for pat in patterns):
            matches.append(col_name)

    if not matches:
        return None
    if len(matches) > 1:
        joined = " | ".join(matches)
        raise ValueError(f"Multiple optional columns matched: {joined}")
    return matches[0]


def time_asleep_to_minutes(value: object) -> float | None:
    if pd.isna(value):
        return None

    if isinstance(value, (int, float)):
        # Requested conversion is from hours to minutes.
        return float(value) * 60.0

    text = str(value).strip()
    if not text:
        return None

    text_norm = normalize_text(text)

    # Handle explicit hour/minute strings like "8h", "8 h 30", "8.5 hours".
    hour_match = re.fullmatch(r"(\d+(?:\.\d+)?)\s*h(?:ours?)?", text_norm)
    if hour_match:
        return float(hour_match.group(1)) * 60.0

    hour_min_match = re.fullmatch(r"(\d+(?:\.\d+)?)\s*h(?:ours?)?\s*(\d+)\s*m(?:in(?:ute)?s?)?", text_norm)
    if hour_min_match:
        hours = float(hour_min_match.group(1))
        minutes = float(hour_min_match.group(2))
        return hours * 60.0 + minutes

    # Handle HH:MM[:SS] strings.
    td = pd.to_timedelta(text, errors="coerce")
    if pd.notna(td):
        return float(td.total_seconds() / 60.0)

    # Last attempt: numeric string interpreted as hours.
    numeric = pd.to_numeric(text, errors="coerce")
    if pd.notna(numeric):
        return float(numeric) * 60.0

    return None


def wake_duration_to_minutes(value: object) -> float | None:
    if pd.isna(value):
        return None
    text = normalize_text(value)
    if not text:
        return None

    # Midpoint mapping for range labels.
    if re.search(r"^0\s*-\s*15", text):
        return 7.5
    if re.search(r"^15\s*-\s*30", text):
        return 22.5
    if re.search(r"^30\s*-\s*45", text):
        return 37.5
    if re.search(r"^45\s*-\s*60", text):
        return 52.5

    # If already numeric, keep as-is.
    numeric = pd.to_numeric(text, errors="coerce")
    if pd.notna(numeric):
        return float(numeric)
    return None


def sleep_quality_to_continuous(value: object) -> float | None:
    if pd.isna(value):
        return None
    text = normalize_text(value)
    if not text:
        return None

    numeric = pd.to_numeric(text, errors="coerce")
    if pd.notna(numeric):
        return float(numeric)

    if re.search(r"^very poor$|^very bad$", text):
        return 1.0
    if re.search(r"^poor$|^bad$", text):
        return 2.0
    if re.search(r"^fair$|^ok$|^okay$|^average$|^neutral$", text):
        return 3.0
    if re.search(r"^good$", text):
        return 4.0
    if re.search(r"^very good$|^excellent$", text):
        return 5.0
    return None


def normalize_gender(value: object) -> str | None:
    if pd.isna(value):
        return None
    text = normalize_text(value)
    if not text:
        return None
    if text == "female":
        return "Female"
    if text == "male":
        return "Male"
    return "Other/Prefer Not to Say"


def compute_arousal_rate(arousal_n: pd.Series, total_cues: pd.Series) -> pd.Series:
    arousal_num = pd.to_numeric(arousal_n, errors="coerce")
    total_num = pd.to_numeric(total_cues, errors="coerce")
    return arousal_num.div(total_num.where(total_num > 0))


def binarize_positive(series: pd.Series) -> pd.Series:
    numeric = pd.to_numeric(series, errors="coerce")
    out = pd.Series(pd.NA, index=series.index, dtype="Int64")
    non_missing = numeric.notna()
    out.loc[non_missing] = (numeric.loc[non_missing] > 0).astype("Int64")
    return out


PHONE_MOTION_COLS = [
    "overall_motion",
    "stimulation_motion",
    "cue_delta_var",
    "high_freq",
    "low_freq",
]

FITBIT_MOTION_COLS = [
    "fitbit_overall_motion",
    "fitbit_stim_motion",
]

FITBIT_MOTION_INDICATOR = "fitbit_overall_motion"


def weekly_lucid_counts_to_daily_freq(df: pd.DataFrame) -> pd.DataFrame:
    """Convert past-week lucid counts to daily rates (count / 7)."""
    out = df.copy()
    conversions = [
        ("lucid_dreams_past_week", "lucid_dreams_freq_pw"),
        ("lucid_attempts_past_week", "lucid_attempts_freq_pw"),
    ]
    for source_col, target_col in conversions:
        if source_col not in out.columns:
            continue
        out[target_col] = pd.to_numeric(out[source_col], errors="coerce") / 7.0
        out = out.drop(columns=[source_col])
    return out


def separate_phone_and_fitbit_motion(df: pd.DataFrame) -> tuple[pd.DataFrame, dict[str, int]]:
    """Keep phone motion or Fitbit motion per row, never both."""
    out = df.copy()
    cleared: dict[str, int] = {}

    if FITBIT_MOTION_INDICATOR not in out.columns:
        return out, cleared

    phone_cols = [col for col in PHONE_MOTION_COLS if col in out.columns]
    fitbit_cols = [col for col in FITBIT_MOTION_COLS if col in out.columns]
    fitbit_mask = out[FITBIT_MOTION_INDICATOR].notna()

    for col in phone_cols:
        clear_mask = fitbit_mask & out[col].notna()
        cleared[f"phone:{col}"] = int(clear_mask.sum())
        out.loc[clear_mask, col] = pd.NA

    for col in fitbit_cols:
        clear_mask = (~fitbit_mask) & out[col].notna()
        cleared[f"fitbit:{col}"] = int(clear_mask.sum())
        out.loc[clear_mask, col] = pd.NA

    return out, cleared


def main() -> None:
    if not INPUT_FILE.exists():
        raise FileNotFoundError(f"Input file not found: {INPUT_FILE}")

    df = pd.read_excel(INPUT_FILE)

    time_asleep_col = find_single_column(
        df.columns,
        [r"^time asleep$"],
        "Time asleep",
    )
    wake_duration_col = find_single_column(
        df.columns,
        [r"how long did you wake up", r"if you woke up one or more times.*how long"],
        "wake up duration",
    )
    sleep_quality_col = find_single_column(
        df.columns,
        [r"^sleep quality$", r"how would you rate your sleep quality"],
        "sleep quality",
    )

    # Standardize key analysis column names once here, so downstream GLMM scripts
    # can use stable names directly.
    rename_specs: list[tuple[list[str], str]] = [
        ([r"were you at any point lucid"], "lucid"),
        ([r"did you notice any cues"], "cue_notice"),
        ([r"^pid$"], "pid"),
        ([r"^wakethresh$"], "wakeThresh"),
        ([r"^highestvol$"], "highestVol"),
        ([r"^arousaln$"], "arousalN"),
        ([r"^totalcues$"], "totalCues"),
        ([r"how many lucid dreams.*past week"], "lucid_dreams_past_week"),
        ([r"lucid dreaming attempts in past week"], "lucid_attempts_past_week"),
        ([r"^what is your age\??$"], "age"),
        ([r"^what is your gender\??$"], "gender"),
    ]

    rename_map: dict[str, str] = {}
    for patterns, target_name in rename_specs:
        col = find_optional_column(df.columns, patterns)
        if col is not None:
            rename_map[col] = target_name

    df[time_asleep_col] = df[time_asleep_col].apply(time_asleep_to_minutes)
    df[wake_duration_col] = df[wake_duration_col].apply(wake_duration_to_minutes)
    df[sleep_quality_col] = df[sleep_quality_col].apply(sleep_quality_to_continuous)
    rename_map[time_asleep_col] = "time_asleep"
    rename_map[wake_duration_col] = "wake_up_duration"
    rename_map[sleep_quality_col] = "sleep_quality"

    df = df.rename(columns=rename_map)
    df = weekly_lucid_counts_to_daily_freq(df)
    if "gender" in df.columns:
        df["gender"] = df["gender"].apply(normalize_gender)
    if "arousalN" in df.columns and "totalCues" in df.columns:
        df["arousal_rate"] = compute_arousal_rate(df["arousalN"], df["totalCues"])
        df["cued_night"] = binarize_positive(df["totalCues"])
        df["had_arousal"] = binarize_positive(df["arousalN"])

    df, motion_cleared = separate_phone_and_fitbit_motion(df)

    OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)
    df.to_excel(OUTPUT_FILE, index=False)

    print(f"Saved: {OUTPUT_FILE}")
    print(f"Updated in place columns: {time_asleep_col}, {wake_duration_col}, {sleep_quality_col}")
    if motion_cleared:
        print("Separated phone vs Fitbit motion (cleared overlapping values):")
        for label, count in sorted(motion_cleared.items()):
            if count > 0:
                print(f"  {label}: {count}")
    else:
        print("No phone/Fitbit motion columns found to separate.")
    print("Lucid past-week counts converted to daily rates:")
    print("  lucid_dreams_past_week -> lucid_dreams_freq_pw (count / 7)")
    print("  lucid_attempts_past_week -> lucid_attempts_freq_pw (count / 7)")
    print("Renamed columns:")
    for src, dst in sorted(rename_map.items(), key=lambda x: x[1]):
        print(f"  {src} -> {dst}")


if __name__ == "__main__":
    main()
