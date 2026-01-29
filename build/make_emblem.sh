#!/usr/bin/env bash

set -euo pipefail

INPUT="$1"
TARGET_DIR="$2"

if [[ ! -f "$INPUT" ]]; then
  echo "Error: Input file does not exist: $INPUT"
  exit 1
fi


TMPFILE=$(mktemp /tmp/emblem.XXXXXX.png)
CRUSHED="$TMPFILE"

# Step 1: crush and optimize original
echo "Optimizing original"
pngcrush -rem allb -reduce "$INPUT" "$CRUSHED" >/dev/null 2>&1
optipng -quiet "$CRUSHED"

# Step 2: resize to 128x128 and 64x64 using ImageMagick
echo "Resizing to 64x64 and 128x128"
convert "$CRUSHED" -resize 128x128 -background none -gravity center -extent 128x128 "$TARGET_DIR/emblem128.png"
convert "$CRUSHED" -resize 64x64  -background none -gravity center -extent 64x64  "$TARGET_DIR/emblem64.png"

# Step 3: re-optimize resized versions
echo "Optimizing resized versions"
pngcrush -rem allb -reduce "$TARGET_DIR/emblem128.png" "$TARGET_DIR/emblem128.crushed.png" >/dev/null 2>&1
optipng -quiet "$TARGET_DIR/emblem128.crushed.png"
mv -f "$TARGET_DIR/emblem128.crushed.png" "$TARGET_DIR/emblem128.png"

pngcrush -rem allb -reduce "$TARGET_DIR/emblem64.png" "$TARGET_DIR/emblem64.crushed.png" >/dev/null 2>&1
optipng -quiet "$TARGET_DIR/emblem64.crushed.png"
mv -f "$TARGET_DIR/emblem64.crushed.png" "$TARGET_DIR/emblem64.png"

echo "✅ Emblems saved in: $TARGET_DIR"
rm -f "$TMPFILE"
