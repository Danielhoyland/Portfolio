import os
import unicodedata
from PIL import Image, ImageDraw, ImageFont

# ================= CONFIG =================

INPUT_FOLDER = "input-photos"
OUTPUT_FOLDER = "finished-photos"
LAYERS_FOLDER = "layers"

TARGET_WIDTH = 1800
TARGET_HEIGHT = 2600

FONT_PATH = "MountainsofChristmas-Bold.ttf"
EMOJI_PATH = r"C:\Windows\Fonts\seguiemj.ttf"

NAME_FONT_SIZE = 100
HEADER_FONT_SIZE = 120

TEXT_COLOR = (40, 40, 43)

LINE_SPACING = 20

# =========================================

os.makedirs(OUTPUT_FOLDER, exist_ok=True)

# Load overlay layers
layers = {
    key: Image.open(os.path.join(LAYERS_FOLDER, f"layer{key}.png")).convert("RGBA")
    for key in ["1", "2", "3", "4"]
}

font_name = ImageFont.truetype(FONT_PATH, NAME_FONT_SIZE)
font_emoji = ImageFont.truetype(EMOJI_PATH, NAME_FONT_SIZE)


# ---------- Emoji detection ----------
def is_emoji(char):
    return unicodedata.category(char) == "So" or ord(char) > 0x1F300


# ---------- Manual crop with anchor ----------
def crop_with_anchor(img, target_w, target_h, anchor="c"):
    w, h = img.size

    x_map = {
        "l": 0,
        "c": max((w - target_w) // 2, 0),
        "r": max(w - target_w, 0)
    }

    y_map = {
        "t": 0,
        "c": max((h - target_h) // 2, 0),
        "b": max(h - target_h, 0)
    }

    if len(anchor) == 2:
        y_key, x_key = anchor
    else:
        y_key, x_key = anchor, "c"

    left = x_map.get(x_key, x_map["c"])
    top = y_map.get(y_key, y_map["c"])

    return img.crop((left, top, left + target_w, top + target_h))


# ---------- Draw mixed emoji + text ----------
def draw_mixed_text(draw, position, text, font_text, font_emoji, fill, anchor="mb"):
    x, y = position

    total_width = 0
    for char in text:
        font = font_emoji if is_emoji(char) else font_text
        bbox = draw.textbbox((0, 0), char, font=font)
        total_width += bbox[2] - bbox[0]

    if "m" in anchor:
        x -= total_width // 2
    elif "r" in anchor:
        x -= total_width

    cursor_x = x
    for char in text:
        font = font_emoji if is_emoji(char) else font_text
        bbox = draw.textbbox((0, 0), char, font=font)
        char_width = bbox[2] - bbox[0]

        draw.text((cursor_x, y), char, fill=fill, font=font, anchor="ls")
        cursor_x += char_width


# ================= MAIN LOOP =================

for filename in os.listdir(INPUT_FOLDER):

    if not filename.lower().endswith((".png", ".jpg", ".jpeg")):
        continue

    layer_key = filename[0]

    if layer_key not in layers:
        print(f"Skipping {filename} (invalid layer key)")
        continue

    # Remove layer key and extension
    basename = os.path.splitext(filename)[0][1:].strip()

    tokens = basename.split()

    anchor = "c"
    zoom = 1.0

    # Check for anchor
    if tokens and tokens[0] in ["tl", "tr", "bl", "br", "t", "b", "l", "r", "c"]:
        anchor = tokens.pop(0)

    # Check for zoom
    if tokens:
        try:
            zoom = float(tokens[0])
            tokens.pop(0)
        except ValueError:
            pass

    # Clamp zoom (prevents resize crash)
    zoom = max(0.3, min(zoom, 3.0))

    name_text = " ".join(tokens)

    # -------- Open image --------
    img = Image.open(os.path.join(INPUT_FOLDER, filename)).convert("RGBA")

    # -------- Resize with zoom --------
    scaled_target_w = int(TARGET_WIDTH * zoom)
    scaled_target_h = int(TARGET_HEIGHT * zoom)

    img_ratio = img.width / img.height
    target_ratio = scaled_target_w / scaled_target_h

    if img_ratio > target_ratio:
        new_height = scaled_target_h
        new_width = int(new_height * img_ratio)
    else:
        new_width = scaled_target_w
        new_height = int(new_width / img_ratio)

    # Safety check (prevents zero-dimension crash)
    if new_width <= 0 or new_height <= 0:
        print(f"Skipping {filename} (invalid resize dimensions)")
        continue

    img = img.resize((new_width, new_height), Image.LANCZOS)

    # Crop back to final canvas size
    img = crop_with_anchor(img, TARGET_WIDTH, TARGET_HEIGHT, anchor)

    # -------- Apply overlay layer --------
    layer = layers[layer_key].resize((TARGET_WIDTH, TARGET_HEIGHT))
    img = Image.alpha_composite(img, layer)

    # -------- Draw name text --------
    draw = ImageDraw.Draw(img)
    center_x = TARGET_WIDTH // 2

    if name_text == "JAPENESE":
        name_text = "少年たち"
    name_text = name_text.replace("%", "?")

    draw_mixed_text(
        draw,
        (center_x, TARGET_HEIGHT - LINE_SPACING * 2),
        name_text,
        font_name,
        font_emoji,
        TEXT_COLOR,
        anchor="mb"
    )

    # -------- Save --------
    img.convert("RGB").save(
        os.path.join(OUTPUT_FOLDER, filename),
        quality=95
    )

    print(f"Processed: {filename}")

print("All photos processed.")