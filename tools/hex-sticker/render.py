from pathlib import Path
from PIL import Image, ImageDraw, ImageFont, ImageStat, ImageChops
import argparse
import math

p = argparse.ArgumentParser()
p.add_argument('source')
p.add_argument('name')
p.add_argument('output')
p.add_argument('--ink', default='#142d43')
p.add_argument('--font', default='/System/Library/Fonts/Avenir Next.ttc', help='Path to a bold TTF/OTF font or font collection')
p.add_argument('--font-index', type=int, default=0, help='Face index for a font collection (Avenir Next Bold is 0)')
args = p.parse_args()
W, H = 1600, 1848
source = Image.open(args.source).convert('RGBA')
bg = tuple(round(v) for v in ImageStat.Stat(source.crop((0, 0, source.width, 35))).median)
if args.name == 'graft':
    bg = (255, 215, 163, 255)
canvas = Image.new('RGBA', (W, H), bg)
size = 1450 if args.name == 'graft' else W
art = source.resize((size, size), Image.Resampling.LANCZOS)
fade = Image.new('L', (size, size), 255)
d = ImageDraw.Draw(fade)
for y in range(110):
    d.line((0, y, size - 1, y), fill=round(255*y/109))
art.putalpha(ImageChops.multiply(art.getchannel('A'), fade))
canvas.alpha_composite(art, ((W-size)//2, 125 if args.name != 'tempest' else 205))
draw = ImageDraw.Draw(canvas)
draw.rectangle((0, 1360, W, 1626), fill='#fff5db')
try:
    font = ImageFont.truetype(args.font, 218, index=args.font_index)
except OSError:
    p.error('Cannot load font. Pass --font /path/to/bold-font.ttf and, for a font collection, --font-index N. The default Avenir Next font is available on macOS.')
draw.text((W/2, 1480), args.name, font=font, anchor='mm', fill=args.ink, stroke_width=1)
# A ring between concentric regular hexagons gives every edge the same
# perpendicular thickness and closes all six corners without stroke clipping.
radius = min((W - 16) / math.sqrt(3), (H - 16) / 2)
inner_radius = radius - 30 / math.cos(math.pi / 6)

def hex_mask(r):
    scale = 4
    mask = Image.new('L', (W * scale, H * scale), 0)
    points = [
        ((W / 2 + r * math.sin(i * math.pi / 3)) * scale,
         (H / 2 - r * math.cos(i * math.pi / 3)) * scale)
        for i in range(6)
    ]
    ImageDraw.Draw(mask).polygon(points, fill=255)
    return mask.resize((W, H), Image.Resampling.LANCZOS)

border = Image.new('RGBA', (W, H), args.ink)
canvas = Image.composite(canvas, border, hex_mask(inner_radius))
canvas.putalpha(hex_mask(radius))
out = Path(args.output)
out.parent.mkdir(parents=True, exist_ok=True)
canvas.save(out, dpi=(600,600))
canvas.resize((400,462), Image.Resampling.LANCZOS).save(out.with_name('logo.png'))
print(out)
