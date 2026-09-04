from pathlib import Path
from PIL import Image

root = Path(__file__).resolve().parents[2]
icons = root / 'pkgdown/favicon'
for filename, size in [('favicon-32x32.png', 32), ('apple-touch-icon.png', 180), ('android-chrome-512x512.png', 512)]:
    image = Image.open(icons / filename).convert('RGBA')
    assert image.size == (size, size), filename
    if filename == 'apple-touch-icon.png':
        assert image.getchannel('A').getextrema() == (255, 255)
    else:
        assert image.getpixel((0, 0))[3] == 0, filename
    if size == 512:
        bounds = image.getchannel('A').getbbox()
        assert bounds[3] - bounds[1] > 490, '512px icon must use the high-resolution source'
app_icon = root / 'inst/app/www/apple-touch-icon.png'
if app_icon.exists():
    assert Image.open(app_icon).convert('RGBA').getchannel('A').getextrema() == (255, 255)
print('Icon dimensions, opaque touch icons, and full-resolution output verified.')
