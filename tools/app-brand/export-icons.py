from pathlib import Path
from PIL import Image

root = Path(__file__).resolve().parents[2]
out = root / 'inst/app/www'
for name, size in [('mark', 512), ('reading', 600)]:
    source = Image.open(root / f'tools/app-brand/otter-{name}-source.png').convert('RGBA')
    source.resize((size, size), Image.Resampling.LANCZOS).save(out / f'rill-otter-{name}.png', optimize=True)
    if name == 'mark':
        source.resize((32, 32), Image.Resampling.LANCZOS).save(out / 'favicon-32.png', optimize=True)
        touch = Image.new('RGBA', (180, 180), '#DEECEB')
        touch.alpha_composite(source.resize((180, 180), Image.Resampling.LANCZOS))
        touch.convert('RGB').save(out / 'apple-touch-icon.png', optimize=True)
