from pathlib import Path
from PIL import Image
import base64
import json

root = Path(__file__).resolve().parents[2]
source = Image.open(root / 'man/figures/logo.png').convert('RGBA')
out = root / 'pkgdown/favicon'
out.mkdir(parents=True, exist_ok=True)
for name, size in [('favicon-96x96.png',96),('favicon-16x16.png',16),('favicon-32x32.png',32),('apple-touch-icon.png',180),('android-chrome-192x192.png',192),('android-chrome-512x512.png',512)]:
    icon = source.copy()
    icon.thumbnail((size,size), Image.Resampling.LANCZOS)
    canvas = Image.new('RGBA',(size,size))
    canvas.alpha_composite(icon,((size-icon.width)//2,(size-icon.height)//2))
    canvas.save(out / name, optimize=True)
Image.open(out / 'android-chrome-512x512.png').save(out / 'favicon.ico', sizes=[(16,16),(32,32),(48,48)])

encoded = base64.b64encode((out / 'favicon-96x96.png').read_bytes()).decode('ascii')
(out / 'favicon.svg').write_text('<svg xmlns="http://www.w3.org/2000/svg" width="96" height="96" viewBox="0 0 96 96"><image width="96" height="96" href="data:image/png;base64,' + encoded + '"/></svg>\n')
(out / 'site.webmanifest').write_text(json.dumps({'name': root.name.removesuffix('-branding'), 'icons': [{'src': 'android-chrome-192x192.png', 'sizes': '192x192', 'type': 'image/png'}, {'src': 'android-chrome-512x512.png', 'sizes': '512x512', 'type': 'image/png'}], 'display': 'standalone'}, indent=2) + '\n')
