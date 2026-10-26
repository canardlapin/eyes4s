from pathlib import Path
from PIL import Image,ImageChops
import json,sys
root=Path(sys.argv[1]);out=Path(sys.argv[2]);checks=[]
for p in sorted((root/'target/studio-snapshots/VisualParitySuite').glob('*/review.json')):
 j=json.loads(p.read_text());assert j['mode']=='linux-monocle-goldens'
 for c in j['captures']:
  a=Image.open(root/c['actual']).convert('RGBA');b=Image.open(root/c['baseline']).convert('RGBA');assert a.size==b.size
  d=ImageChops.difference(a,b);m=d.getchannel(0)
  for k in (1,2,3):m=ImageChops.lighter(m,d.getchannel(k))
  h=m.histogram();changed=sum(h[1:]);maximum=max(i for i,n in enumerate(h) if n)
  r=c['comparison'];assert changed==r['changedPixels'] and maximum==r['maximumChannelDelta'],(j['state'],c['actual'])
  assert maximum<=1 and changed/(a.width*a.height)<=.001
  checks.append({'state':j['state'],'file':c['actual'],'changedPixels':changed,'maximumChannelDelta':maximum,'width':a.width,'height':a.height})
assert len(checks)==60
out.write_text(json.dumps({'schema':1,'method':'Independent Pillow RGBA channel comparison against committed reviewed baselines','captures':checks,'exact':sum(x['changedPixels']==0 for x in checks),'passed':True},indent=2)+'\n')
print('Independently verified60 PNG comparisons;',sum(x['changedPixels']==0 for x in checks),'exact')
