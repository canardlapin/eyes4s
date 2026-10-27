from pathlib import Path
import argparse,sys,json,math,hashlib,collections
ap=argparse.ArgumentParser();ap.add_argument('--reference',type=Path,required=True);ap.add_argument('--repo',type=Path,required=True);args=ap.parse_args()
sys.path.insert(0,str(args.repo/'tools/private-fixtures'))
from run import Tolerance,outside_git
import end_to_end as oracle
root=outside_git(args.reference);manifest=json.loads((root/'manifest.json').read_text());stats=collections.Counter();maximum=collections.defaultdict(float);all_scores=[];worst=None
map_tol=Tolerance(absolute=2e-13,relative=2e-12);score_tol=Tolerance(absolute=2e-12,relative=2e-12)
for entry in manifest['participants']:
 path=root/entry['file'];assert hashlib.sha256(path.read_bytes()).hexdigest()==entry['sha256']
 p=json.loads(path.read_text());r=json.loads((root/entry['output']).read_text());reference={m['id']:m for m in r['maps']};native={m['id']:m['eyes4s']for m in p['maps']};assert set(reference)==set(native)
 plan=dict(x_min=0,x_max=p['width_px'],y_min=0,y_max=p['height_px'],nx=p['nx'],ny=p['ny'],sigma_px=p['sigma_px'])
 xs,ys=oracle.axes(plan);per=[]
 for item in p['maps']:
  ident=item['id'];found=reference[ident]
  expected=oracle.gaussian(plan,item,xs,ys,quantize=True,truncate=True)
  oracle.assert_close(native[ident],expected,map_tol,'eyes4s grid Gaussian')
  maximum['eyes4s_map_oracle']=max(maximum['eyes4s_map_oracle'],max(abs(a-b)for a,b in zip(native[ident],expected)))
  expected_r=oracle.gaussian(plan,item,found['x'],found['y'],support_sigma=3.7)
  digits=max(0,math.floor(7-math.log10(max(expected_r))+.5));rounding=Tolerance(absolute=.50001*10**-digits+2e-13,relative=0)
  oracle.assert_close(found['values'],expected_r,rounding,'eyesim finite-support Gaussian with documented rounding')
  maximum['eyesim_map_oracle']=max(maximum['eyesim_map_oracle'],max(abs(a-b)for a,b in zip(found['values'],expected_r)))
  maximum['native_map_abs']=max(maximum['native_map_abs'],max(abs(a-b)for a,b in zip(native[ident],oracle.normalized(found['values']))))
  stats['maps']+=1
 native_score=oracle.score_function(native);r_score=oracle.score_function({i:m['values']for i,m in reference.items()});actual={s['id']:s for s in r['scores']};assert set(actual)=={s['id']for s in p['scores']}
 for row in p['scores']:
  found=actual[row['id']];assert row['matched']==found['matched'] and row['controls']==found['controls']
  def values(score):
   m=score(row['id'],row['matched']);b=math.fsum(score(row['id'],ref)for ref in row['controls'])/len(row['controls']);return[m,b,m-b]
  e4=values(native_score);er=values(r_score);api=[row['api'][n][0]for n in ['m','b','d']]
  oracle.assert_close(e4,api,score_tol,'independent pair/control enumeration vs API')
  oracle.assert_close([found['eyesim_on_eyes4s'][n]for n in ['observed','baseline','difference']],api,score_tol,'eyesim scores on identical eyes4s arrays')
  oracle.assert_close([found['eyesim'][n]for n in ['observed','baseline','difference']],er,score_tol,'independent scores vs eyesim API')
  maximum['api_score_oracle']=max(maximum['api_score_oracle'],max(abs(a-b)for a,b in zip(e4,api)))
  maximum['eyesim_on_eyes4s_score']=max(maximum['eyesim_on_eyes4s_score'],max(abs(found['eyesim_on_eyes4s'][n]-v)for n,v in zip(['observed','baseline','difference'],api)))
  maximum['eyesim_score_oracle']=max(maximum['eyesim_score_oracle'],max(abs(found['eyesim'][n]-v)for n,v in zip(['observed','baseline','difference'],er)))
  gap=er[2]-api[2];maximum['native_contrast_abs']=max(maximum['native_contrast_abs'],abs(gap));stats['queries']+=1;stats['selected_pairs']+=1+len(row['controls']);stats['opposite_contrast_signs']+=api[2]*er[2]<0
  summary=dict(participant=p['participant'],id=row['id'],controls=len(row['controls']),eyes4s=dict(zip(['observed','baseline','difference'],api)),eyesim=dict(zip(['observed','baseline','difference'],er)),contrast_gap=gap)
  all_scores.append(summary)
  if worst is None or abs(gap)>worst['gap']:worst={'gap':abs(gap),'entry':entry,'query':row,'summary':summary}
 stats['participants']+=1
 (root/'verification-progress.json').write_text(json.dumps(dict(stats,maximum=dict(maximum)),indent=2)+'\n')
 print('verified participant',entry['index'],flush=True)
# Stage the largest native contrast difference without selecting a favourable example.
p=json.loads((root/worst['entry']['file']).read_text());r=json.loads((root/worst['entry']['output']).read_text());row=worst['query'];needed={row['id'],row['matched'],*row['controls']};items={m['id']:m for m in p['maps']if m['id']in needed};rr={m['id']:m for m in r['maps']if m['id']in needed}
plan=dict(x_min=0,x_max=p['width_px'],y_min=0,y_max=p['height_px'],nx=p['nx'],ny=p['ny'],sigma_px=p['sigma_px']);xs,ys=oracle.axes(plan)
stages=[]
for name in ['eyesim_actual','endpoints_finite','endpoints_full','centers_full','binned_centers_full','binned_centers_truncated','eyes4s_actual']:
 maps={}
 for ident,item in items.items():
  z=rr[ident]
  if name=='eyesim_actual':values=z['values']
  elif name=='endpoints_finite':values=oracle.gaussian(plan,item,z['x'],z['y'],support_sigma=3.7)
  elif name=='endpoints_full':values=oracle.gaussian(plan,item,z['x'],z['y'])
  elif name=='eyes4s_actual':values=item['eyes4s']
  else:values=oracle.gaussian(plan,item,xs,ys,quantize=name.startswith('binned'),truncate=name.endswith('truncated'))
  maps[ident]=values
 score=oracle.score_function(maps);m=score(row['id'],row['matched']);b=math.fsum(score(row['id'],ref)for ref in row['controls'])/len(row['controls']);d=m-b
 stages.append({'stage':name,'observed':m,'baseline':b,'difference':d,'change_from_previous':None if not stages else d-stages[-1]['difference']})
(root/'largest-disagreement.json').write_text(json.dumps({'selection':'largest absolute native contrast difference across every contributing query','query':worst['summary'],'stages':stages},indent=2)+'\n')
(root/'scores.json').write_text(json.dumps(all_scores,separators=(',',':'))+'\n')
means=[]
for participant in sorted({x['participant']for x in all_scores}):
 rows=[x for x in all_scores if x['participant']==participant];means.append({'participant':participant,'queries':len(rows),**{e:math.fsum(x[e]['difference']for x in rows)/len(rows)for e in ['eyes4s','eyesim']}})
result={'schema':1,'status':'passed','counts':dict(stats),'maximum_absolute':dict(maximum),'map_tolerance':map_tol.__dict__,'score_tolerance':score_tol.__dict__,'eyesim_rounding':'Per-map tolerance determined by documented zapsmall(digits=7), not widened to fit observations.','participant_means':means,'equal_participant_mean':{e:math.fsum(x[e]for x in means)/len(means)for e in ['eyes4s','eyesim']},'scope':'Spatial software validation on declared computational viewport; endpoint and cell-center grids remain different. No calibrated angles, timing provenance, memory inference or manuscript replication claimed. Two all-offscreen trial maps remain explicit failures, without imputation.'}
(root/'verification.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps({'status':'passed','counts':dict(stats),'maximum_absolute':dict(maximum)}))
