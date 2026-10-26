from pathlib import Path
import hashlib,json,re,shutil,stat,subprocess,xml.etree.ElementTree as ET,zipfile
S=Path('/private/tmp/eyes4s-usability-release4-20261026')
O=Path('/private/tmp/eyes4s-usability-final-proof')
SHA='ecf67c3f9efaad0a8671f8482018cc8363b68036'
TREE='0b6114908728f13f83bd38a3a8de51686fa9584a'
def digest(p):return hashlib.sha256(p.read_bytes()).hexdigest()
def checked(cmd,cwd=None):return subprocess.run(cmd,cwd=cwd,check=True)
run=json.loads((O/'workflow.json').read_text())
assert run['status']=='completed' and run['conclusion']=='success' and run['headSha']==SHA
arts=json.loads((O/'artifacts.json').read_text())['artifacts']
results=[]
for label in ('linux','linux-hidpi','macos'):
    matches=[a for a in arts if a['name']==f'studio-snapshots-{label}-{SHA}' and not a['expired']]
    assert len(matches)==1,(label,matches)
    a=matches[0]; archive=O/f"{a['id']}.zip"; archive_hash=digest(archive)
    assert a['digest']=='sha256:'+archive_hash,(label,'archive digest mismatch')
    v=O/f'verify-{label}'
    if not v.exists():
        checked(['git','clone','--quiet','--no-hardlinks',str(S),str(v)])
        checked(['git','checkout','--quiet','--detach',SHA],v)
    else:
        assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=v,text=True).strip()==SHA
        assert not subprocess.check_output(['git','status','--porcelain','--untracked-files=no'],cwd=v,text=True).strip()
    with zipfile.ZipFile(archive) as z:
        for info in z.infolist():
            p=(v/info.filename).resolve()
            assert p.is_relative_to(v.resolve()) and not stat.S_ISLNK(info.external_attr>>16),info.filename
            assert info.filename.startswith('target/') or (info.filename.startswith('studio/') and '/target/' in info.filename),info.filename
        z.extractall(v)
    receipt=O/f'{label}-matrix-receipt.json'
    cmd=['python3','tools/studio-visual-parity/check.py','--output',str(receipt)]
    if label!='linux':cmd.append('--allow-functional-only')
    checked(cmd,v)
    matrix=Path('target/studio-snapshots/VisualParitySuite')
    named=[]
    counts={'compare.ParticipantsNavigatorFxSuite':5,'harness.VisualPixelsSuite':3,'explore.SourceRecordsFxSuite':7,'shell.VisualParitySuite':30,'shell.CanvasBoundaryFxSuite':2,'shell.SmallCanvasBoundaryFxSuite':2,'shell.WorkspaceViewportFxSuite':5,'shell.BoardWorkspaceViewportFxSuite':6,'shell.A11yTreeSuite':26,'compare.CompareMapHostFxSuite':3,'harness.HarnessSmokeSuite':6,'plot.NativeOutputScaleFxSuite':1}
    for suite,count in counts.items():
        xml=v/'studio/desktop/target/test-reports'/f'TEST-eyes4s.studio.desktop.{suite}.xml'
        cases=list(ET.parse(xml).getroot().iter('testcase'))
        assert len(cases)==count and all(not any(c.find(t) is not None for t in ('failure','error')) for c in cases),(label,suite)
        skipped=[c.get('name') for c in cases if c.find('skipped') is not None]
        allowed=['a headless run draws on Monocle\'s virtual screen, not the display'] if label!='linux' and suite=='harness.HarnessSmokeSuite' else []
        if label=='macos' and suite=='harness.HarnessSmokeSuite':allowed+=['opens a 1440x900 stage, clicks, asserts and writes 1x and 2x snapshots']
        if label=='macos' and suite=='shell.A11yTreeSuite':
            allowed += [f'{name}: every focusable node has an accessible role and name' for name in ('data','explore','analysis','compare','compare-results','compare-results-dark','figures','data-empty')]
        assert all(n in allowed for n in skipped),(label,suite,skipped)
        named.append({'suite':suite,'cases':[c.get('name') for c in cases],'xmlSha256':digest(xml),'notApplicable':skipped})
    all_reports=[]
    for xml in sorted((v/'studio').glob('**/target/test-reports/*.xml')):
        cs=list(ET.parse(xml).getroot().iter('testcase'))
        assert not any(c.find('failure') is not None or c.find('error') is not None for c in cs),(label,xml)
        all_reports.append({'path':str(xml.relative_to(v)),'cases':len(cs),'passed':sum(c.find('skipped') is None for c in cs),'skipped':[c.get('name') for c in cs if c.find('skipped') is not None],'sha256':digest(xml)})
    (O/f'{label}-executed-reports.json').write_text(json.dumps(all_reports,indent=2)+'\n')
    receipts=[json.loads(p.read_text()) for p in (v/matrix).glob('*/review.json')]
    runtime={k:sorted({str(r[k]) for r in receipts}) for k in ('osName','osArch','javaVersion','javaFxVersion','glassPlatform','prismOrder','logicalWidth','logicalHeight','nativeScaleX','nativeScaleY')}
    if label=='linux':
        checked(['python3','tools/studio-e2e/check.py','--output',str(O/'e2e-receipt.json')],v)
        for axis in ('.jvm','.js'):
            xml=v/'studio/app'/axis/'target/test-reports/TEST-eyes4s.studio.app.vm.PaneFocusSuite.xml'
            cs=list(ET.parse(xml).getroot().iter('testcase'))
            assert len(cs)==3 and all(not any(c.find(t) is not None for t in ('failure','error','skipped')) for c in cs)
            named.append({'suite':f'app/{axis}/PaneFocusSuite','cases':[c.get('name') for c in cs],'xmlSha256':digest(xml)})
    if label=='linux-hidpi':
        files=list((v/'target/studio-snapshots/NativeOutputScaleFxSuite').rglob('native-output-scale.json'));assert len(files)==1
        hi=json.loads(files[0].read_text())
        assert hi['nativeScaleX']==hi['nativeScaleY']==hi['requiredScale']==2 and hi['hostOverride'] is False and hi['stripeWidthDevicePixels']==1
        assert hi['rasterDifferenceFraction']<=hi['maximumDifferenceFraction']<=.005
        (O/'gtk2-native-output-scale.json').write_text(json.dumps(hi,indent=2)+'\n')
    boundaries=[b for cls in ['CanvasBoundaryFxSuite','SmallCanvasBoundaryFxSuite'] for b in (v/'target/studio-snapshots'/cls).glob('*/canvas-boundary.json')]
    assert len(boundaries)==4
    for b in boundaries:
        phases=json.loads(b.read_text())['phases'];comparisons=[p for p in phases if 'difference' in p]
        assert len(comparisons)==16 and all(p['difference']['accepted'] for p in comparisons)
        for p in phases:
            if 'before' in p:assert all(h['status']=='Drawn' for h in p['before']+p['after'])
    visibility=[]
    for cls in ('WorkspaceViewportFxSuite','BoardWorkspaceViewportFxSuite'):
        maps=list((v/'target/studio-snapshots'/cls).glob('*/bounds.json'))
        plots=list((v/'target/studio-snapshots'/cls).glob('*/bounds-and-focus.json'))
        assert len(maps)==len(plots)==2,(label,cls)
        for p in maps:
            j=json.loads(p.read_text());assert len(j['mapDataFrameHeights'])==2 and min(j['mapDataFrameHeights'])>=120
            visibility.append({'path':str(p.relative_to(v)),'sha256':digest(p),'measurements':j})
        for p in plots:
            j=json.loads(p.read_text());assert j['participantPlotHeight']>=200
            assert len(j['forward'])==21 and j['reverse']==j['forward'][:1]+j['forward'][:0:-1]
            visibility.append({'path':str(p.relative_to(v)),'sha256':digest(p),'measurements':j})
    transcripts=[]
    for name in ('compare-results','compare-results-dark'):
        p=v/'target/studio-snapshots/A11yTreeSuite'/f'{name}.json';j=json.loads(p.read_text())
        assert len(j['walked'])==21 and j['derived']=='\n'.join(j['walked'])+'\n'
        transcripts.append({'path':str(p.relative_to(v)),'sha256':digest(p),'scene':j['scene'],'stops':len(j['walked'])})
    tab=v/'target/studio-snapshots/ParticipantsNavigatorFxSuite/tab-order.json';t=json.loads(tab.read_text())
    assert len(t['stops'])==21
    assert len([x for x in t['stops'] if x['role']=='LIST_VIEW' and x['name'].startswith('Participants,')])==1
    results.append({'court':label,'artifact':a['id'],'artifactName':a['name'],'archiveSha256':archive_hash,'matrixReceiptSha256':digest(receipt),'runtime':runtime,'namedCases':named,'executedReportsSha256':digest(O/f'{label}-executed-reports.json'),'workspaceVisibility':visibility,'fullResultsTranscripts':transcripts,'tabTranscriptSha256':digest(tab),'tabStops':len(t['stops']),'tabScene':{k:t[k] for k in ('logicalWidth','logicalHeight')}})
record={'schema':1,'candidate':SHA,'tree':TREE,'studioRun':40357518894,'workflowAttempt':run['attempt'],'courts':results,'limits':['Mac retains eight board-size-only role/name audit skips under the existing EYES4S_STUDIO_SMALL_DISPLAY policy; its full Tab traversals, disabled-reason audits and all small-window cases must still execute. Linux and GTK execute those board-size role/name cases.','Linux Monocle native1 owns pixel goldens;2x snapshots are raster transforms.','Mac and GTK2 matrix checks are functional/repeat; GTK2 has a separate actual native2 analytic raster proof.','No G2, physical Mac Retina, VoiceOver, full release performance, packaging or researcher usability acceptance.']}
m=re.search(r'SourceRecordsFxSuite: median ([0-9.]+) ms, p99 ([0-9.]+) ms, worst ([0-9.]+) ms', (O/'gtk-job.log').read_text())
assert m and float(m[1]) <=32
record['gtkRecordScrolling']={'medianMs':float(m[1]),'p99Ms':float(m[2]),'worstMs':float(m[3]),'medianLimitMs':32,'jobLogSha256':digest(O/'gtk-job.log'),'scope':'existing SourceRecords scrolling regression; not full S10.6 performance acceptance'}
(O/'hosted-review.json').write_text(json.dumps(record,indent=2)+'\n')
print('VERIFIED all three source-bound hosted courts, full Results focus and supported-window geometry')
