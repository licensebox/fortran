#!/usr/bin/env python3
"""JSON framing only. All SDK operations execute in the language adapter."""
import json, subprocess, sys
r=json.load(sys.stdin)
options={k:r[k] for k in ('product','keys','server','directory','ca_bundle') if k in r}
options['version']='conformance'
fields=[r['op'],r['product'],r.get('source',''),r.get('raw_id',''),r.get('token',''),json.dumps(r.get('keys',{})),str(r.get('now',0)),r.get('binding',''),json.dumps(options),json.dumps({'module':r.get('module','core')}),r.get('module','core')]
data='\n'.join(x.encode().hex() for x in fields)+'\n'
p=subprocess.run(sys.argv[1:],input=data,text=True,capture_output=True)
if p.returncode: sys.stderr.write(p.stderr);sys.exit(p.returncode)
lines=p.stdout.splitlines()
if len(lines)!=2: raise RuntimeError('invalid language adapter response: '+p.stdout)
error=bytes.fromhex(lines[0]).decode();value=bytes.fromhex(lines[1]).decode()
print(json.dumps({'ok':False,'error':error} if error else {'ok':True,'value':json.loads(value)}))
