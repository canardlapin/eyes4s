#!/usr/bin/env python3
"""Compact sensitivity evidence and a standalone scientific figure from verified output."""
import argparse
import hashlib
import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--analysis", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--figure", type=Path, required=True)
    args = parser.parse_args()
    data = json.loads(args.analysis.read_text())
    records = data["scores"]
    contexts = []
    sigmas = sorted({r["sigma"] for r in records})
    sizes = sorted({r["nx"] for r in records})
    for sigma in sigmas:
        for nx in sizes:
            rows = [r for r in records if r["sigma"] == sigma and r["nx"] == nx]
            maps = [r for r in data["map_checks"] if r["sigma"] == sigma and r["nx"] == nx]
            contexts.append(dict(nx=nx,ny=nx*3//4,sigma=sigma,sigma_in_cells=sigma/(8/nx),
                                 max_reference_score_difference=max(abs(r["signed_native_to_reference"]) for r in rows),
                                 max_continuous_score_difference=max(abs(r["scores"]["native"]["difference"]-r["scores"]["continuous-full-gaussian"]["difference"]) for r in rows),
                                 ordered_max_changes={k:max(abs(r["ordered_changes"][k]) for r in rows) for k in ("binning","support","placement","rounding")},
                                 retained_weight_fraction_range=[min(r["retained_weight_fraction"] for r in maps),max(r["retained_weight_fraction"] for r in maps)],
                                 strict_sign_disagreements=sum(r["strict_sign_disagreement"] for r in rows)))
    result = {k:data[k] for k in ("input_sha256","native_log_sha256","input_identity","jobs","exact_jvm_js_agreement","map_oracle_tolerance","score_oracle_tolerance","attribution")}
    result.update(schema=1,analysis_sha256=hashlib.sha256(args.analysis.read_bytes()).hexdigest(),
                  map_checks=len(data["map_checks"]),query_checks=len(records),
                  map_max_absolute_error=max(r["max_absolute_error"] for r in data["map_checks"]),
                  contexts=contexts,
                  sign_cases=[{k:r[k] for k in ("nx","ny","sigma","query","matched_template","control_pool","scores")} for r in records if r["strict_sign_disagreement"]])
    if "actual_r_reference" in data:
        result["actual_r_reference"] = data["actual_r_reference"]
    args.output.parent.mkdir(parents=True,exist_ok=True)
    args.output.write_text(json.dumps(result,indent=2)+"\n")
    fig, axes = plt.subplots(1,2,figsize=(10,3.8),layout="constrained")
    colors = plt.cm.viridis([.05,.35,.65,.9])
    for sigma,color in zip(sigmas,colors):
        rows = [r for r in contexts if r["sigma"] == sigma]
        for ax,field in zip(axes,("max_reference_score_difference","max_continuous_score_difference")):
            ax.plot(sizes,[r[field] for r in rows],marker="o",color=color,label=f"σ = {sigma:g}")
    for ax,title in zip(axes,("Native vs pinned R conventions","Native vs continuous full Gaussian")):
        ax.set(xscale="log",yscale="log",xlabel="Grid columns (rows = ¾ columns)",ylabel="Maximum |Δ corrected cosine score|",title=title)
        ax.set_xticks(sizes,labels=[str(n) for n in sizes]);ax.grid(alpha=.2)
    axes[0].legend(frameon=False)
    args.figure.parent.mkdir(parents=True,exist_ok=True)
    fig.savefig(args.figure,dpi=180)


if __name__ == "__main__":
    main()
