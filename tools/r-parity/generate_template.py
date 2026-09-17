#!/usr/bin/env python3
"""Check the actual Scala training export, R adapter, rational oracle and pinned eyesim map fit."""
import argparse
import csv
from fractions import Fraction
import hashlib
import io
import json
import os
from pathlib import Path
import subprocess
import tarfile
import tempfile

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
INPUT = HERE / "fixtures/baseline-cases.json"
REQUEST = HERE / "fixtures/template-training.csv"
OUTPUT = HERE / "fixtures/template.json"
SCALA = ROOT / "io/src/test/scala/eyes4s/io/TemplateFitReference.scala"
REVISION = "ecb9c496257bce51acd5330af6a5e7a8d5b84e05"
ABSOLUTE_TOLERANCE = 1e-12


def rational_fit(rows):
    # Exact two-column normal equations are an oracle, not the floating QR backend.
    a = sum(Fraction(r["features"][0])**2 for r in rows)
    b = sum(Fraction(r["features"][0])*Fraction(r["features"][1]) for r in rows)
    c = sum(Fraction(r["features"][1])**2 for r in rows)
    d = sum(Fraction(r["features"][0])*Fraction(r["response"]) for r in rows)
    e = sum(Fraction(r["features"][1])*Fraction(r["response"]) for r in rows)
    determinant = a*c-b*b
    return [(c*d-b*e)/determinant, (a*e-b*d)/determinant]


def csv_text(header, rows):
    stream = io.StringIO(newline="")
    writer = csv.writer(stream, lineterminator="\r\n")
    writer.writerow(header)
    writer.writerows(rows)
    return stream.getvalue()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--eyesim", type=Path, required=True)
    parser.add_argument("--training-csv", type=Path, help="Initial actual Scala CLI export; omitted for regeneration")
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    data = INPUT.read_bytes()
    case = json.loads(data)["template_model"]
    request = (args.training_csv or REQUEST).read_bytes()
    table = list(csv.reader(io.StringIO(request.decode())))
    header, body = table[0], table[1:]
    assert header[7:] == ["feature:template-a", "feature:template-b"]
    assert len(body) == len(case["training"]) == 4
    for i, (row, expected) in enumerate(zip(body, case["training"])):
        assert row[4] == str(i) and row[5] == expected["fold"]
        assert list(map(float, row[7:])) == expected["features"]
        assert float(row[6]) == expected["response"]
    exact = rational_fit(case["training"])
    assert exact == [Fraction(1), Fraction(2)]
    held = case["held_out"]
    prediction = sum(a*Fraction(b) for a,b in zip(exact, held["features"]))
    contaminated_rows = case["training"] + [dict(held, response=700)]
    contaminated_exact = rational_fit(contaminated_rows)
    env = os.environ | {"LC_ALL": "C", "LANG": "C", "RGL_USE_NULL": "TRUE"}
    with tempfile.TemporaryDirectory(prefix="eyes4s-template-reference-") as tmp:
        tmp = Path(tmp)
        def fit(name, contents, success=True):
            input_path, output_path = tmp / (name + ".csv"), tmp / (name + "-fit.csv")
            input_path.write_bytes(contents)
            run = subprocess.run(["Rscript", "--vanilla", str(ROOT / "tools/template-fit/fit.R"),
                                  str(input_path), str(output_path)], env=env, capture_output=True, text=True)
            if not success:
                assert run.returncode != 0 and not output_path.exists(), (name, run.stdout, run.stderr)
                return
            assert run.returncode == 0, run.stderr
            output = output_path.read_bytes().decode()
            rows = list(csv.DictReader(io.StringIO(output)))
            return output, rows
        receipt, fitted = fit("training", request)
        for row, beta in zip(fitted, exact):
            assert abs(float(row["coefficient"]) - float(beta)) <= ABSOLUTE_TOLERANCE
        assert len(fitted) == 2
        assert all(r["rank"] == "2" and r["observations"] == "4" and r["training_hash"] == body[0][1] for r in fitted)
        _, singleton = fit("one-feature-one-row", csv_text(header[:8], [body[0][:8]]).encode())
        assert len(singleton) == 1 and float(singleton[0]["coefficient"]) == 1
        # This offset case distinguishes no-intercept fitting from an accidental intercept.
        shifted = [row[:6] + [str(float(row[6])+3)] + row[7:] for row in body]
        _, shifted_fit = fit("shifted-response", csv_text(header, shifted).encode())
        shifted_exact = rational_fit([dict(r, response=r["response"]+3) for r in case["training"]])
        assert shifted_exact == [Fraction(2), Fraction(4)]
        for row, beta in zip(shifted_fit, shifted_exact):
            assert abs(float(row["coefficient"])-float(beta)) <= ABSOLUTE_TOLERANCE
        leaked = body + [body[0][:4] + ["4", "test", "700", "3", "2"]]
        _, bad_fit = fit("contaminated", csv_text(header, leaked).encode())
        assert any(abs(float(r["coefficient"]) - float(b)) > 1 for r,b in zip(bad_fit, exact))
        for r,b in zip(bad_fit, contaminated_exact):
            assert abs(float(r["coefficient"]) - float(b)) <= ABSOLUTE_TOLERANCE
        deficient = [row[:8] + [str(2*float(row[7]))] for row in body]
        fit("rank-deficient", csv_text(header, deficient).encode(), success=False)
        fit("underdetermined", csv_text(header, body[:1]).encode(), success=False)
        nonfinite = [list(row) for row in body]
        nonfinite[0][6] = "NaN"
        fit("nonfinite", csv_text(header, nonfinite).encode(), success=False)
        archive, src, lib = tmp/"source.tar", tmp/"source", tmp/"library"
        src.mkdir(); lib.mkdir()
        subprocess.run(["git", "-C", str(args.eyesim), "archive", "--format=tar", "--output", str(archive), REVISION], check=True)
        with tarfile.open(archive) as tar:
            tar.extractall(src, filter="data")
        install = subprocess.run(["R", "CMD", "INSTALL", f"--library={lib}", str(src)], env=env, capture_output=True, text=True)
        assert install.returncode == 0, install.stdout + install.stderr
        ref = tmp/"eyesim.json"
        subprocess.run(["Rscript", "--vanilla", str(HERE/"template.R"), str(lib), str(INPUT), str(ref)], env=env, check=True)
        reference = json.loads(ref.read_text())
        assert reference["terms"] == ["template_a", "template_b"]
        # eyesim fits independently normalized maps: x columns sum to 4/3, y to 10.
        for got, expected in zip(reference["coefficients"], (Fraction(2,5), Fraction(3,5))):
            assert abs(got-float(expected)) <= ABSOLUTE_TOLERANCE
    document = dict(eyesim_revision=REVISION, input_sha256=hashlib.sha256(data).hexdigest(),
        training_sha256=hashlib.sha256(request).hexdigest(), training_hash=body[0][1],
        tolerance=dict(absolute=ABSOLUTE_TOLERANCE,relative=0),
        exact_coefficients=list(map(str,exact)), held_out_prediction=str(prediction),
        held_out_mutated_response="700", held_out_mutated_residual=str(Fraction(700)-prediction),
        contaminated_coefficients=list(map(str,contaminated_exact)),
        coefficient_receipt=receipt, eyesim_normalized_map_fit=reference,
        rejected_adapter_cases=["rank-deficient","underdetermined","nonfinite"])
    license_header = (ROOT/"design/src/main/scala/eyes4s/design/TemplateFit.scala").read_text().split("package ")[0]
    scala = license_header + "package eyes4s.io\n\n// Generated by tools/r-parity/generate_template.py. Do not hand-edit.\n// format: off\nobject TemplateFitReference:\n"
    scala += "  val trainingCsv = " + json.dumps(request.decode()) + "\n"
    scala += "  val receipt = " + json.dumps(receipt) + "\n// format: on\n"
    files = {REQUEST:request, OUTPUT:(json.dumps(document,indent=2)+"\n").encode(), SCALA:scala.encode()}
    for path, content in files.items():
        if args.check:
            assert path.read_bytes() == content, f"Template fixture drift: {path}"
        else:
            path.write_bytes(content)
    print("Template adapter and pinned map reference pass; exact held-out target and contaminated-fit mutant checked; three invalid designs refused.")


if __name__ == "__main__":
    main()
