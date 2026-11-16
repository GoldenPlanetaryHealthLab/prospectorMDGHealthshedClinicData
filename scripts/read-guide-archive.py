"""Read only selected frozen Feather columns for the data guide.

Run from the repository root with the pinned reader documented in the guide:
uv run --no-project --with pyarrow==22.0.0 python scripts/read-guide-archive.py
The diagnostic projection stays in ignored data/intermediates; it is not input
to the scientific pipeline. No Drive outputs or wrong_fname files are read.
"""
import hashlib
import json
from pathlib import Path
import shutil
import tempfile
import zipfile

import pyarrow
import pyarrow.feather as feather
import pyarrow.parquet as parquet


def sha256(path):
    with open(path, "rb") as source:
        return hashlib.file_digest(source, "sha256").hexdigest()


def main():
    root = Path("data/intermediates/data-guide")
    request_path = root / "request.json"
    request = json.loads(request_path.read_text())
    archive = Path(request["baseline"]["archive_zip"])
    if sha256(archive) != request["baseline"]["archive_sha256"]:
        raise ValueError("Archive differs from the reviewed May 2025 freeze")
    member = request["baseline"]["diagnosis_directory_member"] + "file2.feather"
    with tempfile.TemporaryDirectory(prefix="feather-", dir=root) as temporary:
        extracted = Path(temporary) / "file2.feather"
        with zipfile.ZipFile(archive) as bundle, bundle.open(member) as source:
            with extracted.open("wb") as target:
                shutil.copyfileobj(source, target)
        table = feather.read_table(extracted, columns=["clinic_ID", "cAnnee", "cPeriode", "fname"] + request["variables"])
        temporary_output = Path(temporary) / "archive-selected.parquet"
        parquet.write_table(table, temporary_output)
        output = root / "archive-selected.parquet"
        temporary_output.replace(output)
        receipt = {"archive_sha256": sha256(archive), "member": member,
                   "member_sha256": sha256(extracted), "projection_sha256": sha256(output),
                   "request_sha256": sha256(request_path), "pyarrow": pyarrow.__version__,
                   "rows": table.num_rows, "columns": table.column_names}
    (root / "archive-receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
    print("Frozen archive projection ready; no research rows printed.")


if __name__ == "__main__":
    main()
