"""Check semantic block placement without rendering or opening scientific data.

Run from the repository root: python3 tests/check-manual-structure.py
"""

from pathlib import Path
import re


def inspect_page(path):
    divs = []
    counts = {kind: 0 for kind in ("prereq", "procedure", "check")}
    fence = None
    errors = []
    for line_number, line in enumerate(path.read_text().splitlines(), 1):
        if fence:
            marker, width, language, context = fence
            if re.fullmatch(rf"\s*{re.escape(marker)}{{{width},}}\s*", line):
                fence = None
            elif language == "r" and re.match(r"s\d\w*\s*<-", line):
                if "procedure" not in context or "check" in context:
                    errors.append(f"{path}:{line_number}: target declaration outside a procedure")
            continue

        code = re.match(r"\s*(`{3,}|~{3,})(.*)", line)
        if code:
            marker, info = code.groups()
            language = "r" if re.match(r"\{r(?:[\s,}]|$)", info.strip()) else info.strip()
            fence = (marker[0], len(marker), language, tuple(kind for _, kind in divs))
            continue

        div = re.match(r"\s*(:{3,})\s*(.*)", line)
        if not div:
            continue
        marker, attributes = div.groups()
        if not attributes:
            if not divs or divs[-1][0] != len(marker):
                errors.append(f"{path}:{line_number}: unmatched div fence")
            else:
                divs.pop()
            continue
        kind = re.search(r"\.manual-(prereq|procedure|check)\b", attributes)
        kind = kind.group(1) if kind else None
        divs.append((len(marker), kind))
        if kind:
            counts[kind] += 1

    if fence or divs:
        errors.append(f"{path}: unclosed code or div fence")
    if re.match(r"\d\d_page-", path.name):
        for kind in ("prereq", "check"):
            if counts[kind] != 1:
                errors.append(f"{path}: expected one {kind} block, found {counts[kind]}")
        if not counts["procedure"]:
            errors.append(f"{path}: no procedure block")
    return errors


pages = [
    path for path in Path("notebooks").rglob("*.qmd")
    if not any(part in ("_extensions", ".ipynb_checkpoints") for part in path.parts)
    and path.read_text().strip()
]
errors = [error for page in pages for error in inspect_page(page)]
if errors:
    raise SystemExit("\n".join(errors))
print(f"Manual structure verified across {len(pages)} pages: target declarations belong to procedures.")
