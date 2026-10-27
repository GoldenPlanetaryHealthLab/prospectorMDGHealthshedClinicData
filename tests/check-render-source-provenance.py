"""Verify canonical generated Source headers without rendering or reading data."""
from pathlib import Path
import re
import subprocess
import sys
import tempfile

page = Path('notebooks/01_misc-render-isolated.qmd').read_text()
code = re.search(r"python3 - .* <<'PY'\n(.*?)\nPY", page, re.S).group(1)
with tempfile.TemporaryDirectory() as temporary:
    emitted = Path(temporary, 'generated')
    staging = str(Path(temporary, 'staged project'))
    repository = str(Path(temporary, 'repository'))
    for prefix, suffix in [('# Source: ', ''), ('-- Source: ', ''),
                           ('<!-- Source: ', ' -->'), ('/* Source: ', ' */')]:
        body = f'body retains {staging}/data/path\n'
        emitted.write_text(f'{prefix}{staging}/notebooks/source.qmd{suffix}\n' + body)
        subprocess.run([sys.executable, '-', str(emitted), staging, repository], input=code, text=True, check=True)
        expected = f'{prefix}{repository}/notebooks/source.qmd{suffix}\n' + body
        assert emitted.read_text() == expected
        before = emitted.read_bytes()
        subprocess.run([sys.executable, '-', str(emitted), staging, repository], input=code, text=True, check=True)
        assert emitted.read_bytes() == before
print('Generated source headers are canonical and idempotent; body paths are unchanged.')
