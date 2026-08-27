# Project Template: Climate + Public Health Data Science (FASRC HPC)

This template is designed for climate and public health analysis at Harvard FASRC, with:
- **R or Python** for analysis code
- **Quarto** for reproducible notebooks/reports
- **Spack** for reproducible software environments on HPC

## Suggested project layout

```text
ProjectTemplate/
├── README.md
├── data/
│   ├── raw/          # immutable source data
│   ├── interim/      # temporary processing outputs
│   └── processed/    # analysis-ready datasets
├── sandbox/          # scratch work, one-off experiments, not production code
├── notebooks/        # .qmd analyses and reports
├── src/              # reusable analysis code (R or Python)
└── env/              # environment definitions (e.g., spack.yaml)
```

## Environment setup (Spack)

Use Spack to pin software versions and keep runs reproducible across cluster nodes.

Use `curl` to predict which packages you will need by pinging the Posit Package Manager Repository:

```sh
curl -X POST "https://packagemanager.posit.co/__api__/repos/4/sysreqs" \
  -H "Content-Type: application/json" \
  -d '{
    "requirements":["targets","readxl","dplyr","fusen","quarto"],
    "r_version":"4.5.1",
    "bioc_version":"3.21"
  }'
```

```bash
# Example workflow (adjust to your local FASRC setup)
spack env create project-env ./env/spack.yaml
spack env activate project-env
spack install
```

## Installing analysis packages

### Python

```bash
python -m venv .venv
source .venv/bin/activate
pip install -U pip
pip install -r requirements.txt
```

### R

```r
install.packages(c("tidyverse", "arrow", "data.table", "sf", "targets"))
# Optional for project-local package management:
# install.packages("renv")
# renv::init()
```

## Quarto usage

Keep computational narratives in `.qmd` files under `notebooks/`.

```bash
quarto render notebooks/
```

## Best practices: move analysis into a package

As analyses stabilize, move reusable code from notebooks/scripts into a package.

### Python package path
- Put reusable functions/classes in `src/<package_name>/`
- Add `pyproject.toml` and package metadata early
- Keep notebooks thin: call package functions instead of embedding logic
- Add tests for package functions and run them in CI/HPC jobs

### R package path
- Organize reusable functions in `R/`
- Use `DESCRIPTION` + `NAMESPACE` and document with `roxygen2`
- Use `usethis`/`devtools` to scaffold and maintain package structure
- Keep analyses as consumers of package functions, not as the source of truth

## Directory conventions

- **`sandbox/`**: scratch space for exploratory code and temporary files. Treat as disposable.
- **`data/`**: canonical project data location. Keep raw data read-only; write derived data to `interim/` or `processed/`.

## Reproducibility notes

- Pin environment dependencies (Spack + language-specific lockfiles).
- Avoid editing raw input files.
- Promote shared logic into versioned package code.
- Render Quarto reports from scripted, repeatable pipelines.
