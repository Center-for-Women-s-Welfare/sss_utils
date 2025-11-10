# sssUtils

Utility functions for common tasks in the Self-Sufficiency Standard (SSS) project workflow.

This package provides path and configuration helpers used across `sss_production` and related repositories, including environment-based path resolution via `SSS_DATA_BASE` (Google Drive data root) and `SSS_CODE_BASE` (local code root).

---

### ⚙️ Environment setup

To use the path utilities, define these environment variables in your `~/.Renviron` file:

SSS_DATA_BASE="G:/Shared drives/CWW Team Drive/SSS/sss_production"
SSS_CODE_BASE="C:/Users/YourName/Desktop/local_dev"

---

### 🧭 Example usage

# Build paths under the shared Drive (data)
build_sss_path("data", 2026, "processed", module = "taxes_state")

# Build paths under your local repo (code)
build_sss_path("src", 2026, "processing", where = "code", module = "child_care")

# Load configuration files
load_sss_config("IA")  # Auto-detects current repo or falls back to sss_production
