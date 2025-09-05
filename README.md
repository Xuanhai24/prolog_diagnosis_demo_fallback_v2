# Prolog Diagnosis Demo (Full-stack)
A minimal full-stack demo for a symptom-based diagnosis expert system:
- **Backend core (Prolog)**: `engine.pl` (stateless predicates).
- **API (Python Flask)**: `app.py` using **PySWIP** to call Prolog.
- **Frontend**: `templates/index.html` + `static/style.css`.

## Quickstart
1. Install SWI-Prolog on your machine.
2. Create a Python venv and install requirements:
   ```bash
   pip install -r requirements.txt
   ```
3. Run Flask:
   ```bash
   python app.py
   ```
4. Open the browser at `http://127.0.0.1:5000/`.

## How it works
- `/api/parse`: normalize free text into canonical symptom atoms.
- `/api/questions`: suggest top missing discriminative questions.
- `/api/diagnose`: compute CF for all diseases, return triage level, red flags, department, OTC advice.

> Disclaimer: This demo is for educational purposes only, not medical advice.

## If `pyswip` installation fails on Windows or Python 3.12/3.13
This project now includes a **fallback** that does **not require pyswip**.

### Option A — PySWIP (preferred)
1) Use Python **3.11** (PySWIP wheels often lag behind latest Python).
2) Ensure SWI‑Prolog is installed and in PATH.
3) Install:
```bash
pip install Flask
pip install "pyswip @ git+https://github.com/yuce/pyswip.git"
```
or try a specific version that matches your Python.

### Option B — Fallback (no pyswip required)
1) Ensure SWI‑Prolog (`swipl`) is installed and added to PATH.
2) Install only Flask:
```bash
pip install Flask
```
3) Run the app:
```bash
python app.py
```
The API will call SWI‑Prolog via `bridge.pl` under the hood.
