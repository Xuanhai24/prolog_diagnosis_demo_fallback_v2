# Flask API with dual backend:
# 1) Try PySWIP (native)
# 2) Fallback to SWI-Prolog subprocess via bridge.pl (no pyswip needed)
from flask import Flask, request, jsonify, render_template
import json, subprocess, shlex, os, sys

HAVE_PYSWIP = False
try:
    from pyswip import Prolog
    HAVE_PYSWIP = True
except Exception:
    HAVE_PYSWIP = False

app = Flask(__name__)

def ensure_swipl_available():
    from shutil import which
    return which("swipl") is not None

# ---------- Bridge mode (no PySWIP) ----------
def prolog_cli(goal: str) -> dict:
    """
    Run SWI-Prolog with bridge.pl and a goal, return parsed JSON (dict).
    """
    cmd = ["swipl", "-q", "-s", "bridge.pl", "-g", goal, "-t", "halt"]
    try:
        res = subprocess.run(cmd, capture_output=True, text=True, check=True)
        out = res.stdout.strip()
        return json.loads(out) if out else {}
    except subprocess.CalledProcessError as e:
        return {"error": e.stderr or str(e)}
    except json.JSONDecodeError:
        return {"error": "JSON parse failed from Prolog output."}

def prolog_list_literal(sym_list):
    # ["sot","ho"] -> [sot,ho]
    safe = []
    for s in sym_list:
        s = (s or "").strip().replace("-", "_")
        # basic guard: keep alnum + underscore only
        s = "".join(ch for ch in s if (ch.isalnum() or ch == "_"))
        if s:
            safe.append(s)
    return "[" + ",".join(safe) + "]"

def prolog_quote_text(s: str) -> str:
    # escape for single-quoted atom in Prolog
    return "'" + s.replace("\\", "\\\\").replace("'", "\\'") + "'"

# ---------- PySWIP mode ----------
if HAVE_PYSWIP:
    prolog = Prolog()
    prolog.consult("engine.pl")

    def pyswip_query_one(qstr, var):
        res = list(prolog.query(qstr))
        return res[0].get(var) if res else None

@app.route("/")
def index():
    return render_template("index.html")

@app.route("/api/parse", methods=["POST"])
def api_parse():
    data = request.get_json(force=True)
    text = data.get("text","")
    if HAVE_PYSWIP:
        safe = text.replace("'", "\\'")
        syms = pyswip_query_one(f"normalize_text('{safe}', Syms).", "Syms") or []
        syms = [str(s) for s in syms]
        return jsonify({"symptoms": syms})
    else:
        if not ensure_swipl_available():
            return jsonify({"error": "SWI-Prolog (swipl) not found in PATH."}), 500
        goal = f"cli_normalize({prolog_quote_text(text)})"
        out = prolog_cli(goal)
        return jsonify(out)

@app.route("/api/questions", methods=["POST"])
def api_questions():
    data = request.get_json(force=True)
    syms = data.get("symptoms", [])
    maxq = int(data.get("max", 5))
    if HAVE_PYSWIP:
        syms_list = prolog_list_literal(syms)
        q = pyswip_query_one(f"questions_suggest({syms_list}, {maxq}, Qs).", "Qs") or []
        enriched = []
        for s in q:
            qt = pyswip_query_one(f"question_text({s}, Txt).", "Txt")
            if qt:
                enriched.append({"symptom": str(s), "question": str(qt)})
            else:
                enriched.append({"symptom": str(s), "question": f"Bạn có {s} không?"})
        return jsonify({"questions": enriched})
    else:
        if not ensure_swipl_available():
            return jsonify({"error": "SWI-Prolog (swipl) not found in PATH."}), 500
        syms_list = prolog_list_literal(syms)
        goal = f"cli_questions({syms_list}, {maxq})"
        out = prolog_cli(goal)
        return jsonify(out)

@app.route("/api/diagnose", methods=["POST"])
def api_diagnose():
    data = request.get_json(force=True)
    syms = data.get("symptoms", [])
    if HAVE_PYSWIP:
        syms_list = prolog_list_literal(syms)
        pairs = pyswip_query_one(f"diagnosis({syms_list}, Pairs).", "Pairs") or []
        results = []
        for p in pairs:
            try:
                cf = float(p.args[0])
                disease = str(p.args[1])
            except Exception:
                s = str(p)
                import re
                m = re.match(r"pair\(([^,]+),(.+)\)", s)
                if m:
                    cf = float(m.group(1)); disease = m.group(2)
                else:
                    cf, disease = 0.0, s
            dept = pyswip_query_one(f"department_of({disease}, Dept).", "Dept"); dept = str(dept) if dept else ""
            otc = pyswip_query_one(f"otc_of({disease}, Adv).", "Adv"); otc = str(otc) if otc else ""
            results.append({"disease": disease, "cf": cf, "department": dept, "otc": otc})
        triage = pyswip_query_one(f"triage_level_given({syms_list}, Level).", "Level") or "nhe"
        flags = pyswip_query_one(f"red_flags_in({syms_list}, Flags).", "Flags") or []
        flags = [str(x) for x in flags]
        return jsonify({"results": results, "triage": str(triage), "red_flags": flags})
    else:
        if not ensure_swipl_available():
            return jsonify({"error": "SWI-Prolog (swipl) not found in PATH."}), 500
        syms_list = prolog_list_literal(syms)
        goal = f"cli_diagnose({syms_list})"
        out = prolog_cli(goal)
        return jsonify(out)

if __name__ == "__main__":
    # For Windows friendly reload off by default
    app.run(debug=True)
