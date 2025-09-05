/* ---------------------------------------------------------
   bridge.pl — JSON CLI bridge for SWI‑Prolog
   Uses engine.pl and exposes 3 CLI goals that print JSON to stdout:
     - cli_normalize(Text)
     - cli_questions(SymList, Max)
     - cli_diagnose(SymList)
   Example (shell):
     swipl -q -s bridge.pl -g "cli_diagnose([sot,ho])" -t halt
   --------------------------------------------------------- */
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- set_prolog_flag(encoding, utf8).

:- [engine].

atoms_to_strings([], []).
atoms_to_strings([A|T], [S|R]) :- atom_string(A, S), atoms_to_strings(T, R).

% --- /api/parse equivalent
cli_normalize(Text) :-
  normalize_text(Text, Syms),
  atoms_to_strings(Syms, SStr),
  json_write_dict(current_output, _{ symptoms: SStr }, [width(0)]),
  nl.

% --- /api/questions equivalent
cli_questions(SymList, Max) :-
  questions_suggest(SymList, Max, Qs),
  % build list of dicts with question text
  findall(_{symptom:Ss, question:QT},
    ( member(S, Qs),
      atom_string(S, Ss),
      ( question_text(S, QTT) -> QT = QTT ; format(string(QT), "Bạn có ~w không?", [S]) )
    ),
    Dicts),
  json_write_dict(current_output, _{ questions: Dicts }, [width(0)]),
  nl.

% --- /api/diagnose equivalent
cli_diagnose(SymList) :-
  diagnosis(SymList, Pairs),
  findall(_{disease:Ds, cf:CF, department:DeptS, otc:OTC},
    ( member(pair(CF,D), Pairs),
      atom_string(D, Ds),
      ( department_of(D, Dept) -> atom_string(Dept, DeptS) ; DeptS = "" ),
      ( otc_of(D, Adv) -> OTC = Adv ; OTC = "" )
    ),
    Results),
  triage_level_given(SymList, Level),
  red_flags_in(SymList, Flags),
  atoms_to_strings(Flags, FlagStr),
  atom_string(Level, LevelS),
  json_write_dict(current_output, _{ results: Results, triage: LevelS, red_flags: FlagStr }, [width(0)]),
  nl.
