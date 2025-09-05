/* =========================================================
   Prolog Expert System Engine (Stateless) for Symptom-based Diagnosis
   - SWI-Prolog
   - Designed to be called from Flask/Python via PySWIP
   - All predicates are pure/stateless and accept explicit symptom lists
   ========================================================= */

:- module(engine, [
    normalize_text/2,           % +Text, -Symptoms(list)
    questions_suggest/3,        % +PresentSymptoms, +Max, -Questions(list of atoms)
    diagnosis/2,                % +PresentSymptoms, -SortedPairs(list of pair(CF,B))
    triage_level_given/2,       % +PresentSymptoms, -Level
    red_flags_in/2,             % +PresentSymptoms, -Flags(list)
    department_of/2,            % +Disease, -Department
    otc_of/2,                   % +Disease, -Advice
    disease_name/1,             % -Disease
    question_text/2             % +Symptom, -QuestionText
]).

:- use_module(library(lists)).
:- set_prolog_flag(encoding, utf8).

% -----------------------------
% Aliases (synonyms) for normalization
% -----------------------------
alias("sốt", sot).     alias("sot", sot).     alias("fever", sot).
alias("ho", ho).       alias("cough", ho).
alias("nhức", nhuc_moi). alias("nhức mỏi", nhuc_moi). alias("nhuc", nhuc_moi). alias("nhuc moi", nhuc_moi).
alias("mệt", met_moi). alias("mệt mỏi", met_moi). alias("met", met_moi). alias("met moi", met_moi).
alias("đau họng", dau_hong). alias("dau hong", dau_hong). alias("sore throat", dau_hong).
alias("đau bụng", dau_bung). alias("dau bung", dau_bung). alias("stomachache", dau_bung).
alias("buồn nôn", buon_non). alias("buon non", buon_non). alias("nausea", buon_non).
alias("tiêu chảy", tieu_chay). alias("tieu chay", tieu_chay). alias("diarrhea", tieu_chay).
alias("ngạt mũi", nghet_mui). alias("nghet mui", nghet_mui). alias("nasal congestion", nghet_mui).
alias("chảy mũi", chay_mui). alias("chay mui", chay_mui). alias("runny nose", chay_mui).
alias("đau đầu", dau_dau). alias("dau dau", dau_dau). alias("headache", dau_dau).
alias("đau xoang", dau_xoang). alias("dau xoang", dau_xoang). alias("sinus pain", dau_xoang).
alias("đờm", dom). alias("dom", dom). alias("đàm", dom). alias("dam", dom). alias("phlegm", dom).
alias("khó thở", kho_tho). alias("kho tho", kho_tho). alias("shortness of breath", kho_tho).
alias("đau ngực", dau_nguc). alias("dau nguc", dau_nguc). alias("chest pain", dau_nguc).
alias("mất vị giác", mat_vi_giac). alias("mat vi giac", mat_vi_giac). alias("ageusia", mat_vi_giac).
alias("mất khứu giác", mat_khuu_giac). alias("mat khuu giac", mat_khuu_giac). alias("anosmia", mat_khuu_giac).
alias("đau sau hốc mắt", dau_sau_hoc_mat). alias("dau sau hoc mat", dau_sau_hoc_mat).
alias("phát ban", phat_ban). alias("phat ban", phat_ban). alias("rash", phat_ban).
alias("nôn ra máu", non_ra_mau). alias("non ra mau", non_ra_mau).
alias("đi ngoài ra máu", di_ngoai_ra_mau). alias("di ngoai ra mau", di_ngoai_ra_mau).
alias("choáng váng", choang_vang). alias("choang vang", choang_vang).
alias("mờ mắt", mo_mat). alias("mo mat", mo_mat).
alias("khát nhiều", khat_nhieu). alias("khat nhieu", khat_nhieu).
alias("tiểu nhiều", tieu_nhieu). alias("tieu nhieu", tieu_nhieu).
alias("sụt cân", sut_can). alias("sut can", sut_can).

% Question text for each symptom
question_text(sot,               "Bạn có bị sốt không?").
question_text(ho,                "Bạn có bị ho không?").
question_text(nhuc_moi,          "Bạn có nhức mỏi cơ thể không?").
question_text(met_moi,           "Bạn có cảm thấy mệt mỏi không?").
question_text(dau_hong,          "Bạn có đau họng không?").
question_text(dau_bung,          "Bạn có đau bụng không?").
question_text(buon_non,          "Bạn có buồn nôn không?").
question_text(tieu_chay,         "Bạn có tiêu chảy không?").
question_text(nghet_mui,         "Bạn có ngạt mũi không?").
question_text(chay_mui,          "Bạn có chảy mũi không?").
question_text(dau_dau,           "Bạn có đau đầu không?").
question_text(dau_xoang,         "Bạn có đau vùng xoang không?").
question_text(dom,               "Bạn có khạc ra đờm/đàm không?").
question_text(kho_tho,           "Bạn có khó thở không?").
question_text(dau_nguc,          "Bạn có đau ngực không?").
question_text(mat_vi_giac,       "Bạn có mất vị giác không?").
question_text(mat_khuu_giac,     "Bạn có mất khứu giác không?").
question_text(dau_sau_hoc_mat,   "Bạn có đau sau hốc mắt không?").
question_text(phat_ban,          "Bạn có phát ban trên da không?").
question_text(non_ra_mau,        "Bạn có nôn ra máu không?").
question_text(di_ngoai_ra_mau,   "Bạn có đi ngoài ra máu không?").
question_text(choang_vang,       "Bạn có cảm giác choáng váng không?").
question_text(mo_mat,            "Bạn có nhìn mờ không?").
question_text(khat_nhieu,        "Bạn có khát nhiều bất thường không?").
question_text(tieu_nhieu,        "Bạn có tiểu nhiều bất thường không?").
question_text(sut_can,           "Bạn có sụt cân không chủ ý không?").

% Red flags
red_flag(kho_tho).
red_flag(dau_nguc).
red_flag(non_ra_mau).
red_flag(di_ngoai_ra_mau).

% -----------------------------
% Disease rules with weights (CF components)
% luat(Disease, [Symptom:Weight, ...])
% -----------------------------
luat(cum, [
  sot:0.5, ho:0.4, nhuc_moi:0.3, dau_dau:0.2, nghet_mui:0.2, chay_mui:0.2
]).

luat(viem_hong, [
  dau_hong:0.6, sot:0.3, ho:0.2
]).

luat(viem_phe_quan, [
  ho:0.6, dom:0.4, kho_tho:0.5
]).

luat(viem_xoang, [
  dau_xoang:0.6, nghet_mui:0.5, chay_mui:0.4, sot:0.2, dau_dau:0.3
]).

luat(da_day, [
  dau_bung:0.6, buon_non:0.4
]).

luat(tieu_chay_cap, [
  tieu_chay:0.6, dau_bung:0.3, buon_non:0.3, sot:0.2
]).

luat(covid19, [
  sot:0.4, ho:0.4, met_moi:0.3, dau_dau:0.2, dau_hong:0.2, mat_vi_giac:0.5, mat_khuu_giac:0.5
]).

luat(cao_huyet_ap, [
  dau_dau:0.5, choang_vang:0.4, mo_mat:0.3, met_moi:0.2
]).

luat(tieu_duong, [
  khat_nhieu:0.6, tieu_nhieu:0.5, sut_can:0.4, met_moi:0.3, mo_mat:0.3
]).

luat(sot_xuat_huyet, [
  sot:0.6, dau_sau_hoc_mat:0.5, dau_dau:0.3, phat_ban:0.3
]).

disease_name(D) :- luat(D,_).

% Department and OTC advice
department(cum,             "Nội tổng quát").
department(viem_hong,       "Tai Mũi Họng").
department(viem_phe_quan,   "Hô hấp").
department(viem_xoang,      "Tai Mũi Họng").
department(da_day,          "Tiêu Hóa").
department(tieu_chay_cap,   "Tiêu Hóa").
department(covid19,         "Nội tổng quát / Hô hấp").
department(cao_huyet_ap,    "Tim Mạch").
department(tieu_duong,      "Nội Tiết").
department(sot_xuat_huyet,  "Truyền Nhiễm").

otc(cum, "Nghỉ ngơi, bù nước; có thể dùng thuốc hạ sốt/giảm đau thông dụng; theo dõi 48h. Nếu nặng hơn, đi khám.").
otc(viem_hong, "Súc họng nước ấm; uống ấm; hạ sốt/giảm đau khi cần; nếu sốt cao kéo dài/khó thở -> đi khám.").
otc(viem_phe_quan, "Nghỉ ngơi, bù nước; nếu ho đờm kéo dài/nặng ngực/khó thở -> đi khám.").
otc(viem_xoang, "Rửa mũi dung dịch muối; nghỉ ngơi; nếu đau xoang nhiều/sốt kéo dài -> đi khám.").
otc(da_day, "Ăn nhẹ, tránh cay–dầu mỡ; có thể dùng thuốc trung hòa acid thông dụng; nếu nôn ói liên tục/đau dữ dội -> đi khám.").
otc(tieu_chay_cap, "Uống oresol/bù nước; ăn lỏng; nếu tiêu chảy máu/sốt cao/mất nước -> đi khám.").
otc(covid19, "Nghỉ ngơi, bù nước; theo dõi hô hấp; đeo khẩu trang; khó thở/lú lẫn -> đi khám.").
otc(cao_huyet_ap, "Nghỉ ngơi, đo lại HA; nếu đau ngực/khó thở/đau đầu dữ dội -> đi cấp cứu.").
otc(tieu_duong, "Uống đủ nước; theo dõi đường huyết nếu có; mệt lả/khát cực độ -> đi khám.").
otc(sot_xuat_huyet, "Theo dõi sát; nếu đau sau hốc mắt/phát ban/sốt cao -> đi khám sớm. Tránh tự ý dùng thuốc khi chưa tư vấn.").

department_of(D, Dept) :- department(D, Dept).
otc_of(D, Advice) :- otc(D, Advice).

% -----------------------------
% Utils
% -----------------------------
one_minus_product([], 1.0).
one_minus_product([W|T], Acc) :- one_minus_product(T, A1), Acc is (1.0 - W) * A1.

cf_from_weights(Ws, CF) :- one_minus_product(Ws, P), CF is 1.0 - P.

cf_of_disease_given(D, Symptoms, CF) :-
  luat(D, Pairs),
  findall(W, (member(S:W, Pairs), memberchk(S, Symptoms)), Ws),
  ( Ws = [] -> CF = 0.0 ; cf_from_weights(Ws, CF) ).

% diagnosis(+Symptoms, -SortedPairs)
% SortedPairs is list of pair(CF,D) sorted descending by CF
diagnosis(Symptoms, SortedPairs) :-
  findall(CF-D, (disease_name(D), cf_of_disease_given(D, Symptoms, CF)), Raw),
  sort(Raw, SortedAsc),
  reverse(SortedAsc, SortedDesc),
  maplist(to_pair, SortedDesc, SortedPairs).

to_pair(CF-D, pair(CF,D)).

% triage
red_flags_in(Symptoms, Flags) :-
  findall(R, (red_flag(R), memberchk(R, Symptoms)), L),
  sort(L, Flags).

triage_level_given(Symptoms, cap_cuu) :-
  red_flags_in(Symptoms, Flags), Flags \= [], !.
triage_level_given(Symptoms, nang) :-
  diagnosis(Symptoms, Pairs),
  (Pairs = [pair(Max,_ )|_], Max >= 0.75), !.
triage_level_given(Symptoms, trung_binh) :-
  diagnosis(Symptoms, Pairs),
  (Pairs = [pair(Max,_ )|_], Max >= 0.50), !.
triage_level_given(_, nhe).

% questions_suggest(+Present, +Max, -Questions)
% Return top-Missing symptoms ranked by weight across all diseases
questions_suggest(Present, Max, Questions) :-
  findall(S-W,
    ( disease_name(D),
      luat(D, Pairs),
      member(S:W, Pairs),
      \+ memberchk(S, Present)
    ),
    AllPairs),
  % Sort by weight desc and keep unique symptoms by first occurrence
  maplist(pair_keyflip, AllPairs, Keyed),       % W-S
  sort(Keyed, SortedAsc),                       % ascending by W
  reverse(SortedAsc, SortedDesc),               % descending by W
  keep_first_by_symptom(SortedDesc, [], Unique),% unique S by first (highest W)
  maplist(pair_unflip, Unique, SWs),            % S-W
  take_first_n_syms(SWs, Max, Questions).

pair_keyflip(S-W, W-S).
pair_unflip(W-S, S-W).

keep_first_by_symptom([], Acc, Acc).
keep_first_by_symptom([W-S|T], Seen, Out) :-
  ( memberchk(S, Seen) -> keep_first_by_symptom(T, Seen, Out)
  ; keep_first_by_symptom(T, [S|Seen], [W-S|Out])
  ).

take_first_n_syms([], _, []).
take_first_n_syms(_, 0, []) :- !.
take_first_n_syms([S-_|T], N, [S|R]) :- N1 is N-1, take_first_n_syms(T, N1, R).

% -----------------------------
% normalize_text(+Text, -Symptoms)
%   Detects symptoms by scanning substrings against all aliases (supports multi-word).
%   Case-insensitive; works with both accented and non-accented aliases we defined above.
% -----------------------------
normalize_text(Text, Symptoms) :-
  string_lower(Text, Lower),
  findall(Sym, (
      alias(AliasStr, Sym),
      string_lower(AliasStr, Al),
      Al \= "",
      sub_string(Lower, _, _, _, Al)
    ), RawSyms),
  sort(RawSyms, Symptoms).
% -----------------------------
normalize_text(Text, Symptoms) :-
  split_string(Text, " ,.;:/\\|-", " \t\n\r", Tokens0),
  maplist(string_lower, Tokens0, Lower),
  include(non_empty, Lower, Tokens),
  maplist(token_to_sym, Tokens, Syms0),
  exclude(=(unknown), Syms0, Syms1),
  sort(Syms1, Symptoms).

non_empty(S) :- S \= "".
token_to_sym(Token, Sym) :- ( alias(Token, Sym) -> true ; Sym = unknown ).
