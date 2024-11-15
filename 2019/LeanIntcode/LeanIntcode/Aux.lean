import Lean.Elab.Tactic.Conv.Basic
import Lean.Elab.Command
open Lean Parser.Tactic Parser.Tactic.Conv Elab.Tactic Meta
open Elab Tactic

-- this file is all from Mathlib 

section mathlib_tactic
  elab tk:"#conv " conv:conv " => " e:term : command =>
    Command.runTermElabM fun _ ↦ do
      let e ← Elab.Term.elabTermAndSynthesize e none
      let (rhs, g) ← Conv.mkConvGoalFor e
      _ ← Tactic.run g.mvarId! do
        evalTactic conv
        for mvarId in (← getGoals) do
          liftM <| mvarId.refl <|> mvarId.inferInstance <|> pure ()
        pruneSolvedGoals
        let e' ← instantiateMVars rhs
        logInfoAt tk e'
  
  syntax "#simp" (&" only")? (simpArgs)? " =>"? ppSpace term : command
  macro_rules
    | `(#simp%$tk $[only%$o]? $[[$args,*]]? $[=>]? $e) =>
      `(#conv%$tk simp $[only%$o]? $[[$args,*]]? => $e)

  def elabCheckTactic (tk : Syntax) (ignoreStuckTC : Bool) (term : Term) : TacticM Unit :=
    withoutModifyingStateWithInfoAndMessages <| withMainContext do
      if let `($_:ident) := term then
        -- show signature for `#check ident`
        try
          for c in (← realizeGlobalConstWithInfos term) do
            addCompletionInfo <| .id term c (danglingDot := false) {} none
            logInfoAt tk <| MessageData.signature c
            return
        catch _ => pure ()  -- identifier might not be a constant but constant + projection
      let e ← Term.elabTerm term none
      Term.synthesizeSyntheticMVarsNoPostponing (ignoreStuckTC := ignoreStuckTC)
      let e ← Term.levelMVarToParam (← instantiateMVars e)
      let type ← inferType e
      if e.isSyntheticSorry then
        return
      logInfoAt tk m!"{e} : {type}"

  elab tk:"#check " colGt term:term : tactic => elabCheckTactic tk true term
end mathlib_tactic

namespace List
  def permutationsAux2 (t : α) (ts : List α) (r : List β) : List α → (List α → β) → List α × List β
    | [], _ => (ts, r)
    | y :: ys, f =>
      let (us, zs) := permutationsAux2 t ts r ys (fun x : List α => f (y :: x))
      (y :: us, f (t :: y :: us) :: zs)
  
  def permutationsAux.rec {C : List α → List α → Sort v} (H0 : ∀ is, C [] is)
      (H1 : ∀ t ts is, C ts (t :: is) → C is [] → C (t :: ts) is) : ∀ l₁ l₂, C l₁ l₂
    | [], is => H0 is
    | t :: ts, is =>
        H1 t ts is (permutationsAux.rec H0 H1 ts (t :: is)) (permutationsAux.rec H0 H1 is [])
    termination_by ts is => (length ts + length is, length ts)
    decreasing_by all_goals (simp_wf; omega)
  
  def permutationsAux : List α → List α → List (List α) :=
    permutationsAux.rec (fun _ => []) fun t ts is IH1 IH2 =>
      foldr (fun y r => (permutationsAux2 t ts r y id).2) IH1 (is :: IH2)
  
  def permutations (l : List α) : List (List α) :=
    l :: permutationsAux l []
end List
