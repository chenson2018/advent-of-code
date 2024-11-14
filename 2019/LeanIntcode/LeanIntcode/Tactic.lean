import Lean.Elab.Tactic.Conv.Basic
import Lean.Elab.Command
open Lean Parser.Tactic Parser.Tactic.Conv Elab.Tactic Meta
open Elab Tactic

-- set_option trace.Meta.synthInstance true

-- this is from Mathlib, just for conveinience
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

