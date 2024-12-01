import Lean

-- This is borrowed from https://github.com/kmill/kmill-aoc2023/blob/main/AoC2023/CmdAttr.lean

namespace AoC
open Lean

abbrev AocMainFn := List String → IO Unit

structure AocFn where
  cmd : String
  deriving Inhabited

initialize aocMainFunctions : MapDeclarationExtension AocFn ← mkMapDeclarationExtension

syntax (name := aocMainAttr) "aoc_main " ident : attr

initialize
  registerBuiltinAttribute {
    name := `aocMainAttr
    descr := "add a main entry point for the aoc executable"
    add   := fun decl stx kind => do
      unless kind == AttributeKind.global do
        throwError "invalid `[aoc_main]` attribute, must be global"
      match stx with
      | `(attr| aoc_main $cmd) => withRef stx[0] do
        let .str .anonymous cmd := cmd.getId | throwError "identifier must be atomic"
        modifyEnv fun env => aocMainFunctions.insert env decl {cmd}
      | _  => throwError "invalid `[aoc_main]` attribute"
  }

elab "aoc_dispatch%" args:term : term => do
  let s := (aocMainFunctions.toEnvExtension.getState (← getEnv)).importedEntries.foldl (· ++ ·) #[]
  let base ← `(throw <| IO.userError s!"no such command '{cmd}'")
  let body : Term ← s.foldrM (init := base) (fun (c, fn) t =>
    `(if cmd == $(quote fn.cmd) then $(mkIdent c) args else $t))
  Elab.Term.elabTermEnsuringType
    (← `(do
      let cmd :: args := $args | throw <| IO.userError s!"expecting at least one argument, a command name"
      return ← $body))
    (some <| (← Elab.Term.elabType (← `(IO Unit))))
