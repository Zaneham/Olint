# olint

Think [clippy](https://doc.rust-lang.org/clippy/) for OCaml which is
only fair, given that Rust's original compiler was written in OCaml.
We're simply returning the favour.

A linter for OCaml source files. Catches antipatterns, partial function
misuse, naming violations, and the kind of code that works perfectly
well until a Tuesday in March at 3am when it very much doesn't. Uses
`compiler-libs` directly,no custom parser, no PPX dependency, no
build step required. Just very firmly held opinions.

## Install

```
opam pin add olint git+https://github.com/Zaneham/Olint
```

Or, for those who enjoy the journey:

```
git clone https://github.com/Zaneham/Olint
cd olint
opam install cmdliner
dune build
dune install
```

## Usage

Point it at things. It will find fault.

```
olint src/
olint lib/ bin/ test/
olint --severity warn src/   # only the serious stuff
olint --format json src/     # for machines, who judge silently
olint --list-rules           # know thy accuser
olint --lang lang/fr.txt src/
```

## Example Output

```
src/parser.ml:42:3 [W001] unused open: `open Printf` is not referenced
  hint: remove the open or use a qualified name
src/eval.ml:87:5 [W003] List.length in comparison is O(n); compare against [] instead
  hint: use `xs <> []` or `xs = []` for emptiness checks
src/main.ml:12:1 [W005] List.hd may raise; consider a safe alternative
  hint: use List.hd_opt or pattern matching
src/util.ml:31:9 [W008] `if x then true else false` is redundant; use `x` directly
  hint: remove the if/else and use the condition directly
```

Exit codes: `0` clean, `1` warnings, `2` errors. An escalating scale
of disappointment.

## Rules

| ID | Name | Sev | What it objects to |
|----|------|-----|--------------------|
| W001 | unused-open | W | `open Foo` where Foo contributes nothing to proceedings |
| W002 | redundant-match | W | Identity match â€” every arm returns its pattern, achieving nothing with great ceremony |
| W003 | list-length-compare | W | `List.length xs > 0` is O(n); traversing the entire list to check if it exists |
| W004 | list-nth | W | `List.nth` â€” indexing into a linked list, the data structure equivalent of sending a letter to your neighbour |
| W005 | partial-function | W | `List.hd` without a guard â€” works fine until the list is empty, at which point: scenes |
| W006 | naming-convention | I | Enforces snake\_case for values and PascalCase for modules, because convention is all that stands between us and barbarism |
| W008 | bool-redundancy | W | `if x then true else false` â€” the boolean identity crisis |
| W009 | eta-reduce | I | `fun x -> f x` â€” the unnecessary introduction. "Let me introduce you to someone you've already met" |
| W010 | exn-control-flow | I | `try Hashtbl.find ... with Not_found` when `find_opt` is right there |

Run `olint --list-rules` for the full catalogue of grievances.

## Inline Suppression

Because the difference between a useful tool and an annoying one
is the ability to say "yes, I know, shut up about it."

```ocaml
(* olint:disable W001 *)
open Printf  (* no warning ” you've made your choice *)
(* olint:enable W001 *)

(* olint:disable-next-line W005 *)
let first = List.hd xs  (* just this once *)
```

## JSON Output

For CI pipelines, editors, and other systems that prefer
their criticism in machine-readable form:

```
olint --format json src/
```

```json
[
  {
    "file": "src/parser.ml",
    "line": 42,
    "col": 3,
    "rule": "W001",
    "severity": "W",
    "message": "unused open: `open Printf` is not referenced",
    "hint": "remove the open or use a qualified name"
  }
]
```

## Localisation

Every diagnostic has a stable ID (`W001`, `W003`). A developer in
Osaka can Google "olint W003" regardless of what language the message
is in. Architecture lifted wholesale from
[BarraCUDA](https://github.com/Zaneham/barracuda)'s `bc_err.c`, because
why not.


English is compiled in as the default, largely because the
errors were written in English first and we haven't got round
to the others yet. PR's are accepted for other languages. Spanish and French speakers are warned that once my Duolingo hits 60 days, badly translated error messages are coming your way (so get in before then)

Adding a new language is just as simple as copying file in `lang` and then removing all the English, add your own language.

## Architecture

- **Parsetree-based**: `Ast_iterator` on the untyped AST. No `.cmt`
  files, no build step. `olint src/` just works.
- **compiler-libs**: Zero parsing code. The OCaml compiler parses;
  we merely pass judgement.
- **OCaml 5.4.0**: Adapted for the 5.4 `Longident.Ldot` API change,
  which wrapped both arguments in `Location.loc` for reasons best
  known to the compiler team.
- **Minimal deps**: `compiler-libs.common` + `cmdliner`. 

## License

Apache 2.0 see [LICENSE](LICENSE).

