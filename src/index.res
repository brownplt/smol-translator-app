/*

Let this module does all the bootstrap.

*/

%%raw("import './index.css'")

@module("./highlight") external highlight: string => unit = "highlight"

module Syntax = {
  type t =
    | Lispy
    | Python
    | JavaScript
    | Scala
    | PseudoCode
  let toString = t => {
    switch t {
    | Lispy => "Lispy"
    | Python => "Python"
    | JavaScript => "JavaScript"
    | Scala => "Scala"
    | PseudoCode => "PseudoCode"
    }
  }
  let fromString = s => {
    switch s {
    | "Lispy" => Lispy
    | "Python" => Python
    | "JavaScript" => JavaScript
    | "Scala" => Scala
    | "PseudoCode" => PseudoCode
    | _ => Lispy
    }
  }
  let all = [Lispy, Python, JavaScript, Scala, PseudoCode]
}

let stringOfSrcloc = (srcloc: SExpression.sourcePoint): string => {
  let {ln, ch} = srcloc
  `${ln |> Belt.Int.toString}-${ch |> Belt.Int.toString}`
}

let stringOfSrcrange = (srcrange: SExpression.sourceLocation): string => {
  let {begin, end} = srcrange
  `srcrange-${begin |> stringOfSrcloc}-${end |> stringOfSrcloc}`
}


let stringOfKindedSrcrange = (srcrange: SMoL.kindedSourceLocation): string => {
  let {nodeKind, sourceLocation} = srcrange
  `${SMoL.NodeKind.toString(nodeKind)}-${stringOfSrcrange(sourceLocation)}`
}

let reactOfPrint = (p: SMoL.print<SMoL.kindedSourceLocation>, sourceMap): React.element => {
  let rec reactOfAnnotatedPrint = ({it, ann}: SMoL.print<SMoL.kindedSourceLocation>) => {
    let ann = switch ann {
    | None => it => it
    | Some(ann) =>
      it => {
        let className = stringOfKindedSrcrange(ann)
        <span
          title={Belt.Map.get(sourceMap, ann)
          ->Belt.Option.map(SExpression.SourceLocation.toString)
          ->Belt.Option.getWithDefault("")}
          className
          onMouseEnter={event => {
            highlight(className)
            ReactEvent.Mouse.preventDefault(event)
          }}>
          {it}
        </span>
      }
    }
    switch it {
    | Plain("") => <> </>
    | Group(list{}) => <> </>
    | Plain(s) => ann(React.string(s))
    | Group(es) => ann(React.array(es |> Belt.List.toArray |> Array.map(reactOfAnnotatedPrint)))
    }
  }
  reactOfAnnotatedPrint(p)
}

module Kind = {
  type t =
    | Output
    | Term
    | Program
  let toString = t => {
    switch t {
    | Output => "Output"
    | Term => "Term"
    | Program => "Program"
    }
  }
  let fromString = t => {
    switch t {
    | "Output" => Output
    | "Term" => Term
    | "Program" => Program
    | _ => Program
    }
  }
  let all = [Output, Term, Program]
}

module SourceLocationCmp = Belt.Id.MakeComparable({
  type t = SMoL.kindedSourceLocation
  let cmp = (a, b) => Pervasives.compare(a, b)
})

module App = {
  @react.component
  let make = () => {
    let (source, setSource) = React.useState(_ => "")
    let (kind, setKind) = React.useState(_ => Kind.Program)
    let tr = (kind, targetSyntax, source): React.element => {
      switch switch kind {
      | Kind.Term =>
        switch targetSyntax {
        | Syntax.Lispy => source
        | Python => SMoL.PYTranslator.translateStandAloneTerm(source)
        | JavaScript => SMoL.JSTranslator.translateStandAloneTerm(source)
        | Scala => SMoL.SCTranslator.translateStandAloneTerm(source)
        | PseudoCode => SMoL.PCTranslator.translateStandAloneTerm(source)
        } |> React.string
      | Kind.Output =>
        switch targetSyntax {
        | Syntax.Lispy => source
        | Python => SMoL.PYTranslator.translateOutput(source)
        | JavaScript => SMoL.JSTranslator.translateOutput(source)
        | Scala => SMoL.SCTranslator.translateOutput(source)
        | PseudoCode => SMoL.PCTranslator.translateOutput(source)
        } |> React.string
      | Program =>
        switch targetSyntax {
        | Syntax.Lispy => source |> React.string
        | Python => {
            let print = SMoL.getProgramPrint(SMoL.PYTranslator.translateProgramFull(true, source))
            let sourceMap = SMoL.Print.toSourceMap(print, module(SourceLocationCmp))
            reactOfPrint(print, sourceMap)
          }
        | JavaScript => {
            let print = SMoL.getProgramPrint(SMoL.JSTranslator.translateProgramFull(true, source))
            let sourceMap = SMoL.Print.toSourceMap(print, module(SourceLocationCmp))
            reactOfPrint(print, sourceMap)
          }
        | Scala => {
            let print = SMoL.getProgramPrint(SMoL.SCTranslator.translateProgramFull(true, source))
            let sourceMap = SMoL.Print.toSourceMap(print, module(SourceLocationCmp))
            reactOfPrint(print, sourceMap)
          }
        | PseudoCode => {
            let print = SMoL.getProgramPrint(SMoL.PCTranslator.translateProgramFull(true, source))
            let sourceMap = SMoL.Print.toSourceMap(print, module(SourceLocationCmp))
            reactOfPrint(print, sourceMap)
          }
        }
      } {
      | exception SMoL.SMoLTranslateError(err) =>
        <mark> {SMoL.TranslateError.toString(err) |> React.string} </mark>
      | it => it
      }
    }
    <main>
      <section>
        <h2>
          {React.string("Source ")}
          <select
            onChange={evt => {
              let k: string = ReactEvent.Form.currentTarget(evt)["value"]
              let k = Kind.fromString(k)
              setKind(_ => k)
            }}>
            {React.array(
              Kind.all |> Array.map(k => {
                let value = k |> Kind.toString
                <option value selected={k == kind}> {React.string(value)} </option>
              }),
            )}
          </select>
          {React.string(":")}
        </h2>
        <textarea
          name="sourceProgram"
          value={source}
          rows={20}
          cols={40}
          onChange={evt => {
            let x = ReactEvent.Form.currentTarget(evt)["value"]
            setSource(_ => x)
          }}
        />
      </section>
      <section id={"translations"}>
        <h2>
          {React.string(`Translated `)}
          <u> {React.string(kind |> Kind.toString)} </u>
        </h2>
        <div>
          {React.array(
            Syntax.all |> Array.mapi((i, syntax) => {
              <article key={Js.Int.toString(i)}>
                <h3> {React.string(Syntax.toString(syntax))} </h3>
                {switch tr(kind, syntax, source) {
                | exception SMoL.SMoLPrintError(err) => React.string(err)
                | target => <pre> {target} </pre>
                }}
              </article>
            }),
          )}
        </div>
      </section>
    </main>
  }
}

switch ReactDOM.querySelector("#root") {
| Some(rootElement) => {
    let root = ReactDOM.Client.createRoot(rootElement)
    ReactDOM.Client.Root.render(root, <App />)
  }
| None => ()
}
