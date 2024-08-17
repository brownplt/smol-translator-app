/*

Let this module does all the bootstrap.

*/

%%raw("import './index.css'")

@module("./highlight") external highlight: string => unit = "highlight"
@module("./highlight") external unhighlight: string => unit = "unhighlight"

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

let stringOfSrcloc = (srcloc: SExpression.SrcLoc.t): string => {
  let {ln, ch} = srcloc
  `${ln |> Belt.Int.toString}-${ch |> Belt.Int.toString}`
}

let stringOfSrcrange = (srcrange: SExpression.srcrange): string => {
  let {begin, end} = srcrange
  `srcrange-${begin |> stringOfSrcloc}-${end |> stringOfSrcloc}`
}

let rec reactOfPrint = (p: SMoL.Print.t): React.element => {
  switch p {
  | Plain(s) => React.string(s)
  | Group(es) => React.array(es |> Belt.List.toArray |> Array.map(reactOfAnnotatedPrint))
  }
}
and reactOfAnnotatedPrint = ({it, ann}) => {
  let ann = switch ann {
  | None => it => it
  | Some(ann) =>
    it => {
      let className = stringOfSrcrange(ann)
      <span
        className
        // onMouseMove={event => {
        //   highlight(className)
        //   ReactEvent.Mouse.preventDefault(event)
        // }}
        onMouseEnter={event => {
          highlight(className)
          ReactEvent.Mouse.preventDefault(event)
        }}
        // onMouseLeave={event => {
        //   unhighlight(className)
        //   ReactEvent.Mouse.preventDefault(event)
        // }}
      >
        {it}
      </span>
    }
  }
  switch it {
  | SMoL.Print.Plain("") => <> </>
  | Group(list{}) => <> </>
  | Plain(s) => ann(React.string(s))
  | Group(es) => ann(React.array(es |> Belt.List.toArray |> Array.map(reactOfAnnotatedPrint)))
  }
}

module App = {
  @react.component
  let make = () => {
    let (source, setSource) = React.useState(_ => "")
    let print = targetSyntax => {
      switch targetSyntax {
      | Syntax.Lispy => SMoL.SMoLPrinter.printProgramFull(true)
      | Python => SMoL.PYPrinter.printProgramFull(true)
      | JavaScript => SMoL.JSPrinter.printProgramFull(true)
      | Scala => SMoL.SCPrinter.printProgramFull(true)
      | PseudoCode => SMoL.PCPrinter.printProgramFull(true)
      }
    }
    <main>
      <section>
        <h2> {React.string("Source Program:")} </h2>
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
        <h2> {React.string("Translations")} </h2>
        <div>
          {switch SMoL.Parser.parseProgram(source) {
          | exception SMoL.SMoLParseError(err) => React.string(SMoL.ParseError.toString(err))
          | source =>
            React.array(
              Syntax.all |> Array.mapi((i, s) => {
                <article key={Js.Int.toString(i)}>
                  <h3> {React.string(Syntax.toString(s))} </h3>
                  {switch print(s)(source) {
                  | exception SMoL.SMoLPrintError(err) => React.string(err)
                  | target => <pre> {target.ann.print |> reactOfPrint} </pre>
                  }}
                </article>
              }),
            )
          }}
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
