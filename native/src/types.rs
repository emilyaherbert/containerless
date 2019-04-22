// These type definitions must be consistent with the definitions in TypeScript.
// We are using serde_json to automatically derive deserialization code. The
// following pages of documentation are relevant:
//
// - https://serde.rs/enum-representations.html
//
//   Illustrates the #[serde(tag = "kind")] directive.
//
// - https://serde.rs/attr-rename.html
//
//   Illustrates the #[serde(rename_all = "camelCase")] directive. Note that
//   the directive also renames type-tags. E.g., Exp::Undefined {} serializes
//   to { type: "undefined" } and not { type: "Undefined" }. The example does
//   not make this clear, but it is what we want.
//
// - I could not find documentation for the #[serde(rename = "+")] directive.
//   Instead, I guessed that it existed and it worked!
use serde::Deserialize;

#[derive(PartialEq, Debug, Deserialize)]
pub enum BinOp {
    #[serde(rename = "+num")]
    Add,
    Mul,
    #[serde(rename = "<")]
    LT
}

#[derive(PartialEq, Debug, Deserialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum Exp {
    Number { value: f64 },
    String { value: String },
    Boolean { value: bool },
    Input {},
    Undefined {},
    Identifier { name: usize },
    #[serde(rename = "binop")]
    BinOp { op: BinOp, e1: Box<Exp>, e2: Box<Exp> }
}

#[derive(PartialEq, Debug, Deserialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum Stmt {
    Let { name: usize, body: Exp },
    Assign { name : String, expr: Exp },
    If { test: Exp, then_part: Box<Stmt>, else_part: Box<Stmt> },
    While { test: Exp, body: Box<Stmt> },
    Block { body: Vec<Stmt> },
    Return { value: Exp }
}

/*
#[cfg(test)]
mod tests {
    use super::*;

    fn parse_exp(s: &str) -> Exp {
        serde_json::from_str(&s).expect("Expression (as JSON)")
    }

    fn parse_binop(s: &str) -> BinOp {
        serde_json::from_str(&s).expect("Expression (as JSON)")
    }

    #[test]
    fn parse_identifer() {
        assert_eq!(parse_exp(r#"
                { "kind": "identifier", "name": 0 }
            "#),
            Exp::Identifier { name: 0 });
    }

    #[test]
    fn parse_add() {
        assert_eq!(parse_binop(r#""+num""#), BinOp::Add);
    }

    #[test]
    fn parse_binop_1() {
        // Writing the expected output would be painful and pointless.
        parse_exp(r#"
                { "kind": "binop",
                  "op": "+num",
                  "e1": { "kind": "number", "value": 5 },
                  "e2": { "kind": "number", "value": 10 }
                }
            "#);
    }

}
*/