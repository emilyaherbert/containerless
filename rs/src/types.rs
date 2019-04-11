// These type definitions must be consistent with the definitions in TypeScript.
// We are using serde_json to automatically derive deserialization code. The
// following pages of documentation are relevant:
//
// - https://serde.rs/enum-representations.html
//
//   Illustrates the #[serde(tag = "type")] directive.
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
    #[serde(rename = "+")]
    Add,
    Mul
}

#[derive(PartialEq, Debug, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum Exp {
    Number { value: f64 },
    String { value: String },
    Boolean { value: bool },
    Undefined {},
    Identifier { name: String },
    #[serde(rename = "binop")]
    BinOp { op: BinOp, e1: Box<Exp>, e2: Box<Exp> }
}

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
                { "type": "identifier", "name": "x" }
            "#),
            Exp::Identifier { name: "x".to_string() });
    }

    #[test]
    fn parse_add() {
        assert_eq!(parse_binop(r#""+""#), BinOp::Add);
    }

    #[test]
    fn parse_binop_1() {
        // Writing the expected output would be painful and pointless.
        parse_exp(r#"
                { "type": "binop",
                  "op": "+",
                  "e1": { "type": "number", "value": 5 },
                  "e2": { "type": "number", "value": 10 }
                }
            "#);
    }

}