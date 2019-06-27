
pub mod types {

    use serde::Deserialize;

    use im_rc::hashmap::HashMap;

    pub type Env = HashMap<String, Type>;

    // Constants
    #[derive(Debug, Deserialize)]
    pub enum AExp {
        Bool(bool),
        Int(i32),
        Id(String),
        From(String, String)
    }

    /*
        The traces do something weird with the grammar. In the original JS grammar,
        the only production that used bexp was:
        
        cexp ::= let x = bexp in cexp

        But because bexp includes function application, which translates to a pyramid of let's, it becomes:

        bexp ::= let x = bexp in cexp
            ...
        cexp ::= let x = bexp in cexp
            ...
    */

    // Named Expressions
    #[derive(Debug, Deserialize)]
    pub enum BExp {
        AExp(AExp),
        BinOp(AExp, AExp),
        Clos(Vec<(String, Box<CExp>)>),
        Let(String, Box<BExp>, Box<CExp>),
    }

    // Commands
    #[derive(Debug, Deserialize)]
    pub enum CExp {
        AExp(AExp),
        Seq(Box<CExp>, Box<CExp>),
        Let(String, Box<BExp>, Box<CExp>),
        Set(String, AExp),
        If(AExp, Box<CExp>, Box<CExp>),
        While(AExp, Box<CExp>),
        Label(String, Box<CExp>),
        Break(String, AExp),
        Unknown,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Type {
        TBool,
        TInt,
        TUnknown,
        TUnit,
        TClos(HashMap<String, Type>),
    }

}