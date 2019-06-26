
pub mod types {

    use im_rc::hashmap::HashMap;

    pub type Env = HashMap<String, Type>;

    #[derive(Debug)]
    pub enum AExp {
        Bool(bool),
        Int(i32),
        Id(String),
        From(String, String)
    }

    #[derive(Debug)]
    pub enum CExp {
        AExp(AExp),
        Seq(Box<CExp>, Box<CExp>),
        BinOp(Box<CExp>, Box<CExp>),
        Let(String, Box<CExp>, Box<CExp>),
        Set(String, Box<CExp>),
        If(Box<CExp>, Box<CExp>, Box<CExp>),
        While(Box<CExp>, Box<CExp>),
        Label(String, Box<CExp>),
        Break(String, Box<CExp>),
        Unknown,
        Clos(HashMap<String, Box<CExp>>),
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