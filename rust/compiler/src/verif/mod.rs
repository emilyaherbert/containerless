pub mod transformer;
pub mod typeinf;
//pub mod typeinf2;
//pub mod typeinf3;
pub mod assertions;
pub mod lift_callbacks;

use crate::{
    types::Exp,
    verif::{assertions::Assertions, lift_callbacks::LiftCallbacks, transformer::Transformer},
};

pub fn verify(exp: &Exp) -> Exp {
    let mut assertions = Assertions::new();
    let mut transformer = Transformer::new();
    let mut lift_callbacks = LiftCallbacks::new();

    assertions.assert_supposed_grammar(&exp);
    assertions.assert_unique_names(&exp);
    assertions.assert_all_options_are_none(&exp);

    let mut exp2 = transformer.transform(&exp);
    crate::verif::typeinf::typeinf(&mut exp2).unwrap();
    let exp3 = lift_callbacks.lift(&exp2);

    return exp3;
}

pub fn verify_from_file(filename: &str) -> Exp {
    let json = std::fs::read_to_string(filename)
        .expect(&format!("could not read file {}", filename));
    let exp = serde_json::from_str::<Exp>(&json)
        .unwrap_or_else(|exp| panic!("\n{:?} \nin \n{}", exp, &json));
    return verify(&exp);
}
