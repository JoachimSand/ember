use std::collections::HashMap;

use crate::lexer::*;
use crate::parser::*;

type Scope = HashMap<String, NodePtr>;
type Scopes = Vec<Scope>;

pub fn push_new_scope(scopes : &mut Scopes){
    let scope = Scope::new();

    scopes.push(scope);
} 

fn type_check(node: NodePtr){
    
}


pub fn type_check_start(translation_unit : NodePtr) {
    let mut scopes = Scopes::new();
    
    // Global Scope
    push_new_scope(&mut scopes);
    /*
    if let Node::TranslationalUnit { external_declarations } = *translation_unit {
        for external_declaration in external_declarations {
            match *external_declaration {
                Node::FunctionDefinition{..} => {
                    
                }

                Node::Declaration { declaration_specifiers, init_declarator_list } => {
                    

                }

            }
        }
    }
    */

}

