use dispatcher_agent_lib :: trace_runtime :: Containerless ; use std :: collections :: HashMap ; mod function_foo ; pub fn init ( ) -> HashMap < & 'static str , Containerless > { let mut ht : HashMap < & 'static str , Containerless > = HashMap :: new ( ) ; ht . insert ( "foo" , function_foo :: containerless ) ; return ht ; }