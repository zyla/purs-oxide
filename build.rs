extern crate lalrpop;

fn main() {
    lalrpop::Configuration::new()
        //        .log_debug()
        .process_current_dir()
        .unwrap()
}
