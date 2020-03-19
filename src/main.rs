use slang::*;

use std::collections::HashMap;
use std::io::{self, BufRead, BufReader, Write};

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let filename = args.get(1);
    let mut reader_lines = filename
        .map(|name| BufReader::new(std::fs::File::open(name).expect("invalid filename")).lines());
    let mut vars: HashMap<String, Variable> = default_vars!();

    let mut interpreter_context = InterpreterContext::default();

    loop {
        if filename.is_none() {
            print!("> ");
        }
        io::stdout().flush().unwrap();

        let block_res = parser::read_next_block(&mut reader_lines);
        match block_res {
            Some(block) => {
                let block = parser::process_block(
                    block
                        .iter()
                        .map(|string| string.as_str())
                        .collect::<Vec<&str>>()
                        .as_slice(),
                );
                exec_block(&block, &mut vars, &mut interpreter_context);
            }
            None => break,
        }
    }
}
