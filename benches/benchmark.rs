use criterion::{criterion_group, criterion_main, Criterion};

use slang::*;
use std::collections::HashMap;

macro_rules! run_file {
    ($filename: expr, $vars: expr) => {
        use std::io::{BufRead, BufReader, Write};
        let mut reader_lines = $filename.map(|name| {
            BufReader::new(std::fs::File::open(name).expect("invalid filename")).lines()
        });

        loop {
            std::io::stdout().flush().unwrap();

            let block_res = Lexer::read_next_block(&mut reader_lines);
            match block_res {
                Some(block) => {
                    let block = process_block(
                        block
                            .iter()
                            .map(|string| string.as_str())
                            .collect::<Vec<&str>>()
                            .as_slice(),
                    );
                    exec_block(block, &mut $vars);
                }
                None => break,
            }
        }
    };
}

fn fib_bench() {
    let filename = Some("benches/fib_fast.slang");
    let mut vars: HashMap<String, Variable> = default_vars!();

    run_file!(filename, vars);
}

fn while_loop_bench() {
    let filename = Some("benches/while_loop_bench.slang");
    let mut vars: HashMap<String, Variable> = default_vars!();

    run_file!(filename, vars);
}

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib 25", |b| b.iter(|| fib_bench()));
    c.bench_function("while 100000", |b| b.iter(|| while_loop_bench()));
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(15);
    targets = criterion_benchmark
}
criterion_main!(benches);
