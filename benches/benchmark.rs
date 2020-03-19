use criterion::{criterion_group, criterion_main, Criterion};

use slang::*;
use std::collections::HashMap;

macro_rules! run_file {
    ($filename: expr, $vars: expr) => {
        use std::io::{BufRead, BufReader, Write};
        let mut reader_lines = $filename.map(|name| {
            BufReader::new(std::fs::File::open(name).expect("invalid filename")).lines()
        });
        let mut interpreter_context = InterpreterContext::default();

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
                    exec_block(&block, &mut $vars, &mut interpreter_context);
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

fn multi_table_bench() {
    let filename = Some("benches/multi_table.slang");
    let mut vars: HashMap<String, Variable> = default_vars!();

    run_file!(filename, vars);
}

fn is_prime_bench() {
    let filename = Some("benches/isprime.slang");
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
    c.bench_function("multi table 10 4 times", |b| b.iter(|| multi_table_bench()));
    c.bench_function("isprime 300", |b| b.iter(|| is_prime_bench()));
    c.bench_function("while 100000", |b| b.iter(|| while_loop_bench()));
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(30).measurement_time(std::time::Duration::from_secs(15));
    targets = criterion_benchmark
}
criterion_main!(benches);
