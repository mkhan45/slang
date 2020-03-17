#[cfg(test)]
#[rustfmt::skip]
pub mod tests {
    use std::collections::HashMap;
    use crate::*;

    macro_rules! run_file {
        ($filename: expr, $vars: expr) => {
            use std::io::{BufReader, Write};

            let mut reader_lines = $filename.map(|name| BufReader::new(std::fs::File::open(name).expect("invalid filename")).lines());

            loop {
                io::stdout().flush().unwrap();

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
                    },
                    None => break,
                }
            }
        }
    }

    #[test]
    fn assign() {
        let mut vars: HashMap<String, Variable> = default_vars!();
        let block = vec![
            "let x = 5".to_string()
        ];
        let block = process_block(
            block.iter()
            .map(|string| string.as_str())
            .collect::<Vec<&str>>()
            .as_slice(),
        );
        exec_block(block, &mut vars);
        assert_eq!(vars.get("x").unwrap(), &Variable::Integer(5));
    }

    #[test]
    fn expression_eval() {
        let mut vars: HashMap<String, Variable> = default_vars!();
        let block = vec![
            "let a = 5".to_string(),
            "let b = 5 * 10".to_string(),
            "let c = 5 * 10 + 3".to_string(),
            "let d = 5 * (10 + 3)".to_string(),
            "let e = 5 + 10 * 3".to_string(),
        ];
        let block = process_block(
            block.iter()
            .map(|string| string.as_str())
            .collect::<Vec<&str>>()
            .as_slice(),
        );
        exec_block(block, &mut vars);

        assert_eq!(vars.get("a").unwrap(), &Variable::Integer(5));
        assert_eq!(vars.get("b").unwrap(), &Variable::Integer(5 * 10));
        assert_eq!(vars.get("c").unwrap(), &Variable::Integer(5 * 10 + 3));
        assert_eq!(vars.get("d").unwrap(), &Variable::Integer(5 * (10 + 3)));
        assert_eq!(vars.get("e").unwrap(), &Variable::Integer(5 + 10 * 3));
    }

    #[test]
    fn casting() {
        let mut vars: HashMap<String, Variable> = default_vars!();

        let block = vec![
            "let a = 5 as String".to_string(),
        ];
        let block = process_block(
            block.iter()
            .map(|string| string.as_str())
            .collect::<Vec<&str>>()
            .as_slice(),
        );
        exec_block(block, &mut vars);

        assert_eq!(vars.get("a").unwrap(), &Variable::Str("5".to_string()));
    }

    #[test]
    fn while_loop() {
        let mut vars: HashMap<String, Variable> = default_vars!();
        let block = vec![
            "let x = 0".to_string(),
            "while x < 5 {".to_string(),
            "x = x + 1".to_string(),
            "}".to_string(),
        ];
        let block = process_block(
            block.iter()
            .map(|string| string.as_str())
            .collect::<Vec<&str>>()
            .as_slice(),
        );
        exec_block(block, &mut vars);
        assert_eq!(vars.get("x").unwrap(), &Variable::Integer(5));
    }

    #[test]
    fn nested_while_loop() {
        let mut vars: HashMap<String, Variable> = default_vars!();
        let block = vec![
            "let x = 0".to_string(),
            "let c = 0".to_string(),
            "while x < 5 {".to_string(),
            "let y = 0".to_string(),
            "while y < 5 {".to_string(),
            "c = x * y".to_string(),
            "y = y + 1".to_string(),
            "}".to_string(),
            "x = x + 1".to_string(),
            "}".to_string(),
        ];
        let block = process_block(
            block.iter()
            .map(|string| string.as_str())
            .collect::<Vec<&str>>()
            .as_slice(),
        );
        exec_block(block, &mut vars);
        assert_eq!(vars.get("x").unwrap(), &Variable::Integer(5));
        assert_eq!(vars.get("c").unwrap(), &Variable::Integer(16));
    }

    #[test]
    fn simple_fileread() {
        let filename = Some("tests/simple_fileread.slang");
        let mut vars: HashMap<String, Variable> = default_vars!();
        run_file!(filename, vars);

        assert_eq!(vars.get("x").unwrap(), &Variable::Integer(5));
    }

    #[test]
    fn multi_table() {
        let filename = Some("tests/multi_table.slang");
        let mut vars: HashMap<String, Variable> = default_vars!();
        run_file!(filename, vars);

        assert_eq!(vars.get("x").unwrap(), &Variable::Integer(10));
    }

    #[test]
    pub fn fib_test() {
        fn fib(n: isize) -> isize{
            let mut a = 1isize;
            let mut b = 1isize;

            (0..n).for_each(|_|{
                let c = b;
                b = a + b;
                a = c;
            });

            a
        }

        let filename = Some("tests/fib_fast.slang");
        let mut vars: HashMap<String, Variable> = default_vars!();

        run_file!(filename, vars);

        assert_eq!(vars.get("a").unwrap(), &Variable::Integer(fib(24)));
    }
}
