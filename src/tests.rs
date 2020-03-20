#[cfg(test)]
#[rustfmt::skip]
pub mod tests {
    use std::collections::HashMap;
    use crate::{*, parser};
    use std::io;

    macro_rules! run_file {
        ($filename: expr, $vars: expr) => {
            use std::io::{BufReader, Write, BufRead};
            let mut interpreter_context = InterpreterContext::default();

            let mut reader_lines = $filename.map(|name| BufReader::new(std::fs::File::open(name).expect("invalid filename")).lines());

            loop {
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
                        exec_block(&block, &mut $vars, &mut interpreter_context);
                    },
                    None => break,
                }
            }
        }
    }

    #[test]
    fn assign() {
        let mut vars: HashMap<String, Variable> = default_vars!();
        let mut interpreter_context = InterpreterContext::default();
        let block = vec![
            "let x = 5".to_string()
        ];
        let block = parser::process_block(
            block.iter()
            .map(|string| string.as_str())
            .collect::<Vec<&str>>()
            .as_slice(),
        );
        exec_block(&block, &mut vars, &mut interpreter_context);
        assert_eq!(vars.get("x").unwrap(), &Variable::Integer(5));
    }

    #[test]
    fn expression_eval() {
        let mut vars: HashMap<String, Variable> = default_vars!();
        let mut interpreter_context = InterpreterContext::default();

        let block = vec![
            "let a = 5".to_string(),
            "let b = 5 * 10".to_string(),
            "let c = 5 * 10 + 3".to_string(),
            "let d = 5 * (10 + 3)".to_string(),
            "let e = 5 + 10 * 3".to_string(),
        ];
        let block = parser::process_block(
            block.iter()
            .map(|string| string.as_str())
            .collect::<Vec<&str>>()
            .as_slice(),
        );
        exec_block(&block, &mut vars, &mut interpreter_context);

        assert_eq!(vars.get("a").unwrap(), &Variable::Integer(5));
        assert_eq!(vars.get("b").unwrap(), &Variable::Integer(5 * 10));
        assert_eq!(vars.get("c").unwrap(), &Variable::Integer(5 * 10 + 3));
        assert_eq!(vars.get("d").unwrap(), &Variable::Integer(5 * (10 + 3)));
        assert_eq!(vars.get("e").unwrap(), &Variable::Integer(5 + 10 * 3));
    }

    #[test]
    fn casting() {
        let mut vars: HashMap<String, Variable> = default_vars!();
        let mut interpreter_context = InterpreterContext::default();

        let block = vec![
            "let a = 5 as String".to_string(),
        ];
        let block = parser::process_block(
            block.iter()
            .map(|string| string.as_str())
            .collect::<Vec<&str>>()
            .as_slice(),
        );
        exec_block(&block, &mut vars, &mut interpreter_context);

        assert_eq!(vars.get("a").unwrap(), &Variable::Str(Rc::new("5".to_string())));
    }

    #[test]
    fn while_loop() {
        let mut vars: HashMap<String, Variable> = default_vars!();
        let mut interpreter_context = InterpreterContext::default();

        let block = vec![
            "let x = 0".to_string(),
            "while x < 5 {".to_string(),
            "x = x + 1".to_string(),
            "}".to_string(),
        ];
        let block = parser::process_block(
            block.iter()
            .map(|string| string.as_str())
            .collect::<Vec<&str>>()
            .as_slice(),
        );
        exec_block(&block, &mut vars, &mut interpreter_context);
        assert_eq!(vars.get("x").unwrap(), &Variable::Integer(5));
    }

    #[test]
    fn nested_while_loop() {
        let mut vars: HashMap<String, Variable> = default_vars!();
        let mut interpreter_context = InterpreterContext::default();
        let block = vec![
            "let x = 0".to_string(),
            "let c = 0".to_string(),
            "while x < 5 {".to_string(),
                "let y = 0".to_string(),
                "while y < 5 {".to_string(),
                    "let d = 0".to_string(),
                    "while d < 5 {".to_string(),
                        "c = x * y * d".to_string(),
                        "d = d + 1".to_string(),
                    "}".to_string(),
                    "y = y + 1".to_string(),
                "}".to_string(),
                "x = x + 1".to_string(),
            "}".to_string(),
        ];
        let block = parser::process_block(
            block.iter()
            .map(|string| string.as_str())
            .collect::<Vec<&str>>()
            .as_slice(),
        );
        exec_block(&block, &mut vars, &mut interpreter_context);
        assert_eq!(vars.get("x").unwrap(), &Variable::Integer(5));
        assert_eq!(vars.get("c").unwrap(), &Variable::Integer(4 * 4 * 4));
    }

    #[test]
    fn simple_fileread() {
        let filename = Some("tests/simple_fileread.slang");
        let mut vars: HashMap<String, Variable> = default_vars!();
        run_file!(filename, vars);

        assert_eq!(vars.get("x").unwrap(), &Variable::Integer(5));
    }

    #[test]
    fn if_statement() {
        let filename = Some("tests/if_test.slang");
        let mut vars: HashMap<String, Variable> = default_vars!();
        run_file!(filename, vars);

        assert_eq!(vars.get("x").unwrap(), &Variable::Integer(1));
        assert_eq!(vars.get("y").unwrap(), &Variable::Integer(0));
        assert_eq!(vars.get("c").unwrap(), &Variable::Integer(3));
    }

    #[test]
    fn if_else_statement() {
        let filename = Some("tests/if_else_test.slang");
        let mut vars: HashMap<String, Variable> = default_vars!();
        run_file!(filename, vars);

        assert_eq!(vars.get("x").unwrap(), &Variable::Integer(1));
        assert_eq!(vars.get("y").unwrap(), &Variable::Integer(2));
    }

    #[test]
    fn if_elif_statement() {
        let filename = Some("tests/elif.slang");
        let mut vars: HashMap<String, Variable> = default_vars!();
        run_file!(filename, vars);

        assert_eq!(vars.get("a").unwrap(), &Variable::Bool(false));
        assert_eq!(vars.get("b").unwrap(), &Variable::Bool(false));
        assert_eq!(vars.get("c").unwrap(), &Variable::Bool(true));
        assert_eq!(vars.get("d").unwrap(), &Variable::Bool(false));
    }

    #[test]
    fn basic_fn() {
        let filename = Some("tests/basic_function.slang");
        let mut vars: HashMap<String, Variable> = default_vars!();
        run_file!(filename, vars);

        assert_eq!(vars.get("x").unwrap(), &Variable::Integer(13));
    }

    #[test]
    fn multi_table() {
        let filename = Some("tests/multi_table.slang");
        let mut vars: HashMap<String, Variable> = default_vars!();
        run_file!(filename, vars);

        assert_eq!(vars.get("x").unwrap(), &Variable::Integer(10));
    }

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

    #[test]
    pub fn fib_test() {
        let filename = Some("tests/fib_fast.slang");
        let mut vars: HashMap<String, Variable> = default_vars!();

        run_file!(filename, vars);

        assert_eq!(vars.get("a").unwrap(), &Variable::Integer(fib(24)));
    }

    #[test]
    fn fib_recurse() {
        let filename = Some("tests/fib_recurse.slang");
        let mut vars: HashMap<String, Variable> = default_vars!();
        run_file!(filename, vars);

        assert_eq!(vars.get("x").unwrap(), &Variable::Integer(fib(7)));
    }
}
