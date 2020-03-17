#[cfg(test)]
#[rustfmt::skip]
mod tests {
    use std::collections::HashMap;
    use crate::*;

    #[test]
    fn assign() {
        let mut vars: HashMap<String, Variable> = HashMap::new();
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
    fn while_loop() {
        let mut vars: HashMap<String, Variable> = HashMap::new();
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
        let mut vars: HashMap<String, Variable> = HashMap::new();
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
}
