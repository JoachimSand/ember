#[cfg(test)]
mod tests {
    use crate::compile::*;
    use std::fs;

    #[test]
    fn all_parser_tests() {
        let entries = fs::read_dir("tests").unwrap();
        let test_paths: Vec<_> = entries
            .map(|res| res.unwrap().path())
            .filter(|p| p.is_file())
            .collect();
        for test_path in test_paths {
            let contents = fs::read_to_string(test_path).unwrap();
            println!("{contents}");
            parse_only(&contents);
        }
    }
}
