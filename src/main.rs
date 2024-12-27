use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::exit;

mod environments;
mod expression;
mod interpreter;
mod lox_error;
mod parser;
mod scanner;
mod token;
mod token_type;
use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];
    let mut result: i32 = 0;
    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
        String::new()
    });

    match command.as_str() {
        "tokenize" => {
            let mut scanner = Scanner::new(&file_contents);
            let tokens = scanner.scan_tokens();
            for token in tokens {
                match token {
                    Ok(t) => {
                        let literal_as_str: String = t
                            .get_literal()
                            .clone()
                            .map(|f| format!("{f}"))
                            .unwrap_or("null".into());
                        println!(
                            "{} {} {}",
                            t.get_token_type(),
                            t.get_lexeme(),
                            literal_as_str
                        );
                    }
                    Err(e) => {
                        e.report();
                        result = e.get_error_code();
                    }
                }
            }
        }
        "parse" => {
            let mut scanner = Scanner::new(&file_contents);
            let maybe_tokens = scanner.scan_tokens();
            let tokens = maybe_tokens
                .iter()
                .filter_map(|result| result.as_ref().ok())
                .cloned()
                .collect();
            let parser = Parser::new(&tokens);
            let maybe_expr = parser.parse_expression();
            match maybe_expr {
                Some(e) => print!("{}", e.visit()),
                None => {
                    result = 65;
                }
            }
        }
        "evaluate" => {
            let mut scanner = Scanner::new(&file_contents);
            let maybe_tokens = scanner.scan_tokens();
            let tokens = maybe_tokens
                .iter()
                .filter_map(|result| result.as_ref().ok())
                .cloned()
                .collect();
            let parser = Parser::new(&tokens);
            let maybe_expr = parser.parse_expression();
            let mut i = Interpreter::new();
            match maybe_expr {
                Some(e) => {
                    let val = i.interpret_expression(&e);
                    match val {
                        Err(e) => {
                            result = e.get_error_code();
                            e.report()
                        }
                        Ok(v) => print!("{}", v),
                    }
                }
                None => {
                    result = 65;
                }
            }
        }
        "run" => {
            let mut scanner = Scanner::new(&file_contents);
            let maybe_tokens = scanner.scan_tokens();
            let tokens = maybe_tokens
                .iter()
                .filter_map(|result| result.as_ref().ok())
                .cloned()
                .collect();
            let parser = Parser::new(&tokens);
            let maybe_statements = parser.parse();
            let mut i = Interpreter::new();
            match maybe_statements {
                Ok(s) => {
                    result = i.interpret_statements(&s);
                }
                Err(e) => {
                    e.report();
                    result = e.get_error_code();
                }
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }

    exit(result);
}
