use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::exit;

mod expression;
mod interpreter;
mod lox_error;
mod parser;
mod scanner;
mod token;
mod token_type;
use lox_error::LoxError;
use parser::Parser;
use scanner::Scanner;
use token::Token;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];
    let mut result = 0;
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
                        result = 65;
                    }
                }
            }
            exit(result);
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
            let maybe_expr = parser.parse();
            match maybe_expr {
                Some(e) => print!("{}", e.visit()),
                None => {
                    result = 65;
                }
            }
            exit(result);
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
            let maybe_expr = parser.parse();
            match maybe_expr {
                Some(e) => {
                    let val = interpreter::interpreter::visit(&e);
                    print!("{}", val)
                }
                None => {
                    result = 65;
                }
            }
            exit(result);
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}
