use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::exit;

mod lox_error;
mod scanner;
mod token;
mod token_type;
use scanner::Scanner;
use token::Literal;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            writeln!(io::stderr(), "Logs from your program will appear here!").unwrap();
            let mut result = 0;

            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            // Uncomment this block to pass the first stage
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
                        writeln!(io::stderr(), "{}", e).unwrap();
                        result = 65;
                    }
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
