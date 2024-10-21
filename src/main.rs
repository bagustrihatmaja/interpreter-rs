mod scanner;
mod token;
mod token_type;

use std::{env, fs, io, process::exit};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        println!("usage interpreter-rs [script]");
        exit(64)
    } else if args.len() == 1 {
        let file_path: &String = &args[1];
        run_file(file_path);
    } else {
        run_prompt();
    }
}

fn run_file(file_path: &String) {
    let contents: String =
        fs::read_to_string(file_path).expect("Something went wrong when reading the file.");
    run(contents);
}

fn run_prompt() {
    loop {
        let mut prompt: String = String::new();
        print!("> ");
        io::stdin()
            .read_line(&mut prompt)
            .expect("Failed to read the line.");
        if !prompt.is_empty() {
            run(prompt);
        }
    }
}

fn run(contents: String) {
    unimplemented!();
}
