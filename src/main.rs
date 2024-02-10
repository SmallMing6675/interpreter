use compiler::lex::lex::lex;
use compiler::parse::parse::parse;
use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        loop {
            print!(">> ");
            io::stdout().flush().unwrap();

            // Read input from the user
            let mut input = String::new();
            io::stdin()
                .read_line(&mut input)
                .expect("Failed to read line");

            // Lex and parse input
            let tokens = lex(&input).expect("Failed to lex input");

            println!("{:?}", tokens);
            let ast = parse(tokens).expect("Failed to parse input");

            println!("{:?}", ast);
        }
    }

    let filename = &args[1];
    let source_code = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(_) => {
            eprintln!("Error: Could not read file '{}'", filename);
            process::exit(1);
        }
    };
}
