use compiler::eval::eval::eval;
use compiler::eval::eval::Environment;
use compiler::lex::lex::lex;
use compiler::parse::parse::parse;
use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    println!("Welcome to the REPL! Enter Ctrl+C to exit.");
    if args.len() != 2 {
        let mut lines = Vec::new();
        loop {
            let mut input = String::new();
            io::stdin()
                .read_line(&mut input)
                .expect("Failed to read line");

            lines.push(input.trim().to_string());

            println!("Lines: {:?}", lines);

            let tokens = lex(&lines.join(" ")).expect("Failed to lex input");

            println!("Tokens: {:?}", tokens);
            let ast = parse(tokens).expect("Failed to parse input");

            println!("AST: {:?}", ast);
            let env = &mut Environment::new();
            for node in ast {
                let value = eval(&node, env).unwrap();
                println!("Result: {:?}", value);
            }
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
