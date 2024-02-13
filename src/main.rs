use compiler::eval::eval::eval;
use compiler::eval::eval::Environment;
use compiler::lex::lex::lex;
use compiler::parse::parse::parse;
use std::env;
use std::fs;
use std::io;
use std::io::Write;

fn main() {
    let args: Vec<String> = env::args().collect();
    let debug_mode = args.iter().any(|arg| arg == "--debug");

    if (args.len() == 2 && debug_mode) || (args.len() == 1 && !debug_mode) {
        println!("Welcome to the REPL! Enter Ctrl+C to exit.");
        let mut lines = Vec::new();
        loop {
            let mut input = String::new();
            print!(">> ");
            io::stdout().flush().unwrap();
            io::stdin()
                .read_line(&mut input)
                .expect("Failed to read line");

            lines.push(input.trim().to_string());

            if debug_mode {
                println!("Lines: {:?}", lines);
            }

            let tokens = lex(&lines.join("\n")).expect("Failed to lex input");

            if debug_mode {
                println!("Tokens: {:?}", tokens);
            }
            let ast = parse(tokens).expect("Failed to parse input");

            if debug_mode {
                println!("AST: {:?}", ast);
            }
            let env = &mut Environment::new();
            for node in &ast {
                let value = eval(node, env).unwrap();
                if debug_mode {
                    println!("Result: {:?}", value);
                }

                if Some(node) == ast.last() {
                    println!("{:?}", value);
                }
            }
        }
    }

    let filename = &args[1];
    let source_code = fs::read_to_string(filename).unwrap();
    let tokens = lex(&source_code).unwrap();
    if debug_mode {
        println!("Tokens: {:?}", tokens);
    }
    let ast = parse(tokens).unwrap();
    if debug_mode {
        println!("AST: {:?}", ast);
    }
    let env = &mut Environment::new();
    for node in &ast {
        let value = eval(node, env).unwrap();
        if debug_mode {
            println!("Result: {:?}", value);
        }
    }
}
