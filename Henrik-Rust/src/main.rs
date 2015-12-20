use std::env;

mod day1;

fn main() {
    println!("Welcome to Advent of Rust 2015!");

    if let Some(day) = env::args().nth(1) {
        match day.as_ref() {
            "1" => day1::day1(),
            _ => println!("{} not implemented", day),
        }
    } else {
        println!("Usage:\n    aoc2015 <day> <input>")
    }
}
