use std::env;
use std::io::{self, Read};

mod day1;

fn main() {
    println!("Welcome to Advent of Rust 2015!");

    let day = env::args()
                  .nth(1)
                  .expect("Failed to read day argument");

    let mut input = String::new();

    io::stdin()
        .read_to_string(&mut input)
        .ok()
        .expect("Failed to read from stdin.");

    match day.as_ref() {
        "1" => day1::day1(input),
        _ => println!("{} not implemented", day),
    }
}
