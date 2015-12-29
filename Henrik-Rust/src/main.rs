#![feature(iter_arith)]

use std::env;
use std::io::{self, Read};

extern crate crypto;

mod day1;
mod day2;
mod day3;
mod day4;

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
        "2" => day2::day2(input),
        "3" => day3::day3(input),
        "4" => day4::day4(input),
        _ => println!("{} not implemented", day),
    }
}
