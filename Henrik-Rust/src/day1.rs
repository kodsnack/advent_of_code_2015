use std::env;

pub fn day1() {
    if let Some(instructions) = env::args().nth(2) {
        println!("Floor: {}", find_floor(instructions.as_ref()));
        println!("Basement: {}", find_basement(instructions.as_ref()));
    } else {
        println!("Usage:\n    aoc2015 1 <input>.");
    }
}

fn find_floor(inst: &str) -> isize {
    inst.chars()
        .fold(0, |acc, c| {
            match c {
                '(' => acc + 1,
                ')' => acc - 1,
                _ => acc,
            }
        })
}

fn find_basement(inst: &str) -> usize {
    let mut acc: isize = 0;

    for (i, c) in inst.chars().enumerate() {
        acc = acc + match c {
            '(' => 1,
            ')' => -1,
            _ => 0,
        };

        if acc == -1 {
            return i + 1;
        }
    }

    0
}

#[test]
fn test_floor() {
    assert_eq!(find_floor("(())"), 0);
    assert_eq!(find_floor("()()"), 0);

    assert_eq!(find_floor("((("), 3);
    assert_eq!(find_floor("(()(()("), 3);
    assert_eq!(find_floor("))((((("), 3);

    assert_eq!(find_floor("())"), -1);
    assert_eq!(find_floor("))("), -1);

    assert_eq!(find_floor(")))"), -3);
    assert_eq!(find_floor(")())())"), -3);
}

#[test]
fn test_basement() {
    assert_eq!(find_basement(")"), 1);
    assert_eq!(find_basement("()())"), 5);
}