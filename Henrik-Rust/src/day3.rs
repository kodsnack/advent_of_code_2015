use std::collections::HashMap;

pub fn day3(input: String) {
    println!("santa visited: {}", send_santa(&input));
    println!("santa + robo visited: {}", send_santa_and_robo(&input));
}

fn send_santa(input: &str) -> usize {
    let mut visited: HashMap<(isize, isize), usize> = HashMap::new();

    visit(&mut input.chars(), &mut visited);

    visited.len()
}

fn send_santa_and_robo(input: &str) -> usize {
    let mut visited: HashMap<(isize, isize), usize> = HashMap::new();

    let mut route_a = input.chars().enumerate().filter_map(|(i, c)| {
        if i % 2 == 0 {
            Some(c)
        } else {
            None
        }
    });

    let mut route_b = input.chars().enumerate().filter_map(|(i, c)| {
        if (i + 1) % 2 == 0 {
            Some(c)
        } else {
            None
        }
    });

    visit(&mut route_a, &mut visited);
    visit(&mut route_b, &mut visited);

    visited.len()
}

fn visit(route: &mut Iterator<Item = char>, visited: &mut HashMap<(isize, isize), usize>) {
    let mut x: isize = 0;
    let mut y: isize = 0;

    visited.insert((x, y), 1);

    for c in route {
        match c {
            _ if c == '^' => y += 1,
            _ if c == '>' => x += 1,
            _ if c == 'v' => y -= 1,
            _ if c == '<' => x -= 1,
            _ => continue,
        }

        let mut counter = visited.entry((x, y)).or_insert(0);
        *counter += 1;
    }
}

#[test]
fn test_visit() {
    let test_string = |input: &str| {
        let mut visited: HashMap<(isize, isize), usize> = HashMap::new();

        visit(&mut input.chars(), &mut visited);

        visited.len()
    };

    assert_eq!(1, test_string(""));
    assert_eq!(2, test_string("<"));
    assert_eq!(2, test_string("<>"));

    assert_eq!(1, test_string("a"));
    assert_eq!(2, test_string("<b"));
    assert_eq!(2, test_string("<c>"));
}

#[test]
fn test_send_santa() {
    assert_eq!(2, send_santa(">"));
    assert_eq!(4, send_santa("^>v<"));
    assert_eq!(2, send_santa("^v^v^v^v^v"));
}


#[test]
fn test_send_santa_and_robo() {
    assert_eq!(3, send_santa_and_robo("^v"));
    assert_eq!(3, send_santa_and_robo("^>v<"));
    assert_eq!(11, send_santa_and_robo("^v^v^v^v^v"));
}