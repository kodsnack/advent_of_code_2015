
use std::cmp;

pub fn day2(input: String) {

    let area: usize = input.lines()
                          .map(|dimensions| Gift::new(dimensions).total_area())
                          .sum();
    let length: usize = input.lines()
                          .map(|dimensions| Gift::new(dimensions).ribon_length())
                          .sum();
    println!("square feet of wrapping paper: {}", area);
    println!("feet of ribbon: {}", length);
}

#[derive(Debug, PartialEq)]
struct Gift {
    l: usize,
    w: usize,
    h: usize,
}

impl Gift {
    fn new(dimensions: &str) -> Gift {
        let mut iter = dimensions.split('x')
                                 .filter_map(|s| s.parse::<usize>().ok());
        Gift {
            l: iter.next().unwrap(),
            w: iter.next().unwrap(),
            h: iter.next().unwrap(),
        }
    }

    fn small_sides(&self) -> (usize, usize) {
        if cmp::max(self.l, self.w) < self.h {
            (self.l, self.w)
        } else {
            (self.h, cmp::min(self.l, self.w))
        }
    }

    fn side_area(&self) -> usize {
        (2 * self.l * self.w) + (2 * self.w * self.h) + (2 * self.h * self.l)
    }

    fn slack_area(&self) -> usize {
        let (a, b) = self.small_sides();

        a * b
    }

    fn total_area(&self) -> usize {
        self.side_area() + self.slack_area()
    }

    fn ribon_length(&self) -> usize {
        let (a, b) = self.small_sides();

        a + a + b + b + self.l * self.w * self.h
    }
}

#[test]
fn test_gift_new() {
    let left = Gift { l: 1, w: 2, h: 3 };
    let right = Gift::new("1x2x3");
    assert_eq!(left, right);
}

#[test]
fn test_gift_small_sides() {
    assert_eq!((1, 1), Gift::new("1x1x1").small_sides());
    assert_eq!((1, 2), Gift::new("1x2x3").small_sides());
    assert_eq!((5, 4), Gift::new("6x4x5").small_sides());
}

#[test]
fn test_gift_side_area() {
    assert_eq!(6, Gift::new("1x1x1").side_area());
    assert_eq!(22, Gift::new("1x2x3").side_area());
    assert_eq!(148, Gift::new("6x4x5").side_area());
}

#[test]
fn test_gift_slack_area() {
    assert_eq!(1, Gift::new("1x1x1").slack_area());
    assert_eq!(2, Gift::new("1x2x3").slack_area());
    assert_eq!(20, Gift::new("6x5x4").slack_area());
}

#[test]
fn test_gift_total_area() {
    assert_eq!(7, Gift::new("1x1x1").total_area());
    assert_eq!(24, Gift::new("1x2x3").total_area());
    assert_eq!(168, Gift::new("6x5x4").total_area());

    assert_eq!(58, Gift::new("2x3x4").total_area());
    assert_eq!(43, Gift::new("1x1x10").total_area());
}

#[test]
fn test_gift_ribon_length() {
    assert_eq!(5, Gift::new("1x1x1").ribon_length());
    assert_eq!(12, Gift::new("1x2x3").ribon_length());
    assert_eq!(138, Gift::new("6x5x4").ribon_length());

    assert_eq!(34, Gift::new("2x3x4").ribon_length());
    assert_eq!(14, Gift::new("1x1x10").ribon_length());
}