use crypto::md5::Md5;
use crypto::digest::Digest;

pub fn day4(input: String) {
    let prefix = &input;
    println!("prefix: {}", prefix);

    find_print(prefix, "00000");
    find_print(prefix, "000000");
}

fn find_print(prefix: &str, output: &str) {
    println!("looking for a suffix that generates a hash that starts with {}",
             output);
    let suffix = find_suffix(prefix, output);
    if let Some(suffix) = suffix {
        println!("found: {}", suffix);
        println!("{}{} -> {}", prefix, suffix, generate_hash(prefix, &suffix));
    } else {
        println!("no match found!");
    }
}

fn find_suffix(prefix: &str, output: &str) -> Option<String> {
    let mut md5 = Md5::new();

    (0..10000000)
        .map(|suffix: u32| suffix.to_string())
        .filter(|suffix| {
            md5.reset();
            md5.input_str(prefix);
            md5.input_str(suffix);
            md5.result_str().starts_with(output)
        })
        .next()
}

fn generate_hash(prefix: &str, suffix: &str) -> String {
    let mut md5 = Md5::new();
    md5.input_str(prefix);
    md5.input_str(suffix);

    md5.result_str()
}

#[test]
fn test_find_suffix() {
    let run_test = |prefix: &str, output: &str| {
        let suffix = find_suffix(prefix, output).unwrap();
        assert!(generate_hash(prefix, &suffix).starts_with(output));
    };

    run_test("abc", "0");
    run_test("abc", "00");
}
