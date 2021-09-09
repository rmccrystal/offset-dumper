use regex::bytes::Regex;
use iced_x86::Instruction;
use iced_x86::IntelFormatter;
use iced_x86::Formatter;
use std::time::Instant;

/// Enables the user to generate a byte regex out of the normal signature format.
pub fn generate_regex(raw: &str) -> Option<Regex> {
    let mut res = raw
        .to_string()
        .split_whitespace()
        .map(|x| match &x {
            &"?" => ".".to_string(),
            &"??" => ".".to_string(),
            x => format!("\\x{}", x),
        })
        .collect::<Vec<_>>()
        .join("");
    res.insert_str(0, "(?s-u)");
    Regex::new(&res).ok()
}

pub fn find_pattern(data: &[u8], pattern: &str) -> Vec<u64> {
    let pattern = pattern.to_lowercase();
    let r = generate_regex(&pattern).expect("Could not make pattern from signature string");
    r.find_iter(data).map(|n| n.start() as u64).collect()
}

pub fn find_pattern_n(data: &[u8], pattern: &str, index: i32) -> Option<u64> {
    if index < 0 {
        find_pattern(data, pattern).into_iter().nth_back((index.abs() - 1) as usize)
    } else {
        let pattern = pattern.to_lowercase();
        let r = generate_regex(&pattern).expect("Could not make pattern from signature string");
        let x = r.find_iter(data).map(|n| n.start() as u64).nth(index as _); x
    }
}

pub fn format_instruction(i: &Instruction) -> String {
    let mut buf = String::new();
    let mut formatter = IntelFormatter::new();
    formatter.format(&i, &mut buf);
    buf
}
