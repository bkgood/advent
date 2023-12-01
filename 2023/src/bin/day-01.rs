use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let words = [
        "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];

    let mut buf = String::new();
    let stdin = io::stdin();
    let mut handle = stdin.lock();

    let mut sum = 0i32;

    loop {
        if handle.read_line(&mut buf)? == 0 {
            break;
        }

        let bs = buf.as_bytes();

        'first: for i in 0..bs.len() {
            if let Some(c) = digit(bs[i]) {
                sum += 10 * c;
                break;
            }

            for (j, word) in words.iter().enumerate() {
                if bs[i..].starts_with(word.as_bytes()) {
                    sum += 10 * (j as i32);
                    break 'first;
                }
            }
        }

        'last: for i in (0..bs.len()).rev() {
            if let Some(c) = digit(bs[i]) {
                sum += c;
                break;
            }

            for (j, word) in words.iter().enumerate() {
                if bs[i..].starts_with(word.as_bytes()) {
                    sum += j as i32;
                    break 'last;
                }
            }
        }

        buf.clear();
    }

    println!("{}", sum);

    Ok(())
}

fn digit(b: u8) -> Option<i32> {
    if b.is_ascii_digit() {
        Some((b - b'0') as i32)
    } else {
        None
    }
}
