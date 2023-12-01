use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let words: Vec<&[u8]> = vec![
        "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ]
    .into_iter()
    .map(|x| x.as_bytes())
    .collect();

    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut handle = stdin.lock();

    let mut sum = 0i32;

    loop {
        if handle.read_line(&mut buffer)? == 0 {
            break;
        }

        let bs = buffer.as_bytes();

        'potato: for i in 0..bs.len() {
            let c = bs[i];
            if c >= b'0' && c <= b'9' {
                sum += 10 * (c - b'0') as i32;
                break;
            }

            for j in 0..words.len() {
                if bs[i..].starts_with(words[j]) {
                    sum += 10 * (j as i32);
                    break 'potato;
                }
            }
        }

        'potato: for i in (0..bs.len()).rev() {
            let c = bs[i];
            if c >= b'0' && c <= b'9' {
                sum += (c - b'0') as i32;
                break;
            }

            for j in 0..words.len() {
                if bs[i..].starts_with(words[j]) {
                    sum += j as i32;
                    break 'potato;
                }
            }
        }

        buffer.clear();
    }

    println!("{}", sum);

    Ok(())
}
