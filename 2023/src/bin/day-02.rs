use std::error;
use std::io::{self, BufRead};
use std::str::FromStr;

fn main() -> Result<(), Box<dyn error::Error>> {
    let stdin = io::stdin();
    let handle = stdin.lock();

    let mut sum = 0i32;
    let mut power_sum = 0i32;

    let max = Sample(12, 13, 14);

    for line in handle.lines().map(|x| x.unwrap()) {
        let l = line.parse::<Line>()?;

        let mut possible = true;
        let mut min = Sample::default();

        for s in l.1 {
            if s.0 > max.0 || s.1 > max.1 || s.2 > max.2 {
                possible = false;
            }

            if s.0 > min.0 {
                min.0 = s.0;
            }

            if s.1 > min.1 {
                min.1 = s.1;
            }

            if s.2 > min.2 {
                min.2 = s.2;
            }
        }

        if possible {
            println!("game {} ok", l.0);
            sum += l.0
        }

        let power = min.0 * min.1 * min.2;
        power_sum += power;
        println!("min: {:?} power: {}", min, power);
    }

    println!("{} power: {}", sum, power_sum);

    Ok(())
}

#[derive(Debug, Default)]
struct Line(i32, Vec<Sample>);

#[derive(Debug, Default)]
struct Sample(i32, i32, i32);

impl FromStr for Line {
    type Err = Box<dyn error::Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (game, rounds) = match s.split_once(':') {
            Some(x) => x,
            None => return Err("no colon")?,
        };

        let mut out = Self::default();

        if let Some((_, game_nr)) = game.split_once(char::is_whitespace) {
            out.0 = game_nr.parse::<i32>()?;
        }

        for round in rounds.split(';') {
            out.1.push(round.parse::<Sample>()?)
        }

        Ok(out)
    }
}

impl FromStr for Sample {
    type Err = Box<dyn error::Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut out = Self::default();

        for sample in s.split(',').map(|x| x.trim()) {
            match sample.split_once(char::is_whitespace) {
                Some((n, "red")) => out.0 = n.parse::<i32>()?,
                Some((n, "green")) => out.1 = n.parse::<i32>()?,
                Some((n, "blue")) => out.2 = n.parse::<i32>()?,
                Some((_, c)) => return Err(format!("can't parse color: {}", c))?,
                None => return Err("can't parse sample")?,
            }
        }

        Ok(out)
    }
}
