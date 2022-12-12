use std::io;

#[derive(Clone,Debug)]
struct Monkey {
    op: Op,
    items: Vec<i64>,
    divisor: i64,
    targets: [usize; 2],
    inspected: i64,
}

impl Default for Monkey {
    fn default() -> Self {
        Monkey {
            op: Op::None,
            items: Vec::new(),
            divisor: 0,
            targets: [0, 0],
            inspected: 0,
        }
    }
}

#[derive(Clone,Debug)]
enum Op {
    None,
    AddImmOp(i64),
    MultImmOp(i64),
    SquareOp,
}

impl Op {
    fn apply(&self, x: i64) -> i64 {
        match self {
            Op::None => panic!("can't apply none"),
            Op::AddImmOp(imm) => x + imm,
            Op::MultImmOp(imm) => x * imm,
            Op::SquareOp => x * x,
        }
    }
}

fn parse_input() -> io::Result<Vec<Monkey>> {
    let stdin = io::stdin();
    let mut buf = String::new();

    let mut monkies: Vec<Monkey> = Vec::new();

    while stdin.read_line(&mut buf)? > 0 {
        if buf.ends_with("\n") {
            buf.truncate(buf.len() - 1);
        }

        if buf.starts_with("Monkey") {
            monkies.push(Monkey::default())
        } else if buf.starts_with("  Starting items: ") {
            let len = monkies.len();
            let monkey = &mut monkies[len-1];

            let mut sub: &str = buf.as_str();

            while let Some(next) = sub.find(|c: char| c.is_ascii_digit()) {
                let until: usize = match sub[next..].find(',') {
                    Some(x) => next + x,
                    None => sub.len(),
                };

                monkey.items.push(sub[next..until].parse::<i64>().expect("parse item"));

                sub = &sub[until..];
            }
        } else if buf.starts_with("  Operation: new = old ") {
            let len = monkies.len();
            let monkey = &mut monkies[len-1];
            if buf.contains("* old") {
                monkey.op = Op::SquareOp;
            } else {
                let imm: i64 = buf[(buf.rfind(' ').unwrap() + 1)..].parse().expect("can't parse imm");

                if buf.contains('*') {
                    monkey.op = Op::MultImmOp(imm);
                } else if buf.contains('+') {
                    monkey.op = Op::AddImmOp(imm);
                } else {
                    return Err(
                        io::Error::new(
                            io::ErrorKind::Other,
                            "can't find op: ".to_owned() + &buf));
                }
            }
        } else if buf.starts_with("  Test: divisible by ") {
            let len = monkies.len();
            let monkey = &mut monkies[len-1];
            monkey.divisor = buf[(buf.rfind(' ').unwrap() + 1)..].parse().expect("can't parse divisor");
        } else if buf.starts_with("    If true: throw to monkey ") {
            let len = monkies.len();
            let monkey = &mut monkies[len-1];
            monkey.targets[1] = buf[(buf.rfind(' ').unwrap() + 1)..].parse().expect("can't parse true");
        } else if buf.starts_with("    If false: throw to monkey ") {
            let len = monkies.len();
            let monkey = &mut monkies[len-1];
            monkey.targets[0] = buf[(buf.rfind(' ').unwrap() + 1)..].parse().expect("can't parse false");
        } else if !buf.is_empty() {
            return Err(
                io::Error::new(
                    io::ErrorKind::Other,
                    "don't understand line: ".to_owned() + &buf));
        }

        buf.clear();
    }

    Ok(monkies)
}

fn round(monkies: &mut Vec<Monkey>, trunc: i64) {
    for i in 0..monkies.len() {
        let (before, rest) = monkies.split_at_mut(i);

        let (monkey, after) = rest.split_at_mut(1);
        let monkey = &mut monkey[0];
        while !monkey.items.is_empty() {
            monkey.inspected += 1;
            let item = monkey.op.apply(monkey.items.remove(0)) % trunc;

            let target = monkey.targets[if item % monkey.divisor == 0 { 1 } else { 0 }];

            if target < i {
                before[target].items.push(item);
            } else {
                after[target - i - 1].items.push(item);
            }
        }
    }
}

fn main() -> io::Result<()> {
    let mut monkies = parse_input()?;

    // only doing part two, sorry.
    let mut trunc = 1;

    for monkey in &monkies {
        trunc *= monkey.divisor;
    }

    for _ in 0..10000 {
        round(&mut monkies, trunc);
    }

    let mut sorted = monkies.to_owned();
    sorted.sort_by(|a, b| a.inspected.cmp(&b.inspected));
    let mut prod = sorted[sorted.len()-1].inspected;
    prod *= sorted[sorted.len()-2].inspected;

    println!("{}", prod);

    Ok(())
}
