use std::collections::VecDeque;

use bitvec::prelude::*;

#[derive(Debug)]
struct Button {
    values: Vec<usize>,
}

impl Button {
    fn parse(input: &str) -> Result<Self, String> {
        let ret: Result<Vec<usize>, String> = input
            .trim()
            .trim_start_matches("(")
            .trim_end_matches(")")
            .split(",")
            .map(|s| s.parse())
            .map(|r| r.map_err(|e| format!("{}", e)))
            .collect();
        let values = ret?;
        Ok(Self { values })
    }

    fn press(&self, indicators: &mut BitVec<u8, Lsb0>) {
        for &idx in &self.values {
            let value = indicators[idx];
            indicators.set(idx, !value);
        }
    }
}

#[derive(Debug)]
struct Machine {
    indicators: BitVec<u8, Lsb0>,
    buttons: Vec<Button>,
    joltages: Vec<usize>,
}

impl Machine {
    fn parse(input: &str) -> Result<Self, String> {
        let mut words = input.split_whitespace();
        let mut indicators = bitvec![u8, Lsb0;];
        if let Some(raw_lights) = words.next() {
            for c in raw_lights.chars() {
                match c {
                    '.' => indicators.push(false),
                    '#' => indicators.push(true),
                    _ => (),
                }
            }
        }
        indicators.shrink_to_fit();
        let mut buttons = Vec::new();
        let mut joltages = Vec::new();
        for word in words {
            if let Ok(button) = Button::parse(word) {
                buttons.push(button);
                continue;
            }
            for joltage_str in word
                .trim_start_matches("{")
                .trim_end_matches("}")
                .split(",")
            {
                let joltage = joltage_str
                    .parse()
                    .map_err(|e| format!("Failed to parse joltage {}: {}", joltage_str, e))?;
                joltages.push(joltage);
            }
        }
        Ok(Self {
            indicators,
            buttons,
            joltages,
        })
    }

    fn bfs(&self) -> Option<usize> {
        let mut queue = VecDeque::new();
        let mut start = self.indicators.clone();
        start.fill(false);
        queue.push_back(start);
        let mut iterations = 0;
        while !queue.is_empty() {
            let len = queue.len();
            for _ in 0..len {
                let cur = queue.pop_front().unwrap();
                if cur == self.indicators {
                    return Some(iterations);
                }
                for button in &self.buttons {
                    let mut next = cur.clone();
                    button.press(&mut next);
                    queue.push_back(next);
                }
            }
            iterations += 1;
        }

        None
    }
}

fn read_file(filepath: &str) -> Result<Vec<Machine>, String> {
    let content = std::fs::read_to_string(filepath)
        .map_err(|e| format!("Failed to read file {}: {}", filepath, e))?;
    let mut machines = Vec::new();
    for line in content.lines() {
        let machine = Machine::parse(line)?;
        machines.push(machine);
    }
    Ok(machines)
}

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let machines = read_file(&filepath).unwrap();
    let mut sum = 0;
    for machine in &machines {
        if let Some(iterations) = machine.bfs() {
            sum += iterations;
            println!("Machine can reach target in {} iterations", iterations);
        } else {
            println!("Machine cannot reach target configuration");
        }
    }
    println!("Sum of iterations: {}", sum);
}
