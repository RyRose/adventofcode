use bitvec::prelude::*;
use good_lp::{ProblemVariables, Solution, SolverModel, default_solver, variable};
use nalgebra::{DMatrix, DVector};
use ndarray::prelude::*;
use ndarray_linalg::Solve;
use std::collections::VecDeque;

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

    fn press(&self, target: &[u16], joltages: &mut [u16]) -> bool {
        for &idx in &self.values {
            if joltages[idx] >= target[idx] {
                return false;
            }
            joltages[idx] += 1;
        }
        true
    }
}

#[derive(Debug)]
struct Machine {
    #[allow(dead_code)]
    indicators: BitVec<u8, Lsb0>,
    buttons: Vec<Button>,
    joltages: Vec<u16>,
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
        joltages.shrink_to_fit();
        Ok(Self {
            indicators,
            buttons,
            joltages,
        })
    }

    fn lpsolve(&self) -> Result<u64, String> {
        let mut system: Array2<u16> = Array2::zeros((self.buttons.len(), self.joltages.len()));
        for (i, button) in self.buttons.iter().enumerate() {
            for &idx in &button.values {
                system[[i, idx]] = 1;
            }
        }

        // Transpose so each row corresponds to a jolt constraint
        system = system.reversed_axes();

        let mut problem = ProblemVariables::new();
        let mut variables = Vec::new();
        for _ in 0..self.buttons.len() {
            variables.push(problem.add(variable().integer().min(0)));
        }

        let mut solver = problem
            .minimise(variables.iter().sum::<good_lp::Expression>())
            .using(default_solver);
        for (i, equation) in system.outer_iter().enumerate() {
            let constraint = equation
                .iter()
                .zip(variables.iter())
                .map(|(coeff, var)| *var * *coeff)
                .sum::<good_lp::Expression>()
                .eq(self.joltages[i]);
            solver = solver.with(constraint);
        }
        let solution = solver
            .solve()
            .map_err(|e| format!("Failed to solve LP: {}", e))?;

        let presses = variables
            .iter()
            .map(|v| solution.value(*v))
            .map(|v| v.round() as u64)
            .collect::<Vec<u64>>();
        let sum = presses.iter().sum::<u64>();
        println!("Solution ({}): {:?}", sum, presses);
        Ok(sum)
    }

    // Does not limit to just integers.
    #[allow(dead_code)]
    fn algebrasolve(&self) {
        let mut a = DMatrix::zeros(self.buttons.len(), self.joltages.len());
        for (i, button) in self.buttons.iter().enumerate() {
            for &idx in &button.values {
                a[(i, idx)] = 1.0;
            }
        }
        let b = DVector::from_vec(
            self.joltages
                .clone()
                .into_iter()
                .map(|x| x as f64)
                .collect(),
        );
        a = a.transpose();

        println!("A: {}", a);
        println!("b: {}", b);
        let result = a.svd(true, true).solve(&b, 1e-8);
        println!("Result: {:?}", result);
    }

    // Does not limit to just integers.
    #[allow(dead_code)]
    fn linalgsolve(&self) {
        let mut b: Array1<f64> = Array1::zeros(self.joltages.len());
        for i in 0..self.joltages.len() {
            b[i] = self.joltages[i] as f64;
        }

        let mut a: Array2<f64> = Array2::zeros((self.buttons.len(), self.joltages.len()));
        for (i, button) in self.buttons.iter().enumerate() {
            for &idx in &button.values {
                a[[i, idx]] = 1.0;
            }
        }

        a = a.reversed_axes();

        println!("A: {}", a);
        println!("b: {}", b);
        let result = a.solve_into(b);
        println!("Result: {:?}", result);
    }

    // WAY TOO SLOW!!!
    #[allow(dead_code)]
    fn bfs(&self) -> Option<usize> {
        let mut queue = VecDeque::new();
        let mut start = self.joltages.clone();
        start.fill(0);
        queue.push_back(start);
        let mut iterations = 0;
        while !queue.is_empty() {
            let len = queue.len();
            println!("BFS iteration {}, queue size {}", iterations, len);
            for _ in 0..len {
                let cur = queue.pop_front().unwrap();
                if cur == self.joltages {
                    return Some(iterations);
                }
                for button in &self.buttons {
                    let mut next = cur.clone();
                    if button.press(&self.joltages, &mut next) {
                        queue.push_back(next);
                    }
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
        sum += machine.lpsolve().unwrap();
    }
    println!("Total button presses: {}", sum);
}
