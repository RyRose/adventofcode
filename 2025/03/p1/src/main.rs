#[derive(Debug)]
struct Bank {
    batteries: Vec<u8>,
}

impl Bank {
    fn parse(line: &str) -> Result<Bank, String> {
        let mut batteries = Vec::new();
        for component in line.bytes() {
            let battery = component - b'0';
            batteries.push(battery);
        }
        Ok(Bank { batteries })
    }

    fn find_joltage(&self) -> u64 {
        let mut max = 0;
        let mut lhs = 0;
        for i in 0..(self.batteries.len() - 1) {
            if self.batteries[i] > max {
                max = self.batteries[i];
                lhs = i;
            }
        }
        let mut rhs = 0;
        max = 0;
        for i in (lhs + 1)..self.batteries.len() {
            if self.batteries[i] > max {
                max = self.batteries[i];
                rhs = i;
            }
        }

        ((self.batteries[lhs] * 10) + self.batteries[rhs]) as u64
    }
}

fn read_file(filepath: &str) -> Result<Vec<Bank>, String> {
    let content =
        std::fs::read_to_string(filepath).map_err(|e| format!("Failed to read file: {}", e))?;
    let mut banks = Vec::new();
    for line in content.lines() {
        let bank = Bank::parse(line)?;
        banks.push(bank);
    }
    Ok(banks)
}

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let banks = read_file(&filepath).expect("Failed to read file");
    let joltages: Vec<u64> = banks.iter().map(|b| b.find_joltage()).collect();
    let sum: u64 = joltages.iter().sum();
    println!("Sum: {}", sum);
}
