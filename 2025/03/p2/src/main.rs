fn find_max(data: &[u8], start: usize, end: usize) -> (u8, usize) {
    let mut max = 0;
    let mut index = start;
    for (i, item) in data.iter().enumerate().take(end).skip(start) {
        if *item > max {
            max = *item;
            index = i;
        }
    }
    (max, index)
}

fn convert_to_u64(data: &[u8]) -> u64 {
    let mut result = 0u64;
    for &digit in data {
        result = result * 10 + digit as u64;
    }
    result
}

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
        let mut cur = 0;
        let mut batteries = Vec::new();
        for i in 0..12 {
            let (m, index) = find_max(&self.batteries, cur, self.batteries.len() - (11 - i));
            cur = index + 1;
            batteries.push(m);
        }
        convert_to_u64(&batteries)
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
