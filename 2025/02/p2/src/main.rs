#[derive(Debug)]
struct IdRange {
    start: i64,
    end: i64,
}

fn parse_range(component: &str) -> Result<IdRange, String> {
    let (start_str, end_str) = component
        .split_once('-')
        .ok_or_else(|| format!("Invalid range format: {}", component))?;
    let start: i64 = start_str
        .parse()
        .map_err(|e| format!("Invalid start id ({}): {}", start_str, e))?;
    let end: i64 = end_str
        .parse()
        .map_err(|e| format!("Invalid end id ({}): {}", end_str, e))?;
    Ok(IdRange { start, end })
}

fn parse_line(line: &str) -> Result<Vec<IdRange>, String> {
    let mut ranges = Vec::new();
    for component in line.split(',') {
        if component.trim().is_empty() {
            continue;
        }
        let range = parse_range(component)?;
        ranges.push(range);
    }
    Ok(ranges)
}

fn read_file(filepath: &str) -> Result<Vec<IdRange>, String> {
    let content =
        std::fs::read_to_string(filepath).map_err(|e| format!("Failed to read file: {}", e))?;
    let mut all_ranges = Vec::new();
    for line in content.lines() {
        let ranges = parse_line(line)?;
        all_ranges.extend(ranges);
    }
    Ok(all_ranges)
}

fn is_invalid(id: i64) -> bool {
    let digits = (id as f64).log10().ceil() as u32;
    for i in 1..=(digits / 2) {
        if !digits.is_multiple_of(i) {
            continue;
        }
        let magnitude = 10i64.pow(i);
        let target = id % magnitude;

        let mut cur = id;
        while (cur % magnitude) == target {
            cur /= magnitude;
        }
        if cur == 0 {
            return true;
        }
    }
    false
}

fn invalid_ids(range: &IdRange) -> Vec<i64> {
    let mut ids = Vec::new();
    for i in range.start..=range.end {
        if is_invalid(i) {
            ids.push(i);
        }
    }
    ids
}

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let ranges = read_file(&filepath).expect("Failed to read file");
    let invalid_ids: Vec<i64> = ranges.iter().flat_map(invalid_ids).collect();
    let sum: i64 = invalid_ids.iter().sum();
    println!("Sum: {}", sum);
}
