#[derive(Debug)]
struct Grid {
    lines: Vec<String>,
    operations: Vec<String>,
}

fn parse_grid(input: &str) -> Result<Grid, String> {
    let lines: Vec<String> = input.lines().map(str::to_string).collect();
    match lines.last() {
        Some(line) => {
            let mut components = Vec::new();
            let mut component = String::new();
            for c in line.chars().rev() {
                component.push(c);
                if !c.is_whitespace() {
                    components.push(component.chars().rev().collect());
                    component.clear();
                }
            }
            components.reverse();

            Ok(Grid {
                lines: lines[..lines.len() - 1].to_vec(),
                operations: components,
            })
        }
        None => Err("Input is empty".to_string()),
    }
}

fn read_file(path: &str) -> Result<Grid, String> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read file ({}): {}", path, e))?;
    parse_grid(&content)
}

fn solve(grid: &Grid) -> Result<i64, String> {
    let mut x = 0;
    let mut total = 0;
    let mut part = String::new();
    let mut subtotal: Vec<i64> = Vec::new();
    for component in grid.operations.iter() {
        for col in (x..x + component.len()).rev() {
            for line in grid.lines.iter() {
                let c = line.chars().nth(col).ok_or(format!(
                    "Column index {} out of bounds for line: {}",
                    col, line
                ))?;
                if c.is_whitespace() {
                    continue;
                }
                part.push(c);
            }
            if part.is_empty() {
                continue;
            }
            subtotal.push(
                part.parse::<i64>()
                    .map_err(|e| format!("Failed to parse number from '{}': {}", part, e))?,
            );
            part.clear();
        }
        // println!("Component: '{}', Subtotal: {:?}", component, subtotal);
        total += match component.trim() {
            "+" => subtotal.iter().sum::<i64>(),
            "*" => subtotal.iter().product::<i64>(),
            _ => return Err(format!("Unknown operation: {}", component)),
        };
        subtotal.clear();
        x += component.len();
    }
    Ok(total)
}

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let grid = read_file(&filepath).unwrap();
    let solved = solve(&grid).unwrap();
    println!("Solved result: {}", solved);
}
