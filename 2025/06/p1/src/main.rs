// Example input:
// 123 328  51 64
//  45 64  387 23
//   6 98  215 314
// *   +   *   +
//
// things are added vertically with the operand below it.

#[derive(Clone, Debug)]
enum Field {
    Number(i64),
    Add,
    Multiply,
}

impl Field {
    fn parse(component: &str) -> Result<Field, String> {
        match component.trim() {
            "+" => Ok(Field::Add),
            "*" => Ok(Field::Multiply),
            num_str => {
                let num: i64 = num_str
                    .parse()
                    .map_err(|e| format!("Invalid number ({}): {}", num_str, e))?;
                Ok(Field::Number(num))
            }
        }
    }
}

fn parse_grid(input: &str) -> Result<Vec<Vec<Field>>, String> {
    let mut grid = Vec::new();
    for line in input.lines() {
        let row: Result<Vec<Field>, String> = line.split_whitespace().map(Field::parse).collect();
        grid.push(row?);
    }
    Ok(grid)
}

fn print_grid(grid: &Vec<Vec<Field>>) {
    for row in grid {
        for field in row {
            match field {
                Field::Number(n) => print!("{:4} ", n),
                Field::Add => print!(" +  "),
                Field::Multiply => print!(" *  "),
            }
        }
        println!();
    }
}

fn invert_grid(grid: &[Vec<Field>]) -> Vec<Vec<Field>> {
    if grid.is_empty() {
        return Vec::new();
    }
    let rows = grid.len();
    let cols = grid[0].len();
    let mut inverted = vec![vec![Field::Number(0); rows]; cols];
    for r in 0..rows {
        for c in 0..cols {
            inverted[c][r] = match &grid[r][c] {
                Field::Number(n) => Field::Number(*n),
                Field::Add => Field::Add,
                Field::Multiply => Field::Multiply,
            };
        }
    }
    inverted
}

fn read_file(path: &str) -> Result<Vec<Vec<Field>>, String> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read file ({}): {}", path, e))?;
    parse_grid(&content)
}

fn solve_grid(grid: &[Vec<Field>]) -> Result<i64, String> {
    let mut total = 0;

    for row in grid {
        let mut cur = match row.last() {
            Some(Field::Multiply) => 1,
            Some(Field::Add) => 0,
            _ => return Err("Expected operator".to_string()),
        };
        for field in row.iter().take(row.len() - 1) {
            let n = match field {
                Field::Number(v) => *v,
                _ => return Err("Expected number".to_string()),
            };
            match row.last() {
                Some(Field::Multiply) => cur *= n,
                Some(Field::Add) => cur += n,
                _ => return Err("Expected operator".to_string()),
            }
        }
        total += cur;
    }

    Ok(total)
}

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let grid = read_file(&filepath).unwrap();
    print_grid(&grid);
    let grid = invert_grid(&grid);
    print_grid(&grid);
    let result = solve_grid(&grid).unwrap();
    println!("Result: {}", result);
}
