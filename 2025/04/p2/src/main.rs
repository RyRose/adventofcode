#[derive(Debug, PartialEq)]
enum Cell {
    Empty,
    PaperRoll,
    MarkedPaperRoll,
}

fn print_grid(grid: &Vec<Vec<Cell>>) {
    for row in grid {
        for cell in row {
            let ch = match cell {
                Cell::Empty => '.',
                Cell::PaperRoll => '@',
                Cell::MarkedPaperRoll => 'x',
            };
            print!("{}", ch);
        }
        println!();
    }
}

fn parse_grid(input: &str) -> Vec<Vec<Cell>> {
    let mut grid = Vec::new();
    for line in input.lines() {
        let mut row = Vec::new();
        for ch in line.chars() {
            let cell = match ch {
                '.' => Cell::Empty,
                '@' => Cell::PaperRoll,
                'x' => Cell::MarkedPaperRoll,
                _ => continue,
            };
            row.push(cell);
        }
        grid.push(row);
    }
    grid
}

fn read_file(filepath: &str) -> Result<Vec<Vec<Cell>>, String> {
    let content =
        std::fs::read_to_string(filepath).map_err(|e| format!("Failed to read file: {}", e))?;
    let grid = parse_grid(&content);
    Ok(grid)
}

fn count_paper_rolls_in_adjacent_spaces(grid: &[Vec<Cell>], x: usize, y: usize) -> usize {
    if grid[x][y] != Cell::PaperRoll {
        return 8;
    }
    let directions = [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ];
    let mut count = 0;
    for (dx, dy) in directions.iter() {
        let nx = x as isize + dx;
        let ny = y as isize + dy;
        if !(nx >= 0 && ny >= 0 && (nx as usize) < grid.len() && (ny as usize) < grid[0].len()) {
            continue;
        }
        match grid[nx as usize][ny as usize] {
            Cell::PaperRoll | Cell::MarkedPaperRoll => count += 1,
            _ => (),
        }
    }
    count
}

fn clear_marked_paper_rolls(grid: &mut [Vec<Cell>]) {
    for x in 0..grid.len() {
        for y in 0..grid[0].len() {
            if grid[x][y] == Cell::MarkedPaperRoll {
                grid[x][y] = Cell::Empty;
            }
        }
    }
}

fn count_paper_rolls(grid: &mut [Vec<Cell>]) -> usize {
    let mut total = 0;
    loop {
        let mut cur = 0;
        for x in 0..grid.len() {
            for y in 0..grid[0].len() {
                if count_paper_rolls_in_adjacent_spaces(grid, x, y) < 4 {
                    grid[x][y] = Cell::MarkedPaperRoll;
                    cur += 1
                }
            }
        }
        if cur == 0 {
            break;
        }
        clear_marked_paper_rolls(grid);
        total += cur;
    }
    total
}

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let mut grid = read_file(&filepath).expect("Failed to read file");
    println!("Initial grid:");
    print_grid(&grid);

    let count = count_paper_rolls(&mut grid);
    println!("\nFinal grid:");
    print_grid(&grid);
    println!("\nCount: {}", count);
}
