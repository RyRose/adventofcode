enum Piece {
    Entrance,
    Empty,
    Splitter,
    Beam,
}

struct Grid {
    grid: Vec<Vec<Piece>>,
    counts: Vec<Vec<Option<i64>>>,
}

impl Grid {
    fn parse(input: &str) -> Result<Grid, String> {
        let mut grid = Vec::new();
        let mut counts = Vec::new();
        for (y, line) in input.lines().enumerate() {
            let mut row = Vec::new();
            let mut count = Vec::new();
            for (x, ch) in line.chars().enumerate() {
                let piece = match ch {
                    '.' => Piece::Empty,
                    'S' => Piece::Entrance,
                    '^' => Piece::Splitter,
                    '|' => Piece::Beam,
                    _ => return Err(format!("Invalid character '{}' at ({}, {})", ch, x, y)),
                };
                row.push(piece);
                count.push(None);
            }
            counts.push(count);
            grid.push(row);
        }
        Ok(Grid { grid, counts })
    }

    fn print(&self) {
        for row in &self.grid {
            for piece in row {
                let ch = match piece {
                    Piece::Empty => '.',
                    Piece::Entrance => 'S',
                    Piece::Splitter => '^',
                    Piece::Beam => '|',
                };
                print!("{}", ch);
            }
            println!();
        }
    }

    fn recsolve(&mut self, i: usize, j: usize) -> Result<i64, String> {
        if let Some(Piece::Beam) = self.grid.get(i).and_then(|row| row.get(j)) {
        } else {
            return Ok(0);
        }
        if i >= self.grid.len() - 1 {
            return Ok(1);
        }
        if let Some(count) = self.counts[i][j] {
            return Ok(count);
        }

        let ret = match self.grid[i + 1][j] {
            Piece::Empty => {
                self.grid[i + 1][j] = Piece::Beam;
                let ret = self.recsolve(i + 1, j);
                self.grid[i + 1][j] = Piece::Empty;
                ret
            }
            Piece::Splitter => {
                let left = if j > 0 {
                    self.grid[i + 1][j - 1] = Piece::Beam;
                    let ret = self.recsolve(i + 1, j - 1)?;
                    self.grid[i + 1][j - 1] = Piece::Empty;
                    ret
                } else {
                    0
                };
                let right = if j < self.grid[0].len() - 1 {
                    self.grid[i + 1][j + 1] = Piece::Beam;
                    let ret = self.recsolve(i + 1, j + 1)?;
                    self.grid[i + 1][j + 1] = Piece::Empty;
                    ret
                } else {
                    0
                };
                Ok(left + right)
            }
            _ => {
                self.print();
                Err(format!("Beam blocked at ({}, {})", i, j + 1))
            }
        };
        let val = ret?;
        self.counts[i][j] = Some(val);
        Ok(val)
    }

    fn solve(&mut self) -> Result<i64, String> {
        // Place initial beam below entrance and solve recursively.
        for (i, value) in self.grid.first().unwrap().iter().enumerate() {
            if let Piece::Entrance = value {
                self.grid[1][i] = Piece::Beam;
                return self.recsolve(1, i);
            }
        }
        Err("Entrance not found".to_string())
    }
}

fn read_file(filepath: &str) -> Result<Grid, String> {
    let input = std::fs::read_to_string(filepath).map_err(|e| e.to_string())?;
    Grid::parse(&input)
}

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let mut grid = read_file(&filepath).unwrap();
    println!("Initial grid:");
    grid.print();
    let value = grid.solve().unwrap();
    println!("\nFinal grid:");
    grid.print();
    println!("\nResult: {:?}", value);
}
