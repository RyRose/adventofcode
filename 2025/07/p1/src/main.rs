enum Piece {
    Entrance,
    Empty,
    Splitter,
    Beam,
}

struct Grid {
    grid: Vec<Vec<Piece>>,
}

impl Grid {
    fn parse(input: &str) -> Result<Grid, String> {
        let mut grid = Vec::new();
        for (y, line) in input.lines().enumerate() {
            let mut row = Vec::new();
            for (x, ch) in line.chars().enumerate() {
                let piece = match ch {
                    '.' => Piece::Empty,
                    'S' => Piece::Entrance,
                    '^' => Piece::Splitter,
                    '|' => Piece::Beam,
                    _ => return Err(format!("Invalid character '{}' at ({}, {})", ch, x, y)),
                };
                row.push(piece);
            }
            grid.push(row);
        }
        Ok(Grid { grid })
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

    fn try_place(&mut self, i: usize, j: usize, value: Piece) -> bool {
        self.grid
            .get_mut(i)
            .and_then(|row| row.get_mut(j))
            .map_or(false, |cell| {
                if let Piece::Empty = cell {
                    *cell = value;
                    true
                } else {
                    false
                }
            })
    }

    fn solve(&mut self) -> Result<i64, String> {
        // Place initial beam below entrance.
        for (i, value) in self.grid.first().unwrap().iter().enumerate() {
            if let Piece::Entrance = value {
                self.grid[1][i] = Piece::Beam;
                break;
            }
        }

        // Propagate beams downwards.
        let mut splits = 0;
        let width = self.grid[0].len();
        let height = self.grid.len();
        for i in 1..(height - 1) {
            for j in 0..width {
                if let Piece::Beam = self.grid[i][j] {
                } else {
                    continue;
                }
                match self.grid[i + 1][j] {
                    Piece::Empty => {
                        self.try_place(i + 1, j, Piece::Beam);
                    }
                    Piece::Splitter => {
                        splits += 1;
                        self.try_place(i + 1, j - 1, Piece::Beam);
                        self.try_place(i + 1, j + 1, Piece::Beam);
                    }
                    _ => {}
                }
            }
        }
        Ok(splits)
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
