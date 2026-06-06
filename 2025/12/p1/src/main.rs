// input:
//
// 0:
// ###
// ##.
// ##.
//
// 1:
// ###
// ##.
// .##
//
// 2:
// .##
// ###
// ##.
//
// 3:
// ##.
// ###
// ##.
//
// 4:
// ###
// #..
// ###
//
// 5:
// ###
// .#.
// ###
//
// 4x4: 0 0 0 0 2 0
// 12x5: 1 0 1 0 2 2
// 12x5: 1 0 1 0 3 2

use std::cell::OnceCell;

use ndarray::{Array2, array, s};

#[derive(Clone, Debug)]
struct Shape {
    grid: Array2<bool>,
    key: u8,
    rotations: OnceCell<Vec<Self>>,
    positions: OnceCell<Vec<(usize, usize)>>,
}

impl Shape {
    // Parse a shape from a string representation
    fn parse(input: &str) -> Result<Self, String> {
        let mut lines = input.lines();
        let key = if let Some(line) = lines.next() {
            line.chars().next().ok_or("Empty shape header")? as u8
        } else {
            b'X'
        };
        let mut grid = array![
            [false, false, false],
            [false, false, false],
            [false, false, false],
        ];
        for (i, line) in lines.enumerate() {
            for (j, c) in line.trim().chars().enumerate() {
                if let '#' = c {
                    grid[[i, j]] = true;
                }
            }
        }
        Ok(Self {
            grid,
            key,
            rotations: OnceCell::new(),
            positions: OnceCell::new(),
        })
    }

    // Rotate the shape 90 degrees clockwise.
    fn rotate(&mut self) {
        self.grid = self.grid.t().slice(s![.., ..;-1]).to_owned();
    }

    // Get all unique rotations of the shape.
    fn rotations(&self) -> &Vec<Self> {
        self.rotations.get_or_init(|| {
            let mut rotations = Vec::new();
            let mut current = self.clone();
            for _ in 0..4 {
                if !rotations.iter().any(|v: &Shape| v.grid == current.grid) {
                    rotations.push(current.clone());
                }
                current.rotate();
            }
            rotations
        })
    }

    // Get the positions of filled cells in the shape.
    fn positions(&self) -> &Vec<(usize, usize)> {
        self.positions.get_or_init(|| {
            let mut positions = Vec::new();
            for i in 0..3 {
                for j in 0..3 {
                    if self.grid[[i, j]] {
                        positions.push((i, j));
                    }
                }
            }
            positions
        })
    }

    fn place(&self, grid: &mut Array2<u8>, x: usize, y: usize) -> bool {
        let mut i = 0;
        let mut finished = true;
        for (dx, dy) in self.positions() {
            let gx = x + dx;
            let gy = y + dy;
            match grid.get((gx, gy)) {
                Some(b'.') => (),
                _ => {
                    finished = false;
                    break;
                }
            }
            grid[[gx, gy]] = self.key;
            i += 1;
        }
        if finished {
            return true;
        }
        // Undo placement
        for (dx, dy) in self.positions().iter().take(i) {
            grid[[x + dx, y + dy]] = b'.';
        }
        false
    }

    fn unplace(&self, grid: &mut Array2<u8>, x: usize, y: usize) {
        for (dx, dy) in self.positions() {
            grid[[x + dx, y + dy]] = b'.';
        }
    }
}

#[derive(Debug)]
struct Region {
    width: usize,
    height: usize,
    desired: Vec<u64>,
}

impl Region {
    fn parse(input: &str) -> Result<Self, String> {
        let parts: Vec<&str> = input.trim().split(':').collect();
        if parts.len() != 2 {
            return Err("Invalid region format".to_string());
        }
        let dimensions: Vec<&str> = parts[0].trim().split('x').collect();
        if dimensions.len() != 2 {
            return Err("Invalid region dimensions".to_string());
        }
        let width = dimensions[0]
            .parse()
            .map_err(|e| format!("Failed to parse width: {}", e))?;
        let height = dimensions[1]
            .parse()
            .map_err(|e| format!("Failed to parse height: {}", e))?;
        let desired = parts[1]
            .split_whitespace()
            .map(|s| {
                s.parse()
                    .map_err(|e| format!("Failed to parse shape index: {}", e))
            })
            .collect::<Result<Vec<u64>, String>>()?;
        Ok(Self {
            width,
            height,
            desired,
        })
    }

    fn _solve(&self, shapes: &[Shape], grid: &mut Array2<u8>, actual: &mut Vec<u64>) -> bool {
        if self.desired == *actual {
            print_grid(grid);
            return true;
        }
        for i in 0..shapes.len() {
            if self.desired[i] == actual[i] {
                continue;
            }
            actual[i] += 1;
            for x in 0..self.width {
                for y in 0..self.height {
                    for rotation in shapes[i].rotations() {
                        if !rotation.place(grid, x, y) {
                            continue;
                        }
                        if self._solve(shapes, grid, actual) {
                            return true;
                        }
                        rotation.unplace(grid, x, y);
                    }
                }
            }
            actual[i] -= 1;
        }

        false
    }

    fn solve(&self, shapes: &[Shape]) -> bool {
        let mut grid = Array2::<u8>::from_elem((self.width, self.height), b'.');
        let mut actual = vec![0u64; shapes.len()];
        self._solve(shapes, &mut grid, &mut actual)
    }
}

fn print_grid(grid: &Array2<u8>) {
    for j in 0..grid.shape()[1] {
        for i in 0..grid.shape()[0] {
            print!("{}", grid[[i, j]] as char);
        }
        println!();
    }
}

#[derive(Debug)]
struct State {
    shapes: Vec<Shape>,
    regions: Vec<Region>,
}

impl State {
    fn parse(input: &str) -> Result<Self, String> {
        let sections: Vec<&str> = input.split("\n\n").collect();
        let mut shapes = Vec::new();
        for section in sections.iter().take(sections.len() - 1) {
            shapes.push(Shape::parse(section)?);
        }

        let mut regions = Vec::new();
        if let Some(section) = sections.last() {
            for line in section.lines() {
                regions.push(Region::parse(line)?);
            }
        }
        Ok(Self { shapes, regions })
    }
}

fn read_file(path: &str) -> Result<State, String> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read file ({}): {}", path, e))?;
    State::parse(&content)
}

// Thoughts on solving:
//
// * Do a brute force search on every possible permutation. Every shape,
//   every position, and every rotation.

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let state = read_file(&filepath).unwrap();
    for region in state.regions.iter() {
        println!("Region:{:?}", region);
        let solved = region.solve(&state.shapes);
        println!("Solved: {}", solved);
    }
    // println!("{:?}", state);
}
