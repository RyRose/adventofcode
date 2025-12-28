use bitvec::prelude::*;

#[derive(Clone, Debug)]
struct Point {
    x: i64,
    y: i64,
}

impl Point {
    fn parse(input: &str) -> Result<Self, String> {
        let res: Result<Vec<i64>, String> = input
            .split(",")
            .map(|s| s.parse())
            .map(|r| r.map_err(|e| format!("{}", e)))
            .collect();
        let values = res?;
        let [x, y] = values
            .try_into()
            .map_err(|_| format!("Invalid number of values: {}", input))?;
        Ok(Self { x, y })
    }

    fn area(&self, other: &Self) -> i64 {
        (1 + (self.x - other.x).abs()) * (1 + (self.y - other.y).abs())
    }
}

#[derive(Debug)]
struct Grid {
    points: Vec<Point>,
    board: BitVec,
    width: i64,
    height: i64,
}

impl Grid {
    fn new(points: Vec<Point>) -> Self {
        let (width, height) = compute_dimensions(&points);
        let board = bitvec![0; (width * height) as usize];
        Self {
            points,
            board,
            width,
            height,
        }
    }

    fn get(&self, x: i64, y: i64) -> bool {
        let idx = (y * self.width + x) as usize;
        self.board[idx]
    }

    fn set(&mut self, x: i64, y: i64, value: bool) {
        let idx = (y * self.width + x) as usize;
        self.board.set(idx, value);
    }

    fn mark_is_inside_it(&mut self, x: i64, y: i64) {
        let mut outside = bitvec![0; (self.width * self.height) as usize];
        let mut stack = Vec::new();
        stack.push((x, y));
        while let Some((x, y)) = stack.pop() {
            if x < 0 || x >= self.width || y < 0 || y >= self.height {
                continue;
            }
            let idx = (y * self.width + x) as usize;

            if stack.len().is_multiple_of(10000000) {
                println!("Visiting cell ({:6}, {:6}); stack: {}", x, y, stack.len());
            }

            // Hit a wall/filled cell -> stop expansion
            if self.get(x, y) {
                continue;
            }
            if outside[idx] {
                continue;
            }
            outside.set(idx, true);

            // Explore neighbors
            stack.push((x + 1, y));
            stack.push((x - 1, y));
            stack.push((x, y + 1));
            stack.push((x, y - 1));
        }

        println!("Marked outside area.");
        for y in 0..self.height {
            for x in 0..self.width {
                let idx = (y * self.width + x) as usize;
                if !outside[idx] {
                    self.set(x, y, true);
                }
            }
        }
        println!("Marked inside area.");
    }

    fn place_points(&mut self) {
        let n = self.points.len();
        for i in 0..n {
            let p1 = &self.points[i];
            let p2 = &self.points[(i + 1) % n];
            let min_x = p1.x.min(p2.x);
            let max_x = p1.x.max(p2.x);
            let min_y = p1.y.min(p2.y);
            let max_y = p1.y.max(p2.y);
            for y in min_y..=max_y {
                for x in min_x..=max_x {
                    self.set(x, y, true);
                }
            }
        }
        self.mark_is_inside_it(0, 0);
    }

    fn parse(filepath: &str) -> Result<Self, String> {
        let data = std::fs::read_to_string(filepath).map_err(|e| format!("{}", e))?;
        let mut points = Vec::new();
        for line in data.lines() {
            let point = Point::parse(line)?;
            points.push(point);
        }
        Ok(Self::new(points))
    }

    fn compute_maximum_rectangle(&self) -> i64 {
        let mut max_area = 0;
        for i in 0..self.points.len() {
            println!("Checking point {}/{}", i + 1, self.points.len());
            for j in (i + 1)..self.points.len() {
                let p1 = &self.points[i];
                let p2 = &self.points[j];
                let mut valid_rectangle = true;

                // Check horizontal edges
                for x in p1.x.min(p2.x)..=p1.x.max(p2.x) {
                    if self.get(x, p1.y) && self.get(x, p2.y) {
                        continue;
                    }
                    valid_rectangle = false;
                    break;
                }

                // Check vertical edges
                for y in p1.y.min(p2.y)..=p1.y.max(p2.y) {
                    if self.get(p1.x, y) && self.get(p2.x, y) {
                        continue;
                    }
                    valid_rectangle = false;
                    break;
                }

                // Update max area if valid
                if !valid_rectangle {
                    continue;
                }
                max_area = max_area.max(p1.area(p2));
            }
        }
        max_area
    }
}

fn compute_dimensions(points: &[Point]) -> (i64, i64) {
    let mut max_x = points[0].x;
    let mut max_y = points[0].y;
    for point in points {
        if point.x > max_x {
            max_x = point.x;
        }
        if point.y > max_y {
            max_y = point.y;
        }
    }
    (max_x + 3, max_y + 2)
}

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let mut grid = Grid::parse(&filepath).unwrap();
    grid.place_points();
    let max_area = grid.compute_maximum_rectangle();
    println!("Maximum rectangle area: {}", max_area);
}
