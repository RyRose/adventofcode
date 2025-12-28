#[derive(Debug)]
struct Point {
    id: usize,
    x: i64,
    y: i64,
}

#[derive(Debug)]
struct Distance {
    lhs: usize,
    rhs: usize,
    value: i64,
}

impl Point {
    fn parse(id: usize, input: &str) -> Result<Self, String> {
        let res: Result<Vec<i64>, String> = input
            .split(",")
            .map(|s| s.parse())
            .map(|r| r.map_err(|e| format!("{}", e)))
            .collect();
        let values = res?;
        let [x, y] = values
            .try_into()
            .map_err(|_| format!("Invalid number of values: {}", input))?;
        Ok(Self { id, x, y })
    }

    fn distance(&self, other: &Self) -> Distance {
        let value = self.area(other);
        Distance {
            lhs: self.id,
            rhs: other.id,
            value,
        }
    }

    fn area(&self, other: &Self) -> i64 {
        (1 + self.x - other.x).abs() * (1 + self.y - other.y).abs()
    }
}

fn read_file(filepath: &str) -> Result<Vec<Point>, String> {
    let data = std::fs::read_to_string(filepath).map_err(|e| format!("{}", e))?;
    let mut points = Vec::new();
    for (i, line) in data.lines().enumerate() {
        let point = Point::parse(i, line)?;
        points.push(point);
    }
    Ok(points)
}

fn compute_distances(points: &[Point]) -> Vec<Distance> {
    let mut distances = Vec::new();
    for i in 0..points.len() {
        for j in i + 1..points.len() {
            distances.push(points[i].distance(&points[j]));
        }
    }
    distances
}

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let points = read_file(&filepath).unwrap();
    let mut distances = compute_distances(&points);
    distances.sort_by_key(|d| d.value);
    if let Some(d) = distances.last() {
        let lhs = &points[d.lhs];
        let rhs = &points[d.rhs];
        println!("Area: {}", lhs.area(rhs));
    }
}
