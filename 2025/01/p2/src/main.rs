#[derive(Debug)]
enum Direction {
    Left,
    Right,
}

#[derive(Debug)]
struct Rotation {
    direction: Direction,
    distance: i64,
}

fn parse_line(line: &str) -> Result<Rotation, String> {
    let (dir_char, dist_str) = line.split_at(1);
    let direction = match dir_char {
        "L" => Direction::Left,
        "R" => Direction::Right,
        _ => return Err(format!("Invalid direction: {}", dir_char)),
    };
    let distance: i64 = dist_str
        .parse()
        .map_err(|e| format!("Invalid distance: {}", e))?;
    Ok(Rotation {
        direction,
        distance,
    })
}

fn read_file(filepath: &str) -> Result<Vec<Rotation>, String> {
    let content =
        std::fs::read_to_string(filepath).map_err(|e| format!("Failed to read file: {}", e))?;
    let mut rotations = Vec::new();
    for line in content.lines() {
        let rotation = parse_line(line)?;
        rotations.push(rotation);
    }
    Ok(rotations)
}

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let rotations = read_file(&filepath).expect("Failed to read file");
    let mut cur = 50;
    let mut count = 0;
    for rotation in rotations {
        let distance = match rotation.direction {
            Direction::Left => -rotation.distance,
            Direction::Right => rotation.distance,
        };
        let ncur = (cur + distance).rem_euclid(100);
        let mut ncount = rotation.distance / 100;
        if cur != 0 && (cur + (distance % 100)) <= 0 || (cur + (distance % 100)) >= 100 {
            ncount += 1;
        }
        cur = ncur;
        count += ncount;
    }
    println!("Final value: {}", count);
}
