#[derive(Debug)]
enum Direction {
    Left,
    Right,
}

#[derive(Debug)]
struct Rotation {
    direction: Direction,
    distance: i32,
}

fn parse_line(line: &str) -> Result<Rotation, String> {
    let (dir_char, dist_str) = line.split_at(1);
    let direction = match dir_char {
        "L" => Direction::Left,
        "R" => Direction::Right,
        _ => return Err(format!("Invalid direction: {}", dir_char)),
    };
    let distance: i32 = dist_str
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

const INITIAL_VALUE: i32 = 50;

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let rotations = read_file(&filepath).expect("Failed to read file");
    let mut initial_value = INITIAL_VALUE;
    let mut count = 0;
    for rotation in rotations {
        match rotation.direction {
            Direction::Left => initial_value -= rotation.distance,
            Direction::Right => initial_value += rotation.distance,
        }
        initial_value %= 100;
        if initial_value == 0 {
            count += 1;
        }
    }
    println!("Final value: {}", count);
}
