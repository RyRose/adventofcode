#[derive(Debug)]
struct IdRange {
    start: i64,
    end: i64,
}

impl IdRange {
    fn parse(component: &str) -> Result<IdRange, String> {
        let (start_str, end_str) = component
            .split_once('-')
            .ok_or_else(|| format!("Invalid range format: {}", component))?;
        let start: i64 = start_str
            .parse()
            .map_err(|e| format!("Invalid start id ({}): {}", start_str, e))?;
        let end: i64 = end_str
            .parse()
            .map_err(|e| format!("Invalid end id ({}): {}", end_str, e))?;
        Ok(IdRange { start, end })
    }
}

#[derive(Debug)]
struct State {
    ranges: Vec<IdRange>,
    ids: Vec<i64>,
}

impl State {
    fn parse(input: &str) -> Result<State, String> {
        let mut sections = input.split("\n\n");
        let ranges_section = sections
            .next()
            .ok_or_else(|| "Missing ranges section".to_string())?;
        let ids_section = sections
            .next()
            .ok_or_else(|| "Missing ids section".to_string())?;
        let ranges: Result<Vec<IdRange>, String> =
            ranges_section.lines().map(IdRange::parse).collect();
        let ids: Result<Vec<i64>, String> = ids_section
            .lines()
            .map(|line| {
                line.parse()
                    .map_err(|e| format!("Invalid id ({}): {}", line, e))
            })
            .collect();
        Ok(State {
            ranges: ranges?,
            ids: ids?,
        })
    }

    fn sort_ranges(&mut self) {
        self.ranges.sort_by_key(|r| r.start);
    }

    fn find_range(&self, id: i64) -> Option<&IdRange> {
        self.ranges.iter().find(|r| r.start <= id && id <= r.end)
    }

    fn solve(&self) -> i64 {
        let mut total = 0;
        for id in self.ids.iter() {
            if let Some(id_range) = self.find_range(*id) {
                println!("Found id: {}; id range: {:?}", id, id_range);
                total += 1;
            }
        }
        total
    }
}

fn read_file(path: &str) -> Result<State, String> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read file ({}): {}", path, e))?;
    State::parse(&content)
}

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let mut state = read_file(&filepath).unwrap();
    state.sort_ranges();
    let ingredients = state.solve();
    println!("Ingredients: {}", ingredients);
}
