#[derive(Debug)]
struct JunctionBox {
    id: usize,
    x: i64,
    y: i64,
    z: i64,
}

#[derive(Debug)]
struct Distance {
    lhs: usize,
    rhs: usize,
    value: i64,
}

impl JunctionBox {
    fn parse(id: usize, line: &str) -> Result<Self, String> {
        let res: Result<Vec<i64>, String> = line
            .split(",")
            .map(|s| s.parse())
            .map(|r| r.map_err(|e| format!("{}", e)))
            .collect();
        let values = res?;
        if values.len() != 3 {
            return Err(format!("Invalid number of values: {}", line));
        }
        Ok(Self {
            id,
            x: values[0],
            y: values[1],
            z: values[2],
        })
    }

    fn distance(&self, other: &Self) -> Distance {
        let value =
            (self.x - other.x).pow(2) + (self.y - other.y).pow(2) + (self.z - other.z).pow(2);
        Distance {
            lhs: self.id,
            rhs: other.id,
            value,
        }
    }
}

fn read_file(filepath: &str) -> Result<Vec<JunctionBox>, String> {
    let data = std::fs::read_to_string(filepath).map_err(|e| format!("{}", e))?;
    let mut junctions = Vec::new();
    for (i, line) in data.lines().enumerate() {
        let junction = JunctionBox::parse(i, line)?;
        junctions.push(junction);
    }
    Ok(junctions)
}

fn compute_distances(junctions: &[JunctionBox]) -> Vec<Distance> {
    let mut distances = Vec::new();
    for i in 0..junctions.len() {
        for j in i + 1..junctions.len() {
            distances.push(junctions[i].distance(&junctions[j]));
        }
    }
    distances
}

/// A classic Union‑Find (Disjoint‑Set Union) with path compression and
/// union‑by‑rank.
#[derive(Debug)]
struct DisjointSet {
    parent: Vec<usize>,
    rank: Vec<usize>,
}

impl DisjointSet {
    /// Create an empty structure.
    pub fn new(n: usize) -> Self {
        Self {
            parent: (0..n).collect(),
            rank: (0..n).map(|_| 1).collect(),
        }
    }

    /// Find the root of `x` with path compression.
    pub fn find(&mut self, x: usize) -> usize {
        if self.parent[x] != x {
            // Path compression: make the parent of x point directly to the root.
            self.parent[x] = self.find(self.parent[x]);
        }
        self.parent[x]
    }

    /// Union the sets that contain `x` and `y`. Returns true iff the
    /// sets were different and we merged them.
    pub fn union(&mut self, x: usize, y: usize) -> bool {
        let mut rx = self.find(x);
        let mut ry = self.find(y);

        if rx == ry {
            return false; // already in the same set
        }

        // Union‑by‑rank: attach the shorter tree to the taller one.
        if self.rank[rx] < self.rank[ry] {
            std::mem::swap(&mut rx, &mut ry);
        }
        self.parent[ry] = rx;
        self.rank[rx] += self.rank[ry];
        // Reset rank to zero to ensure only roots have ranks.
        self.rank[ry] = 0;
        true
    }
}

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let junctions = read_file(&filepath).unwrap();
    let mut distances = compute_distances(&junctions);
    distances.sort_by(|a, b| a.value.cmp(&b.value));

    let mut disjoint_set = DisjointSet::new(junctions.len());
    let mut last_distance_value = None;
    for distance in distances {
        if !disjoint_set.union(distance.lhs, distance.rhs) {
            continue;
        }
        if disjoint_set.rank[distance.lhs] == junctions.len()
            || disjoint_set.rank[distance.rhs] == junctions.len()
        {
            break;
        }
        last_distance_value = Some(distance);
    }
    let last = last_distance_value.unwrap();
    println!(
        "lhs junction: {:?}\nrhs junction: {:?}\nmultiplied: {}",
        junctions[last.lhs],
        junctions[last.rhs],
        junctions[last.lhs].x * junctions[last.rhs].x
    );
}
