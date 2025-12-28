use std::collections::{HashMap, HashSet};

type Device = String;

#[derive(Clone, Debug)]
struct Mapping {
    input: Device,
    output: Vec<Device>,
}

impl Mapping {
    fn parse(data: &str) -> Result<Mapping, String> {
        let (input, raw_outputs) = data
            .split_once(":")
            .map(|(a, b)| (a.to_string(), b))
            .ok_or_else(|| format!("Failed to split data on `:`: {}", data))?;
        let output = raw_outputs
            .split_whitespace()
            .map(|v| v.to_string())
            .collect();
        Ok(Self { input, output })
    }
}

#[derive(Debug)]
struct State {
    mappings: Vec<Mapping>,
    graph: HashMap<Device, Mapping>,
    rgraph: HashMap<Device, Vec<Device>>,
}

impl State {
    fn read(path: &str) -> Result<Self, String> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| format!("Failed to read file ({}): {}", path, e))?;
        let mut mappings = Vec::new();
        let mut graph = HashMap::new();
        let mut rgraph = HashMap::new();
        for line in content.lines() {
            let mapping = Mapping::parse(line)?;
            graph.insert(mapping.input.clone(), mapping.clone());
            for output in &mapping.output {
                rgraph
                    .entry(output.clone())
                    .or_insert_with(Vec::new)
                    .push(mapping.input.clone());
            }
            mappings.push(mapping);
        }
        Ok(Self {
            mappings,
            graph,
            rgraph,
        })
    }

    fn solve(&self) -> i64 {
        let first = [
            self.dfs("svr", "fft"),
            self.dfs("fft", "dac"),
            self.dfs("dac", "out"),
        ];
        println!("First path counts: {:?}", first);
        let second = [
            self.dfs("svr", "dac"),
            self.dfs("dac", "fft"), // NOTE: this is zero in the main dataset.
            self.dfs("fft", "out"),
        ];
        println!("Second path counts: {:?}", second);
        let products = [first, second].map(|arr| arr.iter().product::<i64>());
        println!("Products: {:?}", products);
        products.iter().sum::<i64>()
    }

    fn dfs(&self, start: &str, target: &str) -> i64 {
        self._dfs(start, target, &mut HashMap::new())
    }

    fn _dfs<'a>(&'a self, cur: &'a str, target: &'a str, seen: &mut HashMap<&'a str, i64>) -> i64 {
        if cur == target {
            return 1;
        }
        if let Some(v) = seen.get(cur) {
            return *v;
        }
        let mut ret = 0;
        if let Some(mapping) = self.graph.get(cur) {
            for next in mapping.output.iter() {
                ret += self._dfs(next, target, seen);
            }
        }
        seen.insert(cur, ret);
        ret
    }

    // TOO SLOW!!
    fn _dfs_slow<'a>(&'a self, cur: &'a str, seen: &mut HashSet<&'a str>) -> i32 {
        if cur == "out" {
            if seen.contains("dac") && seen.contains("fft") {
                println!("Seen path to out: {:?}", seen);
                return 1;
            }
            return 0;
        }
        if !seen.insert(cur) {
            return 0;
        }
        let mut ret = 0;
        if let Some(mapping) = self.graph.get(cur) {
            for next in mapping.output.iter() {
                ret += self.dfs_slow(next, seen);
            }
        }
        seen.remove(cur);
        ret
    }
}

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let state = State::read(&filepath).unwrap();
    println!("DFS count: {}", state.solve());
}
