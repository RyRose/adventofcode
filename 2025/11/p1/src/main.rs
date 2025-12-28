use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

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
    // TODO: Think about how to avoid Rc. The set of mappings is static such that we should be able
    // to guarantee references held in hashmap are valid if they are values in mappings.
    mappings: Vec<Rc<Mapping>>,
    graph: HashMap<Device, Rc<Mapping>>,
}

impl State {
    fn read(path: &str) -> Result<Self, String> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| format!("Failed to read file ({}): {}", path, e))?;
        let mut mappings = Vec::new();
        let mut graph = HashMap::new();
        for line in content.lines() {
            let mapping = Rc::new(Mapping::parse(line)?);
            graph.insert(mapping.input.clone(), mapping.clone());
            mappings.push(mapping);
        }
        Ok(Self { mappings, graph })
    }

    fn dfs<'a>(&'a self, cur: &'a str, seen: &mut HashSet<&'a str>) -> i32 {
        if cur == "out" {
            println!("Seen path to out: {:?}", seen);
            return 1;
        }
        if !seen.insert(cur) {
            return 0;
        }
        let mut ret = 0;
        for next in self.graph[cur].output.iter() {
            ret += self.dfs(next, seen);
        }
        seen.remove(cur);
        ret
    }
}

fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    let state = State::read(&filepath).unwrap();
    println!("State: {:?}", state);
    println!("DFS count: {}", state.dfs("you", &mut HashSet::new()));
}
