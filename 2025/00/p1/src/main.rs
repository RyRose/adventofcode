fn main() {
    let filepath = std::env::args().nth(1).unwrap();
    println!("Hello, {}!", filepath);
}
