#[derive(Debug, Clone, Copy)]
pub struct Sourcelocation {
    pub start: usize,
    pub end: usize
}

impl Sourcelocation {
    pub fn new( start: usize, end: usize) -> Sourcelocation {
        Sourcelocation { start, end }
    }
}