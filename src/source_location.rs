#[derive(Debug, Clone, Copy)]
pub struct SourceLocation {
    pub start: usize,
    pub end: usize
}

impl SourceLocation {
    pub fn new( start: usize, end: usize) -> SourceLocation {
        SourceLocation { start, end }
    }
}

pub trait SourcePosition {
    fn source_location(&self) -> SourceLocation;
}
