/// A circular buffer of integers, with a function to calculate their maximum.
///
/// `WindowedMax` stores a fixed set of integers, and provides a single function
/// to calculate their maximum.
pub struct WindowedMax {
    values: Vec<usize>,
    max: usize,
    current_index: usize,
}

impl WindowedMax {

    pub fn new(max_len: usize) -> Self {
        assert!(max_len > 0, "max_len must be greater than zero");
        return WindowedMax { 
            values: vec![0, max_len],
            max: 0,
            current_index: 0
        };
    }

    pub fn add(&mut self, value: usize) {
        let removed_value = self.values[self.current_index];
        self.values[self.current_index] = value;
        if removed_value > self.max {
            self.max = *self.values.iter().max_by_key(|n| *n).unwrap();
        }
        self.current_index = (self.current_index + 1) % self.values.len();
    }

    pub fn max(&self) -> usize {
        return self.max;
    }
}