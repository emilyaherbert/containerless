/// A fixed-size window of values with a manually advancing cursor.
///
/// `FixedWindow::new(0, N)` creates a buffer with at most `N` numbers, and
/// a cursor that points to the current value. Use the `add` method to add
/// to the number at the current cursor, the `move_window` method to advance
/// the cursor, and the `sum` method to calculate the total of the numbers
/// stored in the buffer. After `N` calls to `move_window`, the data structure
/// will start to discard old values stored in the buffer.
///
/// Internally, `FixedWindow` uses a circuluar buffer. The constructor
/// `FixedWindow::new(0, N)` requires `0` as an argument to workaround
/// limitations of the Rust standard library.
use std::ops::{Add, AddAssign, SubAssign};

pub struct FixedWindow<T> {
    values: Vec<T>,
    max_len: usize,
    current_index: usize,
    sum: T,
    zero: T
}

impl<T: Copy + Add<T> + AddAssign<T> + SubAssign<T>> FixedWindow<T> {

    pub fn new(zero: T, max_len: usize) -> Self {
        let current_index = 0;
        let mut values: Vec<T> = Vec::with_capacity(max_len);
        let current_index = 0;
        let sum = zero;
        values.push(zero);
        return FixedWindow { values, max_len, current_index, sum, zero };
    }

    pub fn add(&mut self, value: T) {
        self.values[self.current_index] += value;
    }

    pub fn sum(&self) -> T {
        return self.sum;
    }

    pub fn move_window(&mut self) {
        if self.values.len() < self.max_len {
            self.values.push(self.zero);
            self.sum += self.values[self.current_index];
            self.current_index += 1;
            return;
        }
        let next_index = (self.current_index + 1) % self.max_len;
        self.sum -= self.values[next_index];
        self.sum += self.values[self.current_index];
        self.values[next_index] = self.zero;
        self.current_index = next_index;
    }



}