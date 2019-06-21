use bumpalo::*;
use std::ops::{Deref, DerefMut};
use std::fmt;

struct FatCell<T: Sized> {
    data : T,
    references: isize
}

pub struct Cell<T: Sized> {
    // Pointer to a value allocated in an arena. Client code will never own
    // a Cell, but have a value of type &'a Cell, where 'a is the lifetime
    // of the arena.
    ptr : *mut FatCell<T>
}

unsafe impl<T: Sync> Sync for Cell<T> { }

pub struct Ref<'b, T: Sized> {
    value: &'b T,
    references: *mut isize
}

pub struct RefMut<'b, T: Sized> {
    cell: &'b Cell<T>
}

impl<T: Sized> Deref for RefMut<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe {
            &(&*self.cell.ptr).data
        }
    }
}

impl<T: Sized> DerefMut for RefMut<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe {
            &mut (&mut *self.cell.ptr).data
        }
    }
}

impl<'b, T: Sized> Drop for RefMut<'b, T> {

    fn drop(&mut self) {
        unsafe {
            (&mut *self.cell.ptr).references = 0;
        }
    }
}

impl<'b, T: Sized> Drop for Ref<'b, T> {

    fn drop(&mut self) {
        unsafe {
            *(&mut *self.references) -= 1;
        }
    }
}

impl<T: Sized> Deref for Ref<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        return self.value;
    }
}

impl<T: Sized + fmt::Display> fmt::Display for Ref<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T> Cell<T> {
    pub fn new(allocator: &Bump, value: T) -> &Cell<T> {
        let fat_cell = allocator.alloc(FatCell {
            data: value,
            references: 0
        });
        let cell = allocator.alloc(Cell { ptr: fat_cell });
        return cell;
    }

    pub fn clone<'a>(&'a self, allocator: &'a Bump) -> &'a Cell<T> {
        return allocator.alloc(Cell { ptr: self.ptr });
    }

    fn get_refs(&self) -> isize {
        return unsafe { (&mut *self.ptr).references };
    }

    pub fn borrow<'a>(&'a self) -> Ref<'a, T> {
        if self.get_refs() < 0 {
            panic!("already mutably borrowed");
        }
        unsafe {
            (&mut *self.ptr).references += 1;
        }
        Ref {
            value: unsafe { &(&*self.ptr).data },
            references: unsafe { &mut (&mut *self.ptr).references }
        }
    }

    pub fn borrow_mut<'a>(&'a self) -> RefMut<'a, T> {
        if self.get_refs() != 0 {
            panic!("already borrowed");
        }
        unsafe {
            (&mut *self.ptr).references = -1;
        }
        RefMut { cell: self }
    }

}
