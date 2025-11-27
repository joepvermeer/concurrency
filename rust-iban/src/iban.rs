use sha1::{Sha1, Digest};
use std::sync::{Arc, Mutex, OnceLock};
use std::sync::atomic::{AtomicU32, Ordering};

// 0. m-test
//
// Perform the m-test on 'number'. Use / and % to extract digits from
// the number; do not use `to_string`, as it is too slow.
fn mtest(m: u32, number: u32) -> bool {
    todo!()
}

// 1. count mode
//
// Instead of IORef, you must use AtomicU32.
// See https://doc.rust-lang.org/std/sync/atomic/struct.AtomicU32.html
//
// You may only use `store`, `load`, `compare_exchange_weak` and
// `compare_exchange_strong` on an AtomicU32.
// The method `fetch_and_add` is thus NOT allowed.
//
// You may not use any other synchronisation primitives than AtomicU32.

pub fn count(config: Config) -> u32 {
    todo!()
}

// 2. List mode
//
// Instead of MVar, you must use Mutex.
// See https://doc.rust-lang.org/std/sync/struct.Mutex.html
//
// You can acquire a lock using `.lock().unwrap();`.
// That gives you a MutexGuard, which acts as the value in the Mutex,
// together with the information that you are holding the lock.
// You release the lock by letting the MutexGuard go out of scope.
// You can do that implicitely, or explicitely using `drop(guard);`.
//
// To output a number, call the `output` function (eg
// `output(sequence, number)`) instead of writing it directly to the console.
// This way the testsuite can check your implementation.
//
// You may not use synchronisation primitives other than Mutex.

pub fn list<O: Send + Sync + Fn(u32, u32)>(config: Config, output: &O) {
    todo!()
}

// 3. Search mode
//
// Implement Search mode using a queue, as described in the assignment
// description. To implement the queue, use the MVar queue from the lectures.
// Whereas the Haskell queue only uses MVars, in Rust we must replace an MVar
// with either a Mutex or a OnceLock. The reason is that an MVar can be used for
// different use cases (a lock or a one-slot channel), and in Rust we must
// explicitly choose how we want to use it in advance, in the type.
//
// See https://doc.rust-lang.org/std/sync/struct.OnceLock.html
// To write to a OnceLock, you can use `.set(value)`, and to read (and
// potentially block until a value is present) use `.wait()`.
//
// To help with this, we already filled in the type of `Queue` below.
//
// We also gave the types of the methods on a Queue. Note that enqueue and
// dequeue take a shared reference (&self) instead of a mutable reference (&mut
// self). Using a mutable self-reference would cause the queue to be usable from
// only one thread at a time, and we want the queue to be used concurrently.
// Hence, these methods take a shared reference (&self) and work with interior
// mutability.
//
// Having a shared reference is also sufficient for implementing these methods,
// since we use Mutex and OnceLock.

pub fn search(config: Config, hash: Hash) -> Option<u32> {
    todo!()
}

struct Queue<T: Send + Sync> {
    read_end: Mutex<Stream<T>>,
    write_end: Mutex<Stream<T>>,
}
type Stream<T> = Arc<OnceLock<Item<T>>>;

#[derive(Clone)]
struct Item<T> {
    value: T,
    next: Stream<T>
}

impl<T: Copy + Send + Sync> Queue<T> {
    fn new() -> Queue<T> {
        todo!()
    }
    fn enqueue(&self, value: T) {
        todo!()
    }
    fn dequeue(&self) -> T {
        todo!()
    }
}

#[derive(Clone, Copy)]
pub struct Config {
    pub lower: u32,
    pub upper: u32,
    pub m: u32,
    pub threads: u32
}

pub enum Mode {
    Count,
    List,
    Search(Hash)
}

pub type Hash = [u8; 20];

/// Forks the given number of threads and executes the given function in these
/// threads. The function gets a thread index as argument, between 0
/// (inclusive) and thread_count (exclusive).
/// This function returns after all threads have finished.
pub fn fork_threads<F: Fn(u32) -> () + Send + Sync>(thread_count: u32, f: F) {
    std::thread::scope(|s| {
        for idx in 0 .. thread_count {
            let f_ref = &f;
            s.spawn(move || {
                f_ref(idx)
            });
        }
    });
}

/// Checks if the given hash matches with the given string.
pub fn check_hash(expected: Hash, value: String) -> bool {
    let mut hasher = Sha1::new();
    hasher.update(value);
    hasher.finalize()[..] == expected
}
