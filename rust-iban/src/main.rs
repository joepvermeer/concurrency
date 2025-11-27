mod iban;
use iban::*;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 6 {
        println!("Not enough arguments");
        return;
    }

    let lower: u32 = args[1].parse().expect("Could not parse lower bound");
    let upper: u32 = args[2].parse().expect("Could not parse upper bound");
    let m: u32 = args[3].parse().expect("Could not parse parameter m");
    let threads: u32 = args[4].parse().expect("Could not parse number of threads");
    let mode = match args[5].as_str() {
        "count" => Mode::Count,
        "list" => Mode::List,
        "search" => {
            if args.len() < 7 {
                println!("Missing hash argument");
                return;
            }
            let hash = parse_hash(&args[6]);

            Mode::Search(hash)
        }
        _ => panic!("Expected mode count, list or search")
    };

    let config = Config{ lower, upper, m, threads };

    match mode {
        Mode::Count => println!("{}", count(config)),
        Mode::List => list(config, &|idx, number| println!("{} {}", idx, number)),
        Mode::Search(hash) => {
            match search(config, hash) {
                None => println!("not found"),
                Some(x) => println!("{}", x)
            }
        }
    }
}

fn parse_hash(string: &str) -> Hash {
    assert_eq!(string.len(), 40);
    let half_bytes: Vec<u8> = string.chars().map(|c| u8::from_str_radix(&c.to_string(), 16).unwrap()).collect();
    std::array::from_fn(|i| half_bytes[2 * i] * 16 + half_bytes[2 * i + 1])
}

#[cfg(test)]
mod test {
    use std::sync::atomic::{AtomicU32, Ordering};

    use super::*;

    #[test]
    fn test_count() {
        let cases = [
            (1, 139483, 928234, 11, 1, 71705),
            (1, 2824374, 25823728, 24, 1, 958317),
            (5, 2824374, 25823728, 24, 2, 958317),
            (5, 2824374, 25823728, 24, 4, 958317),
            (5, 2824374, 25823728, 24, 16, 958317),
            (5, 2824374, 25823728, 24, 7, 958317),
            (1, 0, 10, 1, 3, 10),
            (1, 0, 10, 1, 17, 10)
        ];
        for (repeats, lower, upper, m, threads, expected) in cases {
            for _ in 0 .. repeats {
                let result = count(Config { lower, upper, m, threads });
                if result != expected {
                    panic!("Test failed:\ncount {} {} {} {}\nExpected: {}\nObserved: {}", lower, upper, m, threads, expected, result);
                }
            }
        }
    }

    #[test]
    fn test_list() {
        let expected1 = [
            123456804, 123456812, 123456820, 123456889, 123456897,
            123456901, 123456978, 123456986, 123456994, 123457003,
            123457011, 123457088, 123457096, 123457100, 123457169,
            123457177, 123457185, 123457193, 123457258, 123457266,
            123457274, 123457282, 123457290, 123457339, 123457347,
            123457355, 123457363, 123457371, 123457428, 123457436,
            123457444, 123457452, 123457460, 123457509, 123457517,
            123457525, 123457533, 123457541, 123457606, 123457614,
            123457622, 123457630, 123457699, 123457703, 123457711,
            123457788
        ];
        let expected2 = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let expected3: Box<[u32]> = (123456789..=123466788).collect();
        let cases: [(u32, u32, u32, u32, u32, &[u32]); 7] = [
            (1, 123456789, 123457789, 21, 1, &expected1),
            (5, 123456789, 123457789, 21, 2, &expected1),
            (5, 123456789, 123457789, 21, 3, &expected1),
            (5, 123456789, 123457789, 21, 8, &expected1),
            (5, 123456789, 123466789, 1, 8, &expected3),
            (1, 0, 10, 1, 3, &expected2),
            (1, 0, 10, 1, 17, &expected2)
        ];
        println!("");
        for (repeats, lower, upper, m, threads, expected) in cases {
            println!("     list {} {} {} {}", lower, upper, m, threads);
            for _ in 0 .. repeats {
                let output_index = AtomicU32::new(0);
                let outputs: Box<[AtomicU32]> = (0 .. expected.len()).map(|_| AtomicU32::new(0) ).collect();
                list(
                    Config{ lower, upper, m, threads },
                    &|counter, number| {
                        let index = output_index.fetch_add(1, Ordering::Relaxed) as usize;
                        if index >= expected.len() {
                            panic!("List mode generated more outputs than expected");
                        }
                        if counter as usize != index + 1 {
                            panic!("Wrong sequence number");
                        }
                        outputs[index].store(number, Ordering::Relaxed);
                    }
                );

                if output_index.load(Ordering::Relaxed) as usize != expected.len() {
                    panic!("List mode generated fewer outputs than expected");
                }

                let mut outputs_sorted: Box<[u32]> = outputs.iter().map(|x| x.load(Ordering::Relaxed)).collect();
                outputs_sorted.sort();

                assert_eq!(&outputs_sorted as &[u32], expected);
            }
        }
    }

    #[test]
    fn test_search() {
        let cases = [
            (1, 274856170, 274856190, 11, 4, "ba9cd915c8e359d9733edcfe9c61e5aca92afb00", None),
            (1, 274856170, 274856190, 12, 4, "c736ca9048d0967a27ec3833832f7ffb571ebd2f", None),
            (1, 274856170, 274856190, 11, 4, "c736ca9048d0967a27ec3833832f7ffb571ebd2f", Some(274856182)),
            (1, 261756170, 274896190, 47, 4, "a8428d9c87323e977978a67ee48827ca154bd84b", Some(274886190)),
            (1, 261756170, 262035744, 1, 1, "356a192b7913b04c54574d18c28d46e6395428ab", None),
            (5, 261756170, 262035744, 1, 2, "356a192b7913b04c54574d18c28d46e6395428ab", None),
            (5, 261756170, 262035744, 1, 4, "356a192b7913b04c54574d18c28d46e6395428ab", None),
        ];
        for (repeats, lower, upper, m, threads, hash, expected) in cases {
            for _ in 0 .. repeats {
                let result = search(Config{ lower, upper, m, threads }, parse_hash(&hash));
                if result != expected {
                    panic!("Test failed:\nsearch {} {} {} {} {}\nExpected: {:?}\nObserved: {:?}", lower, upper, m, threads, hash, expected, result);
                }
            }
        }
    }

    // Tests are executed alphabetically, so to run the benchmarks last, we
    // prefix them with a 'z'.
    #[test]
    fn z_bench_count() {
        let run = |threads: u32| {
            count(Config{ lower: 2824374, upper: 25823728, m: 24, threads });
        };
        println!("");
        let n1 = bench("N1", 5, || { run(1); });
        let n2 = bench("N2", 5, || { run(2); });
        let n4 = bench("N4", 5, || { run(4); });
        let n8 = bench("N8", 5, || { run(8); });
        assert!(n1 * 0.4 <= n2 && n2 <= n1 * 0.8); 
        assert!(n1 * 0.2 <= n4 && n4 <= n1 * 0.6); 
        assert!(n1 * 0.09 <= n8 && n8 <= n1 * 0.5); 
    }

    #[test]
    fn z_bench_search() {
        let hash = parse_hash("356a192b7913b04c54574d18c28d46e6395428ab");
        let run = |threads: u32| {
            search(Config{ lower: 261756170, upper: 264551910, m: 1, threads }, hash);
        };
        println!("");
        let n1 = bench("N1", 5, || { run(1); });
        let n2 = bench("N2", 5, || { run(2); });
        let n4 = bench("N4", 5, || { run(4); });
        let n8 = bench("N8", 5, || { run(8); });
        assert!(n1 * 0.4 <= n2 && n2 <= n1 * 0.8); 
        assert!(n1 * 0.2 <= n4 && n4 <= n1 * 0.6); 
        assert!(n1 * 0.09 <= n8 && n8 <= n1 * 0.5); 
    }

    fn bench<F: Fn() -> ()>(label: &str, repeats: usize, f: F) -> f64 {
        let start = std::time::Instant::now();
        for _ in 0 .. repeats {
            f();
        }
        let time = start.elapsed() / repeats as u32;
        println!("     {} {:?}", label, time);
        time.as_nanos() as f64
    }
}

