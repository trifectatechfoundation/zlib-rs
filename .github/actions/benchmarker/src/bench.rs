use std::collections::BTreeMap;
use std::process::Command;

use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct SingleBench {
    pub cmd: Vec<String>,
    pub counters: BTreeMap<String, BenchCounter>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct BenchCounter {
    pub value: f64,
    pub unit: String,
}

pub fn bench_single_cmd(cmd: Vec<String>) -> SingleBench {
    if cfg!(target_os = "linux") {
        bench_single_cmd_perf(cmd)
    } else {
        bench_single_cmd_getrusage(cmd)
    }
}

fn bench_single_cmd_perf(cmd: Vec<String>) -> SingleBench {
    #[derive(Debug, Deserialize)]
    #[serde(rename_all = "kebab-case")]
    struct PerfData {
        event: String,
        counter_value: String,
        unit: String,
    }

    let mut perf_stat_cmd = Command::new("perf");
    perf_stat_cmd
        // Perf produces broken JSON when the system locale uses decimal comma rather than decimal point.
        .env("LANG", "C")
        .arg("stat")
        .arg("-j")
        .arg("-e")
        .arg("task-clock,cycles,instructions")
        .arg("--repeat")
        .arg("5") // FIXME 20
        .arg("--");
    perf_stat_cmd.args(&cmd);

    let output = perf_stat_cmd.output().unwrap();
    assert!(
        output.status.success(),
        "`{:?}` failed with {:?}:=== stdout ===\n{}\n\n=== stderr ===\n{}",
        perf_stat_cmd,
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    let counters = String::from_utf8(output.stderr)
        .unwrap()
        .lines()
        .map(|line| {
            serde_json::from_str::<PerfData>(line)
                .unwrap_or_else(|e| panic!("Failed to parse {line:?}: {e}"))
        })
        .filter(|counter| counter.counter_value != "<not counted>")
        .map(|counter| {
            (
                counter.event,
                BenchCounter {
                    value: counter
                        .counter_value
                        .parse::<f64>()
                        .unwrap_or_else(|_| panic!("Failed to parse {}", counter.counter_value)),
                    unit: counter.unit,
                },
            )
        })
        .collect::<BTreeMap<_, _>>();

    SingleBench { cmd, counters }
}

fn bench_single_cmd_getrusage(cmd: Vec<String>) -> SingleBench {
    use std::mem;
    use std::time::Duration;

    fn get_cpu_times() -> Duration {
        use libc::{getrusage, rusage, RUSAGE_CHILDREN};

        let result: rusage = unsafe {
            let mut buf = mem::zeroed();
            let success = getrusage(RUSAGE_CHILDREN, &mut buf);
            assert_eq!(0, success);
            buf
        };

        Duration::new(
            result.ru_utime.tv_sec as _,
            (result.ru_utime.tv_usec * 1000) as _,
        )
    }

    let mut bench_cmd = Command::new(cmd.get(0).unwrap());
    bench_cmd.args(&cmd[1..]);

    let start_cpu = get_cpu_times();
    let output = bench_cmd.output().unwrap();
    let user_time = get_cpu_times() - start_cpu;
    assert!(
        output.status.success(),
        "`{:?}` failed with {:?}:\n=== stdout ===\n{}\n\n=== stderr ===\n{}",
        bench_cmd,
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    SingleBench {
        cmd,
        counters: BTreeMap::from_iter([(
            "user-time".to_owned(),
            BenchCounter {
                value: user_time.as_secs_f64() * 1000.0,
                unit: "msec".to_owned(),
            },
        )]),
    }
}
