use std::collections::{BTreeMap, BTreeSet};
use std::process::Command;
use std::time::SystemTime;
use std::{env, fs};

use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct Commands(BTreeMap<String, Vec<String>>);

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct PerfData {
    event: String,
    counter_value: String,
    unit: String,
}

#[derive(Debug, Serialize)]
struct BenchData {
    // What and when are we benchmarking
    commit_hash: String,
    timestamp: SystemTime,

    // Where are we benchmarking it on
    arch: String,
    os: String,
    runner: String,
    cpu_model: String,

    // The actual results for benchmarks
    bench_groups: Vec<BenchGroup>,
}

#[derive(Debug, Serialize)]
struct BenchGroup {
    group_name: String,
    results: Vec<SingleBench>,
}

#[derive(Debug, Serialize)]
struct SingleBench {
    cmd: Vec<String>,
    counters: BTreeMap<String, BenchCounter>,
}

#[derive(Debug, Serialize)]
struct BenchCounter {
    value: String,
    unit: String,
}

impl BenchData {
    fn render_markdown(&self) -> String {
        use std::fmt::Write;

        let mut md = String::new();

        writeln!(
            md,
            "## [`{commit}`](https://github.com/trifectatechfoundation/zlib-rs/commit/{commit}) \
                (on {cpu})",
            commit = self.commit_hash,
            cpu = self.cpu_model
        )
        .unwrap();
        writeln!(md, "").unwrap();

        for bench_group in &self.bench_groups {
            writeln!(md, "### {}", bench_group.group_name).unwrap();
            writeln!(md).unwrap();

            let mut available_counters = BTreeSet::new();
            for bench in &bench_group.results {
                for counter in bench.counters.keys() {
                    available_counters.insert(counter);
                }
            }

            write!(md, "|command|").unwrap();
            for counter in &available_counters {
                write!(md, "{counter}|").unwrap();
            }
            writeln!(md).unwrap();
            write!(md, "|---|").unwrap();
            for _ in &available_counters {
                write!(md, "---|").unwrap();
            }
            writeln!(md).unwrap();

            for bench in &bench_group.results {
                write!(md, "|`{}`|", bench.cmd.join(" ")).unwrap();
                for &counter in &available_counters {
                    if let Some(data) = bench.counters.get(counter) {
                        write!(md, "`{}` {}|", data.value, data.unit).unwrap();
                    } else {
                        write!(md, "|").unwrap();
                    }
                }
                writeln!(md).unwrap();
            }
        }

        md
    }
}

fn get_cpu_model() -> String {
    if !cfg!(target_os = "linux") {
        return "<unknown>".to_owned();
    }

    serde_json::from_slice::<serde_json::Value>(
        &Command::new("lscpu").arg("-J").output().unwrap().stdout,
    )
    .unwrap()["lscpu"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["field"] == "Model name:")
        .unwrap()["data"]
        .as_str()
        .unwrap()
        .to_owned()
}

fn bench_single_cmd(cmd: Vec<String>) -> SingleBench {
    if cfg!(target_os = "linux") {
        bench_single_cmd_perf(cmd)
    } else {
        bench_single_cmd_getrusage(cmd)
    }
}

fn bench_single_cmd_perf(cmd: Vec<String>) -> SingleBench {
    let mut perf_stat_cmd = Command::new("perf");
    perf_stat_cmd
        .arg("stat")
        .arg("-j")
        .arg("-e")
        .arg("task-clock,cycles,instructions")
        .arg("--repeat")
        .arg("1") // FIXME 20
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
        .map(|line| serde_json::from_str::<PerfData>(line).unwrap())
        .map(|counter| {
            (
                counter.event,
                BenchCounter {
                    value: counter.counter_value,
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
                value: format!("{:.06}", user_time.as_secs_f64() * 1000.0),
                unit: "msec".to_owned(),
            },
        )]),
    }
}

fn main() {
    let mut bench_data = BenchData {
        commit_hash: env::var("GITHUB_SHA").unwrap_or_default(),
        timestamp: SystemTime::now(),

        arch: env::var("RUNNER_ARCH").unwrap_or_default(),
        os: env::var("RUNNER_OS").unwrap_or_default(),
        runner: env::var("RUNNER_NAME").unwrap_or_else(|_| "<local bench>".to_owned()),
        cpu_model: get_cpu_model(),

        bench_groups: vec![],
    };

    let commands: Commands =
        serde_json::from_slice(&fs::read(env::args().nth(1).unwrap()).unwrap()).unwrap();

    for (group_name, benches) in commands.0 {
        let mut group = BenchGroup {
            group_name,
            results: vec![],
        };
        for cmd in benches {
            group.results.push(bench_single_cmd(
                cmd.split(" ").map(|arg| arg.to_owned()).collect(),
            ));
        }
        bench_data.bench_groups.push(group);
    }

    println!("{}", serde_json::to_string(&bench_data).unwrap());

    eprintln!("{}", bench_data.render_markdown());
    if let Ok(path) = env::var("GITHUB_STEP_SUMMARY") {
        fs::write(path, bench_data.render_markdown()).unwrap();
    }
}
