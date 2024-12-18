use std::collections::{BTreeMap, BTreeSet};
use std::process::Command;
use std::time::SystemTime;
use std::{env, fs};

use serde::{Deserialize, Serialize};

mod bench;

use bench::*;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct Commands(BTreeMap<String, Vec<String>>);

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
    if cfg!(target_os = "linux") {
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
    } else if cfg!(target_os = "macos") {
        String::from_utf8(
            Command::new("sysctl")
                .arg("-n")
                .arg("machdep.cpu.brand_string")
                .output()
                .unwrap()
                .stdout,
        )
        .unwrap()
        .trim()
        .to_owned()
    } else {
        "unknown".to_owned()
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
