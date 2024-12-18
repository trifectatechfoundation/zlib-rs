use std::collections::{BTreeMap, BTreeSet};
use std::io::BufRead;
use std::process::Command;
use std::time::SystemTime;
use std::{env, fs};

use serde::{Deserialize, Serialize};

mod bench;

use bench::*;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct Commands(BTreeMap<String, Vec<String>>);

#[derive(Debug, Serialize, Deserialize)]
struct BenchData {
    // What and when are we benchmarking
    commit_hash: String,
    commit_timestamp: u64,

    // timestamp when the benchmark was started
    timestamp: SystemTime,

    // Where are we benchmarking it on
    arch: String,
    os: String,
    runner: String,
    cpu_model: String,

    // The actual results for benchmarks
    bench_groups: BTreeMap<String, Vec<SingleBench>>,
}

impl BenchData {
    fn render_markdown(&self, prev_results: Option<&Self>) -> String {
        if let Some(prev_results) = prev_results {
            assert_eq!(self.arch, prev_results.arch);
            assert_eq!(self.os, prev_results.os);
            assert_eq!(self.runner, prev_results.runner);
            assert_eq!(self.cpu_model, prev_results.cpu_model);
        }

        use std::fmt::Write;

        let mut md = String::new();

        if let Some(prev_results) = prev_results {
            writeln!(
                md,
                "## [`{commit}`](https://github.com/trifectatechfoundation/zlib-rs/commit/{commit}) with parent [`{commit_old}`](https://github.com/trifectatechfoundation/zlib-rs/commit/{commit_old}) \
                    (on {cpu})",
                commit = self.commit_hash,
                commit_old = prev_results.commit_hash,
                cpu = self.cpu_model
            )
                .unwrap();
        } else {
            writeln!(
            md,
            "## [`{commit}`](https://github.com/trifectatechfoundation/zlib-rs/commit/{commit}) \
                (on {cpu})",
            commit = self.commit_hash,
            cpu = self.cpu_model
        )
            .unwrap();
        }
        writeln!(md, "").unwrap();

        for (group_name, group_results) in &self.bench_groups {
            let prev_group_results = prev_results.and_then(|x| x.bench_groups.get(group_name));

            writeln!(md, "### {}", group_name).unwrap();
            writeln!(md).unwrap();

            let mut available_counters = BTreeSet::new();
            for bench in group_results {
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

            for bench in group_results {
                let prev_bench = prev_group_results
                    .and_then(|x| x.iter().find(|prev_bench| prev_bench.cmd == bench.cmd));

                write!(md, "|`{}`|", bench.cmd.join(" ")).unwrap();

                for &counter in &available_counters {
                    if let Some(data) = bench.counters.get(counter) {
                        if let Some(prev_data) = prev_bench.and_then(|prev_bench| {
                            prev_bench.counters.get(
                                counter
                                    .strip_prefix("cpu_core/")
                                    .unwrap_or(counter)
                                    .strip_suffix("/")
                                    .unwrap_or(&counter),
                            )
                        }) {
                            let diff = if data.value > prev_data.value {
                                format!(
                                    "+{:.1}%",
                                    (data.value - prev_data.value) as f64 / prev_data.value as f64
                                        * 100.
                                )
                            } else {
                                format!(
                                    "+{:.1}%",
                                    (prev_data.value - data.value) as f64 / prev_data.value as f64
                                        * 100.
                                )
                            };

                            write!(md, "`{}` {} ({diff})|", data.value, data.unit).unwrap();
                        } else {
                            write!(md, "`{}` {}|", data.value, data.unit).unwrap();
                        }
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
    let (commit_hash, commit_timestamp) = {
        match env::var("GITHUB_SHA") {
            Ok(sha) => {
                // git show 27b31a568651dd725488e422e854095639d75af6 --no-patch --pretty=format:"%ct"
                let output = Command::new("git")
                    .args(&["show", &sha, "--no-patch", "--pretty=format:\"%ct\""])
                    .output()
                    .unwrap();

                let timestamp: u64 = String::from_utf8_lossy(&output.stdout)
                    .trim()
                    .trim_matches('"')
                    .parse()
                    .unwrap();

                (sha, timestamp)
            }
            Err(_) => (String::new(), 0),
        }
    };

    let mut bench_data = BenchData {
        commit_hash,
        commit_timestamp,
        timestamp: SystemTime::now(),

        arch: env::var("RUNNER_ARCH").unwrap_or_default(),
        os: env::var("RUNNER_OS").unwrap_or_default(),
        runner: env::var("RUNNER_NAME").unwrap_or_else(|_| "<local bench>".to_owned()),
        cpu_model: get_cpu_model(),

        bench_groups: BTreeMap::new(),
    };

    let commands: Commands =
        serde_json::from_slice(&fs::read(env::args().nth(1).unwrap()).unwrap()).unwrap();

    let prev_results = (|| {
        let base_commit = String::from_utf8(
            Command::new("git")
                .arg("rev-parse")
                .arg("HEAD~")
                .output()
                .unwrap()
                .stdout,
        )
        .unwrap()
        .trim()
        .to_owned();

        for line in fs::read(env::args().nth(2).unwrap()).unwrap().lines() {
            let Ok(data) = serde_json::from_str::<BenchData>(&line.unwrap()) else {
                continue; // Data format likely changed
            };

            if data.commit_hash == base_commit {
                return Some(data);
            }
        }

        None
    })();

    for (group_name, benches) in commands.0 {
        let mut group_results = vec![];
        for cmd in benches {
            group_results.push(bench_single_cmd(
                cmd.split(" ").map(|arg| arg.to_owned()).collect(),
            ));
        }
        bench_data.bench_groups.insert(group_name, group_results);
    }

    println!("{}", serde_json::to_string(&bench_data).unwrap());

    eprintln!("{}", bench_data.render_markdown(prev_results.as_ref()));
    if let Ok(path) = env::var("GITHUB_STEP_SUMMARY") {
        fs::write(path, bench_data.render_markdown(prev_results.as_ref())).unwrap();
    }
}
