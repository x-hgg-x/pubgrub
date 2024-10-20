// SPDX-License-Identifier: MPL-2.0
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::time::Duration;

use criterion::*;
use serde::de::Deserialize;

use pubgrub::{resolve, Map, OfflineDependencyProvider, Range, SemanticVersion, VersionSet};

fn bench<
    'a,
    P: Debug + Display + Clone + Eq + Hash + Deserialize<'a>,
    VS: VersionSet + Deserialize<'a>,
>(
    b: &mut Bencher,
    case: &'a str,
) where
    <VS as VersionSet>::V: Deserialize<'a>,
{
    let mut dependency_provider: OfflineDependencyProvider<P, VS> =
        ron::de::from_str(case).unwrap();

    let dependencies = dependency_provider
        .packages()
        .map(|p| {
            (
                p.clone(),
                dependency_provider.versions(p).unwrap().cloned().collect(),
            )
        })
        .collect::<Map<_, Vec<_>>>();

    b.iter(|| {
        for (p, versions) in &dependencies {
            for n in versions {
                let _ = resolve(&mut dependency_provider, p, n.clone());
            }
        }
    });
}

fn bench_nested(c: &mut Criterion) {
    let mut group = c.benchmark_group("large_cases");
    group.measurement_time(Duration::from_secs(20));

    for case in std::fs::read_dir("test-examples").unwrap() {
        let case = case.unwrap().path();
        let name = case.file_name().unwrap().to_string_lossy();
        let data = std::fs::read_to_string(&case).unwrap();
        if name.ends_with("u16_NumberVersion.ron") || name.ends_with("u16_u32.ron") {
            group.bench_function(name, |b| {
                bench::<u16, Range<u32>>(b, &data);
            });
        } else if name.ends_with("str_SemanticVersion.ron") {
            group.bench_function(name, |b| {
                bench::<&str, Range<SemanticVersion>>(b, &data);
            });
        }
    }

    group.finish();
}

criterion_group!(benches, bench_nested);
criterion_main!(benches);
