// SPDX-License-Identifier: MPL-2.0
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::time::Duration;

use criterion::*;
use serde::de::Deserialize;

use pubgrub::{resolve, Map, OfflineDependencyProvider, Range, Version, VersionRange};

fn bench<
    'a,
    P: Debug + Display + Clone + Eq + Hash + Deserialize<'a>,
    R: VersionRange + Deserialize<'a>,
>(
    b: &mut Bencher,
    case: &'a str,
) where
    R::V: Deserialize<'a>,
{
    let mut dependency_provider: OfflineDependencyProvider<P, R> = ron::de::from_str(case).unwrap();

    let dependencies = dependency_provider
        .packages()
        .map(|p| {
            (
                p.clone(),
                (0..dependency_provider.versions(p).unwrap().count())
                    .map(|i| Version::new(i as u8).unwrap())
                    .collect(),
            )
        })
        .collect::<Map<_, Vec<_>>>();

    b.iter(|| {
        for (p, versions) in &dependencies {
            for &n in versions {
                let _ = resolve(&mut dependency_provider, p, n);
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
        if name.ends_with("u16_NumberVersion.ron") {
            group.bench_function(name, |b| {
                bench::<u16, Range<u32>>(b, &data);
            });
        }
    }

    group.finish();
}

criterion_group!(benches, bench_nested);
criterion_main!(benches);
