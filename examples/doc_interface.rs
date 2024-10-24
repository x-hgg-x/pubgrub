// SPDX-License-Identifier: MPL-2.0

use pubgrub::{OfflineDependencyProvider, Range};

type NumVS = Range<u32>;

// `root` depends on `menu` and `icons`
// `menu` depends on `dropdown`
// `dropdown` depends on `icons`
// `icons` has no dependency
#[rustfmt::skip]
fn main() {
    let mut dependency_provider = OfflineDependencyProvider::<&str, NumVS>::new();
    dependency_provider.add_dependencies(
        "root", 1u32, [("menu", Range::full()), ("icons", Range::full())],
    );
    dependency_provider.add_dependencies("menu", 1u32, [("dropdown", Range::full())]);
    dependency_provider.add_dependencies("dropdown", 1u32, [("icons", Range::full())]);
    dependency_provider.add_dependencies("icons", 1u32, []);

    // Run the algorithm.
    let solution = dependency_provider.resolve(&"root", 1u32).unwrap();
    println!(
        "Solution: {:?}",
        solution.iter().map(|(p, v)| {
            (p, dependency_provider.versions(p).unwrap().nth(v.get() as usize).unwrap())
        })
        .collect::<Vec<_>>()
    );
}
