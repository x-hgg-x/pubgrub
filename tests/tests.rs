// SPDX-License-Identifier: MPL-2.0

use pubgrub::{OfflineDependencyProvider, PubGrubError, Range};

type NumVS = Range<u32>;

#[test]
fn same_result_on_repeated_runs() {
    let mut dependency_provider = OfflineDependencyProvider::<_, NumVS>::new();

    dependency_provider.add_dependencies("c", 0u32, []);
    dependency_provider.add_dependencies("c", 2u32, []);
    dependency_provider.add_dependencies("b", 0u32, []);
    dependency_provider.add_dependencies("b", 1u32, [("c", Range::between(0u32, 1u32))]);

    dependency_provider.add_dependencies("a", 0u32, [("b", Range::full()), ("c", Range::full())]);

    let name = "a";
    let ver: u32 = 0;
    let one = dependency_provider.resolve(&name, ver);
    for _ in 0..10 {
        match (&one, &dependency_provider.resolve(&name, ver)) {
            (Ok(l), Ok(r)) => assert_eq!(l, r),
            _ => panic!("not the same result"),
        }
    }
}

#[test]
fn should_always_find_a_satisfier() {
    let mut dependency_provider = OfflineDependencyProvider::<_, NumVS>::new();
    dependency_provider.add_dependencies("a", 0u32, [("b", Range::empty())]);
    assert!(matches!(
        dependency_provider.resolve(&"a", 0u32),
        Err(PubGrubError::NoSolution { .. })
    ));

    dependency_provider.add_dependencies("c", 0u32, [("a", Range::full())]);
    assert!(matches!(
        dependency_provider.resolve(&"c", 0u32),
        Err(PubGrubError::NoSolution { .. })
    ));
}

#[test]
fn depend_on_self() {
    let mut dependency_provider = OfflineDependencyProvider::<_, NumVS>::new();
    dependency_provider.add_dependencies("a", 0u32, [("a", Range::full())]);
    assert!(dependency_provider.resolve(&"a", 0u32).is_ok());
    dependency_provider.add_dependencies("a", 66u32, [("a", Range::singleton(111u32))]);
    assert!(dependency_provider.resolve(&"a", 66u32).is_err());
}
