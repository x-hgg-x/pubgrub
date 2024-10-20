// SPDX-License-Identifier: MPL-2.0

use pubgrub::{
    resolve, DefaultStringReporter, DependencyProvider, Map, OfflineDependencyProvider,
    PubGrubError, Range, Reporter as _, SemanticVersion, Set,
};

type NumVS = Range<u32>;
type SemVS = Range<SemanticVersion>;

use std::io::Write;

use log::LevelFilter;

fn init_log() {
    let _ = env_logger::builder()
        .filter_level(LevelFilter::Trace)
        .format(|buf, record| writeln!(buf, "{}", record.args()))
        .is_test(true)
        .try_init();
}

#[test]
/// https://github.com/dart-lang/pub/blob/master/doc/solver.md#no-conflicts
fn no_conflict() {
    init_log();
    let mut dependency_provider = OfflineDependencyProvider::<&str, SemVS>::new();
    #[rustfmt::skip]
        dependency_provider.add_dependencies(
        "root", (1, 0, 0),
        [("foo", Range::between((1, 0, 0), (2, 0, 0)))],
    );
    #[rustfmt::skip]
        dependency_provider.add_dependencies(
        "foo", (1, 0, 0),
        [("bar", Range::between((1, 0, 0), (2, 0, 0)))],
    );
    dependency_provider.add_dependencies("bar", (1, 0, 0), []);
    dependency_provider.add_dependencies("bar", (2, 0, 0), []);

    // Run the algorithm.
    let computed_solution = resolve(&mut dependency_provider, &"root", (1, 0, 0)).unwrap();

    // Solution.
    let mut expected_solution = Map::default();
    expected_solution.insert("root", (1, 0, 0).into());
    expected_solution.insert("foo", (1, 0, 0).into());
    expected_solution.insert("bar", (1, 0, 0).into());

    // Comparing the true solution with the one computed by the algorithm.
    assert_eq!(expected_solution, computed_solution);
}

#[test]
/// https://github.com/dart-lang/pub/blob/master/doc/solver.md#avoiding-conflict-during-decision-making
fn avoiding_conflict_during_decision_making() {
    init_log();
    let mut dependency_provider = OfflineDependencyProvider::<&str, SemVS>::new();
    #[rustfmt::skip]
        dependency_provider.add_dependencies(
        "root", (1, 0, 0),
        [
            ("foo", Range::between((1, 0, 0), (2, 0, 0))),
            ("bar", Range::between((1, 0, 0), (2, 0, 0))),
        ],
    );
    #[rustfmt::skip]
        dependency_provider.add_dependencies(
        "foo", (1, 1, 0),
        [("bar", Range::between((2, 0, 0), (3, 0, 0)))],
    );
    dependency_provider.add_dependencies("foo", (1, 0, 0), []);
    dependency_provider.add_dependencies("bar", (1, 0, 0), []);
    dependency_provider.add_dependencies("bar", (1, 1, 0), []);
    dependency_provider.add_dependencies("bar", (2, 0, 0), []);

    // Run the algorithm.
    let computed_solution = resolve(&mut dependency_provider, &"root", (1, 0, 0)).unwrap();

    // Solution.
    let mut expected_solution = Map::default();
    expected_solution.insert("root", (1, 0, 0).into());
    expected_solution.insert("foo", (1, 0, 0).into());
    expected_solution.insert("bar", (1, 1, 0).into());

    // Comparing the true solution with the one computed by the algorithm.
    assert_eq!(expected_solution, computed_solution);
}

#[test]
/// https://github.com/dart-lang/pub/blob/master/doc/solver.md#performing-conflict-resolution
fn conflict_resolution() {
    init_log();
    let mut dependency_provider = OfflineDependencyProvider::<&str, SemVS>::new();
    #[rustfmt::skip]
        dependency_provider.add_dependencies(
        "root", (1, 0, 0),
        [("foo", Range::higher_than((1, 0, 0)))],
    );
    #[rustfmt::skip]
        dependency_provider.add_dependencies(
        "foo", (2, 0, 0),
        [("bar", Range::between((1, 0, 0), (2, 0, 0)))],
    );
    dependency_provider.add_dependencies("foo", (1, 0, 0), []);
    #[rustfmt::skip]
        dependency_provider.add_dependencies(
        "bar", (1, 0, 0),
        [("foo", Range::between((1, 0, 0), (2, 0, 0)))],
    );

    // Run the algorithm.
    let computed_solution = resolve(&mut dependency_provider, &"root", (1, 0, 0)).unwrap();

    // Solution.
    let mut expected_solution = Map::default();
    expected_solution.insert("root", (1, 0, 0).into());
    expected_solution.insert("foo", (1, 0, 0).into());

    // Comparing the true solution with the one computed by the algorithm.
    assert_eq!(expected_solution, computed_solution);
}

#[test]
/// https://github.com/dart-lang/pub/blob/master/doc/solver.md#conflict-resolution-with-a-partial-satisfier
fn conflict_with_partial_satisfier() {
    init_log();
    let mut dependency_provider = OfflineDependencyProvider::<&str, SemVS>::new();
    #[rustfmt::skip]
    // root 1.0.0 depends on foo ^1.0.0 and target ^2.0.0
        dependency_provider.add_dependencies(
        "root", (1, 0, 0),
        [
            ("foo", Range::between((1, 0, 0), (2, 0, 0))),
            ("target", Range::between((2, 0, 0), (3, 0, 0))),
        ],
    );
    #[rustfmt::skip]
    // foo 1.1.0 depends on left ^1.0.0 and right ^1.0.0
        dependency_provider.add_dependencies(
        "foo", (1, 1, 0),
        [
            ("left", Range::between((1, 0, 0), (2, 0, 0))),
            ("right", Range::between((1, 0, 0), (2, 0, 0))),
        ],
    );
    dependency_provider.add_dependencies("foo", (1, 0, 0), []);
    #[rustfmt::skip]
    // left 1.0.0 depends on shared >=1.0.0
        dependency_provider.add_dependencies(
        "left", (1, 0, 0),
        [("shared", Range::higher_than((1, 0, 0)))],
    );
    #[rustfmt::skip]
    // right 1.0.0 depends on shared <2.0.0
        dependency_provider.add_dependencies(
        "right", (1, 0, 0),
        [("shared", Range::strictly_lower_than((2, 0, 0)))],
    );
    dependency_provider.add_dependencies("shared", (2, 0, 0), []);
    #[rustfmt::skip]
    // shared 1.0.0 depends on target ^1.0.0
        dependency_provider.add_dependencies(
        "shared", (1, 0, 0),
        [("target", Range::between((1, 0, 0), (2, 0, 0)))],
    );
    dependency_provider.add_dependencies("target", (2, 0, 0), []);
    dependency_provider.add_dependencies("target", (1, 0, 0), []);

    // Run the algorithm.
    let computed_solution = resolve(&mut dependency_provider, &"root", (1, 0, 0)).unwrap();

    // Solution.
    let mut expected_solution = Map::default();
    expected_solution.insert("root", (1, 0, 0).into());
    expected_solution.insert("foo", (1, 0, 0).into());
    expected_solution.insert("target", (2, 0, 0).into());

    // Comparing the true solution with the one computed by the algorithm.
    assert_eq!(expected_solution, computed_solution);
}

#[test]
/// a0 dep on b and c
/// b0 dep on d0
/// b1 dep on d1 (not existing)
/// c0 has no dep
/// c1 dep on d2 (not existing)
/// d0 has no dep
///
/// Solution: a0, b0, c0, d0
fn double_choices() {
    init_log();
    let mut dependency_provider = OfflineDependencyProvider::<&str, NumVS>::new();
    dependency_provider.add_dependencies("a", 0u32, [("b", Range::full()), ("c", Range::full())]);
    dependency_provider.add_dependencies("b", 0u32, [("d", Range::singleton(0u32))]);
    dependency_provider.add_dependencies("b", 1u32, [("d", Range::singleton(1u32))]);
    dependency_provider.add_dependencies("c", 0u32, []);
    dependency_provider.add_dependencies("c", 1u32, [("d", Range::singleton(2u32))]);
    dependency_provider.add_dependencies("d", 0u32, []);

    // Solution.
    let mut expected_solution = Map::default();
    expected_solution.insert("a", 0u32);
    expected_solution.insert("b", 0u32);
    expected_solution.insert("c", 0u32);
    expected_solution.insert("d", 0u32);

    // Run the algorithm.
    let computed_solution = resolve(&mut dependency_provider, &"a", 0u32).unwrap();
    assert_eq!(expected_solution, computed_solution);
}

#[test]
fn confusing_with_lots_of_holes() {
    let mut dependency_provider = OfflineDependencyProvider::<&str, NumVS>::new();

    // root depends on foo...
    dependency_provider.add_dependencies(
        "root",
        1u32,
        vec![("foo", Range::full()), ("baz", Range::full())],
    );

    for i in 1..6 {
        // foo depends on bar...
        dependency_provider.add_dependencies("foo", i as u32, vec![("bar", Range::full())]);
    }

    // This package is part of the dependency tree, but it's not part of the conflict
    dependency_provider.add_dependencies("baz", 1u32, vec![]);

    let Err(PubGrubError::NoSolution(mut derivation_tree)) =
        resolve(&mut dependency_provider, &"root", 1u32)
    else {
        unreachable!()
    };
    assert_eq!(
        &DefaultStringReporter::report(&derivation_tree, &dependency_provider),
        r#"Because there is no available version for bar and foo 1 | 2 | 3 | 4 | 5 depends on bar, foo 1 | 2 | 3 | 4 | 5 is forbidden.
And because there is no version of foo in <1 | >1, <2 | >2, <3 | >3, <4 | >4, <5 | >5 and root 1 depends on foo, root 1 is forbidden."#
    );
    derivation_tree.collapse_no_versions();
    assert_eq!(
        &DefaultStringReporter::report(&derivation_tree, &dependency_provider),
        "Because foo depends on bar and root 1 depends on foo, root 1 is forbidden."
    );
    assert_eq!(
        derivation_tree.packages(),
        // baz isn't shown.
        Set::from_iter([
            dependency_provider.name_to_package(&"root").unwrap(),
            dependency_provider.name_to_package(&"foo").unwrap(),
            dependency_provider.name_to_package(&"bar").unwrap(),
        ])
    );
}
