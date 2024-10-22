// SPDX-License-Identifier: MPL-2.0

use pubgrub::{
    DefaultStringReporter, OfflineDependencyProvider, PubGrubError, Range, Reporter,
    SemanticVersion,
};

type SemVS = Range<SemanticVersion>;

// https://github.com/dart-lang/pub/blob/master/doc/solver.md#linear-error-reporting
fn main() {
    let mut dependency_provider = OfflineDependencyProvider::<&str, SemVS>::new();
    #[rustfmt::skip]
    // root 1.0.0 depends on foo ^1.0.0 and baz ^1.0.0
        dependency_provider.add_dependencies(
        "root", (1, 0, 0),
        [
            ("foo", Range::from_range_bounds((1, 0, 0)..(2, 0, 0))),
            ("baz", Range::from_range_bounds((1, 0, 0)..(2, 0, 0))),
        ],
    );
    #[rustfmt::skip]
    // foo 1.0.0 depends on bar ^2.0.0
        dependency_provider.add_dependencies(
        "foo", (1, 0, 0),
        [("bar", Range::from_range_bounds((2, 0, 0)..(3, 0, 0)))],
    );
    #[rustfmt::skip]
    // bar 2.0.0 depends on baz ^3.0.0
        dependency_provider.add_dependencies(
        "bar", (2, 0, 0),
        [("baz", Range::from_range_bounds((3, 0, 0)..(4, 0, 0)))],
    );
    // baz 1.0.0 and 3.0.0 have no dependencies
    dependency_provider.add_dependencies("baz", (1, 0, 0), []);
    dependency_provider.add_dependencies("baz", (3, 0, 0), []);

    // Run the algorithm.
    match dependency_provider.resolve(&"root", (1, 0, 0)) {
        Ok(sol) => println!(
            "Solution: {:?}",
            sol.iter()
                .map(|(p, v)| {
                    (
                        p,
                        dependency_provider
                            .versions(p)
                            .unwrap()
                            .nth(v.get() as usize)
                            .unwrap(),
                    )
                })
                .collect::<Vec<_>>()
        ),
        Err(PubGrubError::NoSolution(mut derivation_tree)) => {
            derivation_tree.collapse_no_versions();
            eprintln!(
                "{}",
                DefaultStringReporter::report(&derivation_tree, &dependency_provider)
            );
            std::process::exit(1);
        }
        Err(err) => panic!("{:?}", err),
    };
}
