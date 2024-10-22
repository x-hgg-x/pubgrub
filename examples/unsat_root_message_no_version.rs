// SPDX-License-Identifier: MPL-2.0

use std::fmt::{self, Debug, Display};
use std::hash::Hash;

use pubgrub::{
    DefaultStringReporter, DependencyProvider, Derived, External, Map, OfflineDependencyProvider,
    Package, PubGrubError, Range, ReportFormatter, Reporter, SemanticVersion, Term, VersionSet,
};
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CustomPackage {
    Root,
    Package(String),
}

impl Display for CustomPackage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CustomPackage::Root => write!(f, "root"),
            CustomPackage::Package(name) => write!(f, "{}", name),
        }
    }
}

type Dp = OfflineDependencyProvider<CustomPackage, Range<SemanticVersion>>;

#[derive(Debug, Default)]
struct CustomReportFormatter;

impl ReportFormatter<Dp, String> for CustomReportFormatter {
    type Output = String;

    fn format_terms(&self, terms: &Map<Package, Term>, dp: &Dp) -> String {
        let terms_vec: Vec<_> = terms
            .iter()
            .map(|(&p, &v)| (p, dp.package_to_name(p).unwrap(), v))
            .collect();
        match terms_vec.as_slice() {
            [] => "version solving failed".into(),
            [(_, package @ CustomPackage::Root, t)] if t.is_positive() => {
                format!("{package} is forbidden")
            }
            [(_, package @ CustomPackage::Root, _)] => {
                format!("{package} is mandatory")
            }
            &[(p, CustomPackage::Package(_), t)] if t.is_positive() => {
                format!(
                    "{} is forbidden",
                    dp.package_version_set_repr(p, t.version_set()),
                )
            }
            &[(p, CustomPackage::Package(_), t)] => {
                format!(
                    "{} is mandatory",
                    dp.package_version_set_repr(p, t.version_set()),
                )
            }
            &[(p1, _, t1), (p2, _, t2)] if t1.is_positive() && t2.is_negative() => {
                External::<String>::FromDependencyOf(p1, t1.version_set(), p2, t2.version_set())
                    .display(dp)
                    .to_string()
            }
            &[(p1, _, t1), (p2, _, t2)] if t1.is_negative() && t2.is_positive() => {
                External::<String>::FromDependencyOf(p2, t2.version_set(), p1, t1.version_set())
                    .display(dp)
                    .to_string()
            }
            slice => {
                let str_terms: Vec<_> = slice.iter().map(|(_, p, t)| format!("{p} {t}")).collect();
                str_terms.join(", ") + " are incompatible"
            }
        }
    }

    fn format_external(&self, external: &External<String>, dp: &Dp) -> String {
        match *external {
            External::NotRoot(package, version) => {
                format!(
                    "we are solving dependencies of {}",
                    dp.package_version_repr(package, version),
                )
            }
            External::NoVersions(package, set) => {
                if set == VersionSet::full() {
                    format!(
                        "there is no available version for {}",
                        dp.package_to_name(package).unwrap(),
                    )
                } else {
                    format!(
                        "there is no version of {}",
                        dp.package_version_set_repr(package, set),
                    )
                }
            }
            External::Custom(package, set, ref reason) => {
                if set == VersionSet::full() {
                    format!(
                        "dependencies of {} are unavailable because {reason}",
                        dp.package_to_name(package).unwrap()
                    )
                } else {
                    format!(
                        "dependencies of {} are unavailable because {reason}",
                        dp.package_version_set_repr(package, set),
                    )
                }
            }
            External::FromDependencyOf(package, package_set, dependency, dependency_set) => {
                let package_name = dp.package_to_name(package).unwrap();
                if package_set == VersionSet::full() && dependency_set == VersionSet::full() {
                    format!(
                        "{package_name} depends on {}",
                        dp.package_to_name(dependency).unwrap()
                    )
                } else if package_set == VersionSet::full() {
                    format!(
                        "{package_name} depends on {}",
                        dp.package_version_set_repr(dependency, dependency_set),
                    )
                } else if dependency_set == VersionSet::full() {
                    if matches!(package_name, CustomPackage::Root) {
                        // Exclude the dummy version for root packages
                        format!(
                            "{package_name} depends on {}",
                            dp.package_to_name(dependency).unwrap()
                        )
                    } else {
                        format!(
                            "{} depends on {}",
                            dp.package_version_set_repr(package, package_set),
                            dp.package_to_name(dependency).unwrap()
                        )
                    }
                } else if matches!(package_name, CustomPackage::Root) {
                    // Exclude the dummy version for root packages
                    format!(
                        "{package_name} depends on {}",
                        dp.package_version_set_repr(dependency, dependency_set),
                    )
                } else {
                    format!(
                        "{} depends on {}",
                        dp.package_version_set_repr(package, package_set),
                        dp.package_version_set_repr(dependency, dependency_set),
                    )
                }
            }
        }
    }

    /// Simplest case, we just combine two external incompatibilities.
    fn explain_both_external(
        &self,
        external1: &External<String>,
        external2: &External<String>,
        current_terms: &Map<Package, Term>,
        dp: &Dp,
    ) -> String {
        // TODO: order should be chosen to make it more logical.
        format!(
            "Because {} and {}, {}.",
            self.format_external(external1, dp),
            self.format_external(external2, dp),
            self.format_terms(current_terms, dp)
        )
    }

    /// Both causes have already been explained so we use their refs.
    fn explain_both_ref(
        &self,
        ref_id1: usize,
        derived1: &Derived<String>,
        ref_id2: usize,
        derived2: &Derived<String>,
        current_terms: &Map<Package, Term>,
        dp: &Dp,
    ) -> String {
        // TODO: order should be chosen to make it more logical.
        format!(
            "Because {} ({}) and {} ({}), {}.",
            self.format_terms(&derived1.terms, dp),
            ref_id1,
            self.format_terms(&derived2.terms, dp),
            ref_id2,
            self.format_terms(current_terms, dp)
        )
    }

    /// One cause is derived (already explained so one-line),
    /// the other is a one-line external cause,
    /// and finally we conclude with the current incompatibility.
    fn explain_ref_and_external(
        &self,
        ref_id: usize,
        derived: &Derived<String>,
        external: &External<String>,
        current_terms: &Map<Package, Term>,
        dp: &Dp,
    ) -> String {
        // TODO: order should be chosen to make it more logical.
        format!(
            "Because {} ({}) and {}, {}.",
            self.format_terms(&derived.terms, dp),
            ref_id,
            self.format_external(external, dp),
            self.format_terms(current_terms, dp)
        )
    }

    /// Add an external cause to the chain of explanations.
    fn and_explain_external(
        &self,
        external: &External<String>,
        current_terms: &Map<Package, Term>,
        dp: &Dp,
    ) -> String {
        format!(
            "And because {}, {}.",
            self.format_external(external, dp),
            self.format_terms(current_terms, dp)
        )
    }

    /// Add an already explained incompat to the chain of explanations.
    fn and_explain_ref(
        &self,
        ref_id: usize,
        derived: &Derived<String>,
        current_terms: &Map<Package, Term>,
        dp: &Dp,
    ) -> String {
        format!(
            "And because {} ({}), {}.",
            self.format_terms(&derived.terms, dp),
            ref_id,
            self.format_terms(current_terms, dp)
        )
    }

    /// Add an already explained incompat to the chain of explanations.
    fn and_explain_prior_and_external(
        &self,
        prior_external: &External<String>,
        external: &External<String>,
        current_terms: &Map<Package, Term>,
        dp: &Dp,
    ) -> String {
        format!(
            "And because {} and {}, {}.",
            self.format_external(prior_external, dp),
            self.format_external(external, dp),
            self.format_terms(current_terms, dp)
        )
    }
}

fn main() {
    let mut dependency_provider =
        OfflineDependencyProvider::<CustomPackage, Range<SemanticVersion>>::new();

    // Define the root package with a dependency on a package we do not provide
    dependency_provider.add_dependencies(
        CustomPackage::Root,
        (0, 0, 0),
        vec![(
            CustomPackage::Package("foo".to_string()),
            Range::singleton((1, 0, 0)),
        )],
    );

    // Run the algorithm
    match dependency_provider.resolve(&CustomPackage::Root, (0, 0, 0)) {
        Ok(sol) => println!("{:?}", sol),
        Err(PubGrubError::NoSolution(derivation_tree)) => {
            eprintln!("No solution.\n");

            eprintln!("### Default report:");
            eprintln!("```");
            eprintln!(
                "{}",
                DefaultStringReporter::report(&derivation_tree, &dependency_provider)
            );
            eprintln!("```\n");

            eprintln!("### Report with custom formatter:");
            eprintln!("```");
            eprintln!(
                "{}",
                DefaultStringReporter::report_with_formatter(
                    &derivation_tree,
                    &CustomReportFormatter,
                    &dependency_provider
                )
            );
            eprintln!("```");
            std::process::exit(1);
        }
        Err(err) => panic!("{:?}", err),
    };
}
