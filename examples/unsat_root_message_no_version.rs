// SPDX-License-Identifier: MPL-2.0

use std::fmt::{self, Debug, Display};
use std::hash::Hash;

use pubgrub::{
    resolve, DefaultStringReporter, Derived, External, Map, OfflineDependencyProvider,
    PackageArena, PackageId, PubGrubError, Ranges, ReportFormatter, Reporter, SemanticVersion,
    Term,
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
            CustomPackage::Package(name) => write!(f, "{name}"),
        }
    }
}

type Dp = OfflineDependencyProvider<CustomPackage, Ranges<SemanticVersion>>;
type Store = PackageArena<CustomPackage>;

#[derive(Debug, Default)]
struct CustomReportFormatter;

impl ReportFormatter<Dp> for CustomReportFormatter {
    type Output = String;

    fn format_terms(
        &self,
        terms: &Map<PackageId, Term<Ranges<SemanticVersion>>>,
        package_store: &Store,
    ) -> String {
        let terms_vec: Vec<_> = terms
            .iter()
            .map(|(&pid, v)| (pid, package_store.pkg(pid).unwrap(), v))
            .collect();
        match terms_vec.as_slice() {
            [] => "version solving failed".into(),
            [(_, package @ CustomPackage::Root, Term::Positive(_))] => {
                format!("{package} is forbidden")
            }
            [(_, package @ CustomPackage::Root, Term::Negative(_))] => {
                format!("{package} is mandatory")
            }
            [(_, package @ CustomPackage::Package(_), Term::Positive(ranges))] => {
                format!("{package} {ranges} is forbidden")
            }
            [(_, package @ CustomPackage::Package(_), Term::Negative(ranges))] => {
                format!("{package} {ranges} is mandatory")
            }
            &[(pid1, _, Term::Positive(r1)), (pid2, _, Term::Negative(r2))] => {
                External::FromDependencyOf(pid1, r1.clone(), pid2, r2.clone())
                    .display::<Dp>(package_store)
                    .to_string()
            }
            &[(pid1, _, Term::Negative(r1)), (pid2, _, Term::Positive(r2))] => {
                External::FromDependencyOf(pid2, r2.clone(), pid1, r1.clone())
                    .display::<Dp>(package_store)
                    .to_string()
            }
            slice => {
                let str_terms: Vec<_> = slice.iter().map(|(_, p, t)| format!("{p} {t}")).collect();
                str_terms.join(", ") + " are incompatible"
            }
        }
    }

    fn format_external(
        &self,
        external: &External<Ranges<SemanticVersion>, &'static str>,
        package_store: &Store,
    ) -> String {
        match external {
            External::NotRoot(package_id, version) => {
                let package = package_store.pkg(*package_id).unwrap();
                format!("we are solving dependencies of {package} {version}")
            }
            External::NoVersions(package_id, set) => {
                let package = package_store.pkg(*package_id).unwrap();
                if set == &Ranges::full() {
                    format!("there is no available version for {package}")
                } else {
                    format!("there is no version of {package} in {set}")
                }
            }
            External::Custom(package_id, set, reason) => {
                let package = package_store.pkg(*package_id).unwrap();
                if set == &Ranges::full() {
                    format!("dependencies of {package} are unavailable because {reason}")
                } else {
                    format!("dependencies of {package} at version {set} are unavailable because {reason}")
                }
            }
            External::FromDependencyOf(package_id, package_set, dep_id, dep_set) => {
                let package = package_store.pkg(*package_id).unwrap();
                let dependency = package_store.pkg(*dep_id).unwrap();
                if package_set == &Ranges::full() && dep_set == &Ranges::full() {
                    format!("{package} depends on {dependency}")
                } else if package_set == &Ranges::full() {
                    format!("{package} depends on {dependency} {dep_set}")
                } else if dep_set == &Ranges::full() {
                    if matches!(package, CustomPackage::Root) {
                        // Exclude the dummy version for root packages
                        format!("{package} depends on {dependency}")
                    } else {
                        format!("{package} {package_set} depends on {dependency}")
                    }
                } else if matches!(package, CustomPackage::Root) {
                    // Exclude the dummy version for root packages
                    format!("{package} depends on {dependency} {dep_set}")
                } else {
                    format!("{package} {package_set} depends on {dependency} {dep_set}")
                }
            }
        }
    }

    /// Simplest case, we just combine two external incompatibilities.
    fn explain_both_external(
        &self,
        external1: &External<Ranges<SemanticVersion>, &'static str>,
        external2: &External<Ranges<SemanticVersion>, &'static str>,
        current_terms: &Map<PackageId, Term<Ranges<SemanticVersion>>>,
        package_store: &Store,
    ) -> String {
        // TODO: order should be chosen to make it more logical.
        format!(
            "Because {} and {}, {}.",
            self.format_external(external1, package_store),
            self.format_external(external2, package_store),
            self.format_terms(current_terms, package_store)
        )
    }

    /// Both causes have already been explained so we use their refs.
    fn explain_both_ref(
        &self,
        ref_id1: usize,
        derived1: &Derived<Ranges<SemanticVersion>, &'static str>,
        ref_id2: usize,
        derived2: &Derived<Ranges<SemanticVersion>, &'static str>,
        current_terms: &Map<PackageId, Term<Ranges<SemanticVersion>>>,
        package_store: &Store,
    ) -> String {
        // TODO: order should be chosen to make it more logical.
        format!(
            "Because {} ({}) and {} ({}), {}.",
            self.format_terms(&derived1.terms, package_store),
            ref_id1,
            self.format_terms(&derived2.terms, package_store),
            ref_id2,
            self.format_terms(current_terms, package_store)
        )
    }

    /// One cause is derived (already explained so one-line),
    /// the other is a one-line external cause,
    /// and finally we conclude with the current incompatibility.
    fn explain_ref_and_external(
        &self,
        ref_id: usize,
        derived: &Derived<Ranges<SemanticVersion>, &'static str>,
        external: &External<Ranges<SemanticVersion>, &'static str>,
        current_terms: &Map<PackageId, Term<Ranges<SemanticVersion>>>,
        package_store: &Store,
    ) -> String {
        // TODO: order should be chosen to make it more logical.
        format!(
            "Because {} ({}) and {}, {}.",
            self.format_terms(&derived.terms, package_store),
            ref_id,
            self.format_external(external, package_store),
            self.format_terms(current_terms, package_store)
        )
    }

    /// Add an external cause to the chain of explanations.
    fn and_explain_external(
        &self,
        external: &External<Ranges<SemanticVersion>, &'static str>,
        current_terms: &Map<PackageId, Term<Ranges<SemanticVersion>>>,
        package_store: &Store,
    ) -> String {
        format!(
            "And because {}, {}.",
            self.format_external(external, package_store),
            self.format_terms(current_terms, package_store)
        )
    }

    /// Add an already explained incompat to the chain of explanations.
    fn and_explain_ref(
        &self,
        ref_id: usize,
        derived: &Derived<Ranges<SemanticVersion>, &'static str>,
        current_terms: &Map<PackageId, Term<Ranges<SemanticVersion>>>,
        package_store: &Store,
    ) -> String {
        format!(
            "And because {} ({}), {}.",
            self.format_terms(&derived.terms, package_store),
            ref_id,
            self.format_terms(current_terms, package_store)
        )
    }

    /// Add an already explained incompat to the chain of explanations.
    fn and_explain_prior_and_external(
        &self,
        prior_external: &External<Ranges<SemanticVersion>, &'static str>,
        external: &External<Ranges<SemanticVersion>, &'static str>,
        current_terms: &Map<PackageId, Term<Ranges<SemanticVersion>>>,
        package_store: &Store,
    ) -> String {
        format!(
            "And because {} and {}, {}.",
            self.format_external(prior_external, package_store),
            self.format_external(external, package_store),
            self.format_terms(current_terms, package_store)
        )
    }
}

fn main() {
    let mut dependency_provider =
        OfflineDependencyProvider::<CustomPackage, Ranges<SemanticVersion>>::new();
    // Define the root package with a dependency on a package we do not provide
    dependency_provider.add_dependencies(
        CustomPackage::Root,
        (0, 0, 0),
        vec![(
            CustomPackage::Package("foo".to_string()),
            Ranges::singleton((1, 0, 0)),
        )],
    );

    // Run the algorithm
    match resolve(&mut dependency_provider, CustomPackage::Root, (0, 0, 0)) {
        Ok(sol) => println!("{:?}", sol),
        Err(PubGrubError::NoSolution(error)) => {
            eprintln!("No solution.\n");

            eprintln!("### Default report:");
            eprintln!("```");
            eprintln!("{}", DefaultStringReporter::report(&error));
            eprintln!("```\n");

            eprintln!("### Report with custom formatter:");
            eprintln!("```");
            eprintln!(
                "{}",
                DefaultStringReporter::report_with_formatter(&error, &CustomReportFormatter)
            );
            eprintln!("```");
            std::process::exit(1);
        }
        Err(err) => panic!("{:?}", err),
    };
}
