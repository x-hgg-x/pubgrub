// SPDX-License-Identifier: MPL-2.0

use std::fmt::{self, Debug, Display};
use std::hash::Hash;

use pubgrub::{
    resolve, DefaultStringReporter, DependencyProvider, Derived, External, Map,
    OfflineDependencyProvider, Package, PubGrubError, Range, ReportFormatter, Reporter,
    SemanticVersion, Term,
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

impl ReportFormatter<Dp, Range<SemanticVersion>, String> for CustomReportFormatter {
    type Output = String;

    fn format_terms(&self, terms: &Map<Package, Term<Range<SemanticVersion>>>, dp: &Dp) -> String {
        let terms_vec: Vec<_> = terms
            .iter()
            .map(|(&p, v)| (p, dp.package_to_name(p).unwrap(), v))
            .collect();
        match terms_vec.as_slice() {
            [] => "version solving failed".into(),
            [(_, package @ CustomPackage::Root, Term::Positive(_))] => {
                format!("{package} is forbidden")
            }
            [(_, package @ CustomPackage::Root, Term::Negative(_))] => {
                format!("{package} is mandatory")
            }
            [(_, package @ CustomPackage::Package(_), Term::Positive(range))] => {
                format!("{package} {range} is forbidden")
            }
            [(_, package @ CustomPackage::Package(_), Term::Negative(range))] => {
                format!("{package} {range} is mandatory")
            }
            &[(p1, _, Term::Positive(r1)), (p2, _, Term::Negative(r2))] => {
                External::<_, String>::FromDependencyOf(p1, r1.clone(), p2, r2.clone())
                    .display(dp)
                    .to_string()
            }
            &[(p1, _, Term::Negative(r1)), (p2, _, Term::Positive(r2))] => {
                External::<_, String>::FromDependencyOf(p2, r2.clone(), p1, r1.clone())
                    .display(dp)
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
        external: &External<Range<SemanticVersion>, String>,
        dp: &Dp,
    ) -> String {
        match external {
            External::NotRoot(package, version) => {
                let package = dp.package_to_name(*package).unwrap();
                format!("we are solving dependencies of {package} {version}")
            }
            External::NoVersions(package, set) => {
                let package = dp.package_to_name(*package).unwrap();
                if set == &Range::full() {
                    format!("there is no available version for {package}")
                } else {
                    format!("there is no version of {package} in {set}")
                }
            }
            External::Custom(package, set, reason) => {
                let package = dp.package_to_name(*package).unwrap();
                if set == &Range::full() {
                    format!("dependencies of {package} are unavailable because {reason}")
                } else {
                    format!("dependencies of {package} at version {set} are unavailable because {reason}")
                }
            }
            External::FromDependencyOf(package, package_set, dependency, dependency_set) => {
                let package = dp.package_to_name(*package).unwrap();
                let dependency = dp.package_to_name(*dependency).unwrap();
                if package_set == &Range::full() && dependency_set == &Range::full() {
                    format!("{package} depends on {dependency}")
                } else if package_set == &Range::full() {
                    format!("{package} depends on {dependency} {dependency_set}")
                } else if dependency_set == &Range::full() {
                    if matches!(package, CustomPackage::Root) {
                        // Exclude the dummy version for root packages
                        format!("{package} depends on {dependency}")
                    } else {
                        format!("{package} {package_set} depends on {dependency}")
                    }
                } else if matches!(package, CustomPackage::Root) {
                    // Exclude the dummy version for root packages
                    format!("{package} depends on {dependency} {dependency_set}")
                } else {
                    format!("{package} {package_set} depends on {dependency} {dependency_set}")
                }
            }
        }
    }

    /// Simplest case, we just combine two external incompatibilities.
    fn explain_both_external(
        &self,
        external1: &External<Range<SemanticVersion>, String>,
        external2: &External<Range<SemanticVersion>, String>,
        current_terms: &Map<Package, Term<Range<SemanticVersion>>>,
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
        derived1: &Derived<Range<SemanticVersion>, String>,
        ref_id2: usize,
        derived2: &Derived<Range<SemanticVersion>, String>,
        current_terms: &Map<Package, Term<Range<SemanticVersion>>>,
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
        derived: &Derived<Range<SemanticVersion>, String>,
        external: &External<Range<SemanticVersion>, String>,
        current_terms: &Map<Package, Term<Range<SemanticVersion>>>,
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
        external: &External<Range<SemanticVersion>, String>,
        current_terms: &Map<Package, Term<Range<SemanticVersion>>>,
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
        derived: &Derived<Range<SemanticVersion>, String>,
        current_terms: &Map<Package, Term<Range<SemanticVersion>>>,
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
        prior_external: &External<Range<SemanticVersion>, String>,
        external: &External<Range<SemanticVersion>, String>,
        current_terms: &Map<Package, Term<Range<SemanticVersion>>>,
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
    match resolve(&mut dependency_provider, &CustomPackage::Root, (0, 0, 0)) {
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
