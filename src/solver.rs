// SPDX-License-Identifier: MPL-2.0

//! PubGrub version solving algorithm.
//!
//! It consists in efficiently finding a set of packages and versions
//! that satisfy all the constraints of a given project dependencies.
//! In addition, when that is not possible,
//! PubGrub tries to provide a very human-readable and clear
//! explanation as to why that failed.
//! Below is an example of explanation present in
//! the introductory blog post about PubGrub
//!
//! ```txt
//! Because dropdown >=2.0.0 depends on icons >=2.0.0 and
//!   root depends on icons <2.0.0, dropdown >=2.0.0 is forbidden.
//!
//! And because menu >=1.1.0 depends on dropdown >=2.0.0,
//!   menu >=1.1.0 is forbidden.
//!
//! And because menu <1.1.0 depends on dropdown >=1.0.0 <2.0.0
//!   which depends on intl <4.0.0, every version of menu
//!   requires intl <4.0.0.
//!
//! So, because root depends on both menu >=1.0.0 and intl >=5.0.0,
//!   version solving failed.
//! ```
//!
//! The algorithm is generic and works for any type of dependency system
//! as long as packages (P) and versions (V) implement
//! the [Package] and Version traits.
//! [Package] is strictly equivalent and automatically generated
//! for any type that implement [Clone] + [Eq] + [Hash] + [Debug] + [Display].
//!
//! ## API
//!
//! ```
//! # use std::convert::Infallible;
//! # use pubgrub::{resolve, OfflineDependencyProvider, PubGrubError, Range};
//! #
//! # type NumVS = Range<u32>;
//! #
//! # fn try_main() -> Result<(), PubGrubError<OfflineDependencyProvider<&'static str, NumVS>>> {
//! #     let mut dependency_provider = OfflineDependencyProvider::<&str, NumVS>::new();
//! #     let package = "root";
//! #     let version = 1u32;
//! let solution = dependency_provider.resolve(&package, version)?;
//! #     Ok(())
//! # }
//! # fn main() {
//! #     assert!(matches!(try_main(), Err(PubGrubError::NoRoot)));
//! # }
//! ```
//!
//! Where `dependency_provider` supplies the list of available packages and versions,
//! as well as the dependencies of every available package
//! by implementing the [DependencyProvider] trait.
//! The call to [resolve] for a given package at a given version
//! will compute the set of packages and versions needed
//! to satisfy the dependencies of that package and version pair.
//! If there is no solution, the reason will be provided as clear as possible.

use std::collections::BTreeSet;
use std::error::Error;
use std::fmt::{Debug, Display};
use std::hash::Hash;

use log::{debug, info};

use crate::Version;
use crate::{
    internal::{Incompatibility, State},
    DependencyConstraints, Map, Package, PubGrubError, SelectedDependencies, VersionSet,
};

/// Main function of the library.
/// Finds a set of packages satisfying dependency bounds for a given package + version pair.
pub fn resolve<DP: DependencyProvider>(
    dependency_provider: &mut DP,
    package_name: &DP::P,
    version: Version,
) -> Result<SelectedDependencies<DP>, PubGrubError<DP>> {
    let Some(package) = dependency_provider.name_to_package(package_name) else {
        return Err(PubGrubError::NoRoot);
    };
    let mut state: State<DP> = State::init(package, version);
    let mut added_dependencies: Map<Package, BTreeSet<Version>> = Map::default();
    let mut next = package;
    loop {
        dependency_provider
            .should_cancel()
            .map_err(PubGrubError::ErrorInShouldCancel)?;

        info!(
            "unit_propagation: {}",
            dependency_provider.package_to_name(next).unwrap()
        );
        state.unit_propagation(next, dependency_provider)?;

        debug!(
            "Partial solution after unit propagation: {}",
            state.partial_solution.display(dependency_provider)
        );

        let Some(highest_priority_pkg) = state
            .partial_solution
            .pick_highest_priority_pkg(|p, r| dependency_provider.prioritize(p, r))
        else {
            return Ok(state.partial_solution.extract_solution(dependency_provider));
        };
        next = highest_priority_pkg;

        let term_intersection = state
            .partial_solution
            .term_intersection_for_package(next)
            .ok_or_else(|| {
                PubGrubError::Failure("a package was chosen but we don't have a term.".into())
            })?;
        let decision = dependency_provider
            .choose_version(next, term_intersection.unwrap_positive())
            .map_err(PubGrubError::ErrorChoosingPackageVersion)?;
        info!(
            "DP chose: {} @ {:?}",
            dependency_provider.package_to_name(next).unwrap(),
            decision
        );

        // Pick the next compatible version.
        let v = match decision {
            None => {
                let inc = Incompatibility::no_versions(next, term_intersection);
                state.add_incompatibility(inc);
                continue;
            }
            Some(x) => x,
        };

        if !term_intersection.contains(v) {
            return Err(PubGrubError::Failure(
                "choose_package_version picked an incompatible version".into(),
            ));
        }

        let is_new_dependency = added_dependencies.entry(next).or_default().insert(v);

        if is_new_dependency {
            // Retrieve that package dependencies.
            let p = next;
            let dependencies = dependency_provider.get_dependencies(p, v).map_err(|err| {
                PubGrubError::ErrorRetrievingDependencies {
                    package_version: dependency_provider.package_version_repr(p, v),
                    source: err,
                }
            })?;

            let dependencies = match dependencies {
                Dependencies::Unavailable(reason) => {
                    state.add_incompatibility(Incompatibility::custom_version(p, v, reason));
                    continue;
                }
                Dependencies::Available(x) => x,
            };

            // Add that package and version if the dependencies are not problematic.
            let dep_incompats = state.add_incompatibility_from_dependencies(p, v, dependencies);

            state.partial_solution.add_version(
                p,
                v,
                dep_incompats,
                &state.incompatibility_store,
                dependency_provider,
            );
        } else {
            // `dep_incompats` are already in `incompatibilities` so we know there are not satisfied
            // terms and can add the decision directly.
            info!(
                "add_decision (not first time): {}",
                dependency_provider.package_version_repr(package, v)
            );
            state.partial_solution.add_decision(next, v);
        }
    }
}

/// An enum used by [DependencyProvider] that holds information about package dependencies.
/// For each [Package] there is a set of versions allowed as a dependency.
#[derive(Clone)]
pub enum Dependencies<M: Clone + Debug + Display> {
    /// Package dependencies are unavailable with the reason why they are missing.
    Unavailable(M),
    /// Container for all available package versions.
    Available(DependencyConstraints),
}

/// Trait that allows the algorithm to retrieve available packages and their dependencies.
/// An implementor needs to be supplied to the [resolve] function.
pub trait DependencyProvider {
    /// How this provider stores the name of the packages.
    type P: Debug + Display + Clone + Eq + Hash;

    /// Representation of a package version.
    type PV: Debug + Display;

    /// Representation of a package version set.
    type PVS: Debug + Display;

    /// Type for custom incompatibilities.
    ///
    /// There are reasons in user code outside pubgrub that can cause packages or versions
    /// to be unavailable. Examples:
    /// * The version would require building the package, but builds are disabled.
    /// * The package is not available in the cache, but internet access has been disabled.
    /// * The package uses a legacy format not supported anymore.
    ///
    /// The intended use is to track them in an enum and assign them to this type. You can also
    /// assign [`String`] as placeholder.
    type M: Clone + Debug + Display;

    /// [Decision making](https://github.com/dart-lang/pub/blob/master/doc/solver.md#decision-making)
    /// is the process of choosing the next package
    /// and version that will be appended to the partial solution.
    ///
    /// Every time such a decision must be made, the resolver looks at all the potential valid
    /// packages that have changed, and a asks the dependency provider how important each one is.
    /// For each one it calls `prioritize` with the name of the package and the current set of
    /// acceptable versions.
    /// The resolver will then pick the package with the highes priority from all the potential valid
    /// packages.
    ///
    /// The strategy employed to prioritize packages
    /// cannot change the existence of a solution or not,
    /// but can drastically change the performances of the solver,
    /// or the properties of the solution.
    /// The documentation of Pub (PubGrub implementation for the dart programming language)
    /// states the following:
    ///
    /// > Pub chooses the latest matching version of the package
    /// > with the fewest versions that match the outstanding constraint.
    /// > This tends to find conflicts earlier if any exist,
    /// > since these packages will run out of versions to try more quickly.
    /// > But there's likely room for improvement in these heuristics.
    ///
    /// Note: the resolver may call this even when the range has not changed,
    /// if it is more efficient for the resolvers internal data structures.
    fn prioritize(&mut self, package: Package, range: VersionSet) -> Self::Priority;
    /// The type returned from `prioritize`. The resolver does not care what type this is
    /// as long as it can pick a largest one and clone it.
    ///
    /// [Reverse] can be useful if you want to pick the package with
    /// the fewest versions that match the outstanding constraint.
    type Priority: Ord + Clone;

    /// The kind of error returned from these methods.
    ///
    /// Returning this signals that resolution should fail with this error.
    type Err: Error + 'static;

    /// Once the resolver has found the highest `Priority` package from all potential valid
    /// packages, it needs to know what version of that package to use. The most common pattern
    /// is to select the largest version that the range contains.
    fn choose_version(
        &mut self,
        package: Package,
        range: VersionSet,
    ) -> Result<Option<Version>, Self::Err>;

    /// Retrieves the package dependencies.
    /// Return [Dependencies::Unavailable] if its dependencies are unavailable.
    #[allow(clippy::type_complexity)]
    fn get_dependencies(
        &mut self,
        package: Package,
        version: Version,
    ) -> Result<Dependencies<Self::M>, Self::Err>;

    /// This is called fairly regularly during the resolution,
    /// if it returns an Err then resolution will be terminated.
    /// This is helpful if you want to add some form of early termination like a timeout,
    /// or you want to add some form of user feedback if things are taking a while.
    /// If not provided the resolver will run as long as needed.
    fn should_cancel(&mut self) -> Result<(), Self::Err> {
        Ok(())
    }

    /// Get the name of a package.
    fn package_to_name(&self, package: Package) -> Option<&Self::P>;

    /// Get the package correponding to a name.
    fn name_to_package(&self, package_name: &Self::P) -> Option<Package>;

    /// Get a representation of a package version.
    fn package_version_repr(&self, package: Package, version: Version) -> Self::PV;

    /// Get a representation of a package version set.
    fn package_version_set_repr(&self, package: Package, version_set: VersionSet) -> Self::PVS;
}
