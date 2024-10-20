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
//! let solution = resolve(&mut dependency_provider, &package, version)?;
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

use std::cmp::Reverse;
use std::collections::{BTreeMap, BTreeSet};
use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Debug, Display};
use std::hash::Hash;

use log::{debug, info};

use crate::{
    internal::{Incompatibility, State},
    DependencyConstraints, FnvIndexSet, Map, Package, PubGrubError, SelectedDependencies,
    VersionSet,
};

/// Main function of the library.
/// Finds a set of packages satisfying dependency bounds for a given package + version pair.
pub fn resolve<DP: DependencyProvider>(
    dependency_provider: &mut DP,
    package_name: &DP::P,
    version: impl Into<DP::V>,
) -> Result<SelectedDependencies<DP>, PubGrubError<DP>> {
    let Some(package) = dependency_provider.name_to_package(package_name) else {
        return Err(PubGrubError::NoRoot);
    };
    let mut state: State<DP> = State::init(package, version.into());
    let mut added_dependencies: Map<Package, BTreeSet<DP::V>> = Map::default();
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
                let inc = Incompatibility::no_versions(next, term_intersection.clone());
                state.add_incompatibility(inc);
                continue;
            }
            Some(x) => x,
        };

        if !term_intersection.contains(&v) {
            return Err(PubGrubError::Failure(
                "choose_package_version picked an incompatible version".into(),
            ));
        }

        let is_new_dependency = added_dependencies
            .entry(next)
            .or_default()
            .insert(v.clone());

        if is_new_dependency {
            // Retrieve that package dependencies.
            let p = next;
            let dependencies = dependency_provider.get_dependencies(p, &v).map_err(|err| {
                PubGrubError::ErrorRetrievingDependencies {
                    package: dependency_provider.package_to_name(p).unwrap().clone(),
                    version: v.clone(),
                    source: err,
                }
            })?;

            let dependencies = match dependencies {
                Dependencies::Unavailable(reason) => {
                    state.add_incompatibility(Incompatibility::custom_version(
                        p,
                        v.clone(),
                        reason,
                    ));
                    continue;
                }
                Dependencies::Available(x) => x,
            };

            // Add that package and version if the dependencies are not problematic.
            let dep_incompats =
                state.add_incompatibility_from_dependencies(p, v.clone(), dependencies);

            state.partial_solution.add_version(
                p,
                v.clone(),
                dep_incompats,
                &state.incompatibility_store,
                dependency_provider,
            );
        } else {
            // `dep_incompats` are already in `incompatibilities` so we know there are not satisfied
            // terms and can add the decision directly.
            info!(
                "add_decision (not first time): {} @ {}",
                dependency_provider.package_to_name(next).unwrap(),
                v
            );
            state
                .partial_solution
                .add_decision(next, v, dependency_provider);
        }
    }
}

/// An enum used by [DependencyProvider] that holds information about package dependencies.
/// For each [Package] there is a set of versions allowed as a dependency.
#[derive(Clone)]
pub enum Dependencies<VS: VersionSet, M: Clone + Debug + Display> {
    /// Package dependencies are unavailable with the reason why they are missing.
    Unavailable(M),
    /// Container for all available package versions.
    Available(DependencyConstraints<VS>),
}

/// Trait that allows the algorithm to retrieve available packages and their dependencies.
/// An implementor needs to be supplied to the [resolve] function.
pub trait DependencyProvider {
    /// How this provider stores the name of the packages.
    type P: Debug + Display + Clone + Eq + Hash;

    /// How this provider stores the versions of the packages.
    ///
    /// A common choice is [`SemanticVersion`][crate::version::SemanticVersion].
    type V: Debug + Display + Clone + Ord;

    /// How this provider stores the version requirements for the packages.
    /// The requirements must be able to process the same kind of version as this dependency provider.
    ///
    /// A common choice is [`Range`][crate::range::Range].
    type VS: VersionSet<V = Self::V>;

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
    fn prioritize(&mut self, package: Package, range: &Self::VS) -> Self::Priority;
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
        range: &Self::VS,
    ) -> Result<Option<Self::V>, Self::Err>;

    /// Retrieves the package dependencies.
    /// Return [Dependencies::Unavailable] if its dependencies are unavailable.
    #[allow(clippy::type_complexity)]
    fn get_dependencies(
        &mut self,
        package: Package,
        version: &Self::V,
    ) -> Result<Dependencies<Self::VS, Self::M>, Self::Err>;

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
}

/// A basic implementation of [DependencyProvider].
#[derive(Debug, Clone, Default)]
pub struct OfflineDependencyProvider<P: Debug + Display + Clone + Eq + Hash, VS: VersionSet> {
    arena: FnvIndexSet<P>,
    dependencies: Map<Package, BTreeMap<VS::V, Map<Package, VS>>>,
}

#[cfg(feature = "serde")]
impl<P: Debug + Display + Clone + Eq + Hash, VS: VersionSet> serde::Serialize
    for OfflineDependencyProvider<P, VS>
where
    P: serde::Serialize,
    VS::V: serde::Serialize,
    VS: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let map = self
            .dependencies
            .iter()
            .map(|(p, bmap)| {
                (
                    &self.arena[p.0 as usize],
                    bmap.iter()
                        .map(|(v, hmap)| {
                            (
                                v,
                                hmap.iter()
                                    .map(|(dep, vs)| (&self.arena[dep.0 as usize], vs))
                                    .collect::<Map<_, _>>(),
                            )
                        })
                        .collect::<BTreeMap<_, _>>(),
                )
            })
            .collect::<Map<_, _>>();

        map.serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de, P: Debug + Display + Clone + Eq + Hash, VS: VersionSet> serde::Deserialize<'de>
    for OfflineDependencyProvider<P, VS>
where
    P: serde::Deserialize<'de>,
    VS::V: serde::Deserialize<'de>,
    VS: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let dependencies = <Map<P, BTreeMap<VS::V, Map<P, VS>>>>::deserialize(deserializer)?;

        let arena = dependencies
            .iter()
            .flat_map(|(p, bmap)| {
                [p].into_iter()
                    .chain(bmap.values().flat_map(|hmap| hmap.keys()))
            })
            .cloned()
            .collect::<FnvIndexSet<_>>();

        let dependencies = dependencies
            .into_iter()
            .map(|(p, bmap)| {
                (
                    Package(arena.get_index_of(&p).unwrap() as u32),
                    bmap.into_iter()
                        .map(|(v, hmap)| {
                            (
                                v,
                                hmap.into_iter()
                                    .map(|(dep, vs)| {
                                        (Package(arena.get_index_of(&dep).unwrap() as u32), vs)
                                    })
                                    .collect::<Map<_, _>>(),
                            )
                        })
                        .collect::<BTreeMap<_, _>>(),
                )
            })
            .collect::<Map<_, _>>();

        Ok(Self {
            arena,
            dependencies,
        })
    }
}

impl<P: Debug + Display + Clone + Eq + Hash, VS: VersionSet> OfflineDependencyProvider<P, VS> {
    /// Creates an empty OfflineDependencyProvider with no dependencies.
    pub fn new() -> Self {
        Self {
            arena: FnvIndexSet::default(),
            dependencies: Map::default(),
        }
    }

    /// Registers the dependencies of a package and version pair.
    /// Dependencies must be added with a single call to
    /// [add_dependencies](OfflineDependencyProvider::add_dependencies).
    /// All subsequent calls to
    /// [add_dependencies](OfflineDependencyProvider::add_dependencies) for a given
    /// package version pair will replace the dependencies by the new ones.
    ///
    /// The API does not allow to add dependencies one at a time to uphold an assumption that
    /// [OfflineDependencyProvider.get_dependencies(p, v)](OfflineDependencyProvider::get_dependencies)
    /// provides all dependencies of a given package (p) and version (v) pair.
    pub fn add_dependencies<I: IntoIterator<Item = (P, VS)>>(
        &mut self,
        package: P,
        version: impl Into<VS::V>,
        dependencies: I,
    ) {
        let package_deps = dependencies.into_iter().collect::<Vec<_>>();

        self.arena.extend(
            [package.clone()]
                .into_iter()
                .chain(package_deps.iter().map(|(p, _)| p.clone())),
        );

        *self
            .dependencies
            .entry(Package(self.arena.get_index_of(&package).unwrap() as u32))
            .or_default()
            .entry(version.into())
            .or_default() = package_deps
            .into_iter()
            .map(|(p, vs)| (Package(self.arena.get_index_of(&p).unwrap() as u32), vs))
            .collect();
    }

    /// Lists packages that have been saved.
    pub fn packages(&self) -> impl Iterator<Item = &P> {
        self.dependencies
            .keys()
            .map(|dep| &self.arena[dep.0 as usize])
    }

    /// Lists versions of saved packages in sorted order.
    /// Returns [None] if no information is available regarding that package.
    pub fn versions(&self, package: &P) -> Option<impl Iterator<Item = &VS::V>> {
        Some(
            self.dependencies
                .get(&Package(self.arena.get_index_of(package)? as u32))?
                .keys(),
        )
    }

    /// Lists dependencies of a given package and version.
    /// Returns [None] if no information is available regarding that package and version pair.
    fn dependencies(&self, package: Package, version: &VS::V) -> Option<DependencyConstraints<VS>> {
        Some(self.dependencies.get(&package)?.get(version)?.clone())
    }
}

/// An implementation of [DependencyProvider] that
/// contains all dependency information available in memory.
/// Currently packages are picked with the fewest versions contained in the constraints first.
/// But, that may change in new versions if better heuristics are found.
/// Versions are picked with the newest versions first.
impl<P: Debug + Display + Clone + Eq + Hash, VS: VersionSet> DependencyProvider
    for OfflineDependencyProvider<P, VS>
{
    type P = P;
    type V = VS::V;
    type VS = VS;
    type M = String;

    type Err = Infallible;

    fn choose_version(
        &mut self,
        package: Package,
        range: &VS,
    ) -> Result<Option<VS::V>, Infallible> {
        Ok(self
            .dependencies
            .get(&package)
            .and_then(|versions| versions.keys().rev().find(|v| range.contains(v)).cloned()))
    }

    type Priority = Reverse<usize>;
    fn prioritize(&mut self, package: Package, range: &VS) -> Self::Priority {
        Reverse(
            self.dependencies
                .get(&package)
                .map(|versions| versions.keys().filter(|v| range.contains(v)).count())
                .unwrap_or(0),
        )
    }

    fn get_dependencies(
        &mut self,
        package: Package,
        version: &VS::V,
    ) -> Result<Dependencies<VS, Self::M>, Infallible> {
        Ok(match self.dependencies(package, version) {
            None => {
                Dependencies::Unavailable("its dependencies could not be determined".to_string())
            }
            Some(dependencies) => Dependencies::Available(dependencies),
        })
    }

    fn package_to_name(&self, package: Package) -> Option<&Self::P> {
        self.arena.get_index(package.0 as usize)
    }

    fn name_to_package(&self, package_name: &Self::P) -> Option<Package> {
        Some(Package(self.arena.get_index_of(package_name)? as u32))
    }
}
