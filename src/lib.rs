// SPDX-License-Identifier: MPL-2.0

//! PubGrub version solving algorithm.
//!
//! Version solving consists in efficiently finding a set of packages and versions
//! that satisfy all the constraints of a given project dependencies.
//! In addition, when that is not possible,
//! we should try to provide a very human-readable and clear
//! explanation as to why that failed.
//!
//! # Package and Version traits
//!
//! All the code in this crate is manipulating packages and versions, and for this to work
//! we defined a [Package] trait
//! that is used as bounds on most of the exposed types and functions.
//!
//! Package identifiers needs to implement our [Package] trait,
//! which is automatic if the type already implements
//! [Clone] + [Eq] + [Hash] + [Debug] + [Display](std::fmt::Display).
//! So things like [String] will work out of the box.
//!
//! TODO! This is all wrong. Need to talk about VS, not Version.
//! Our Version trait requires
//! [Clone] + [Ord] + [Debug] + [Display](std::fmt::Display).
//! For convenience, this library provides [SemanticVersion]
//! that implements semantic versioning rules.
//!
//! # Basic example
//!
//! Let's imagine that we are building a user interface
//! with a menu containing dropdowns with some icons,
//! icons that we are also directly using in other parts of the interface.
//! For this scenario our direct dependencies are `menu` and `icons`,
//! but the complete set of dependencies looks like follows:
//!
//! - `root` depends on `menu` and `icons`
//! - `menu` depends on `dropdown`
//! - `dropdown` depends on `icons`
//! - `icons` has no dependency
//!
//! We can model that scenario with this library as follows
//! ```
//! # use pubgrub::{OfflineDependencyProvider, resolve, Range};
//!
//! type NumVS = Range<u32>;
//!
//! let mut dependency_provider = OfflineDependencyProvider::<&str, NumVS>::new();
//!
//! dependency_provider.add_dependencies(
//!     "root",
//!     1u32,
//!     [("menu", Range::full()), ("icons", Range::full())],
//! );
//! dependency_provider.add_dependencies("menu", 1u32, [("dropdown", Range::full())]);
//! dependency_provider.add_dependencies("dropdown", 1u32, [("icons", Range::full())]);
//! dependency_provider.add_dependencies("icons", 1u32, []);
//!
//! // Run the algorithm.
//! let solution = dependency_provider.resolve(&"root", 1u32).unwrap();
//! ```
//!
//! # DependencyProvider trait
//!
//! In our previous example we used the
//! [OfflineDependencyProvider],
//! which is a basic implementation of the [DependencyProvider] trait.
//!
//! But we might want to implement the [DependencyProvider]
//! trait for our own type.
//! Let's say that we will use [String] for packages,
//! and [SemanticVersion] for versions.
//! This may be done quite easily by implementing the following functions.
//! ```
//! # use pubgrub::{DependencyProvider, Dependencies, Range, DependencyConstraints, Map, Package, Version, VersionSet};
//! # use std::error::Error;
//! # use std::borrow::Borrow;
//! # use std::convert::Infallible;
//! #
//! # struct MyDependencyProvider;
//! #
//! impl DependencyProvider for MyDependencyProvider {
//!     fn choose_version(&mut self, package: Package, range: VersionSet) -> Result<Option<Version>, Infallible> {
//!         unimplemented!()
//!     }
//!
//!     type Priority = usize;
//!     fn prioritize(&mut self, package: Package, range: VersionSet) -> Self::Priority {
//!         unimplemented!()
//!     }
//!
//!     fn get_dependencies(
//!         &mut self,
//!         package: Package,
//!         version: Version,
//!     ) -> Result<Dependencies<Self::M>, Infallible> {
//!         Ok(Dependencies::Available(DependencyConstraints::default()))
//!     }
//!
//!     type Err = Infallible;
//!     type P = String;
//!     type PV = String;
//!     type PVS = String;
//!     type M = String;
//!
//!     fn package_to_name(&self, package: Package) -> Option<&Self::P> {
//!         unimplemented!()
//!     }
//!
//!     fn name_to_package(&self, package_name: &Self::P) -> Option<Package> {
//!         unimplemented!()
//!     }
//!
//!     fn package_version_repr(&self, package: Package, version: Version) -> Self::PV {
//!         unimplemented!()
//!     }
//!
//!     fn package_version_set_repr(&self, package: Package, version_set: VersionSet) -> Self::PVS {
//!         unimplemented!()
//!     }
//! }
//! ```
//!
//! The first method
//! [choose_version](DependencyProvider::choose_version)
//! chooses a version compatible with the provided range for a package.
//! The second method
//! [prioritize](DependencyProvider::prioritize)
//! in which order different packages should be chosen.
//! Usually prioritizing packages
//! with the fewest number of compatible versions speeds up resolution.
//! But in general you are free to employ whatever strategy suits you best
//! to pick a package and a version.
//!
//! The third method [get_dependencies](DependencyProvider::get_dependencies)
//! aims at retrieving the dependencies of a given package at a given version.
//!
//! In a real scenario, these two methods may involve reading the file system
//! or doing network request, so you may want to hold a cache in your
//! [DependencyProvider] implementation.
//! How exactly this could be achieved is shown in `CachingDependencyProvider`
//! (see `examples/caching_dependency_provider.rs`).
//! You could also use the [OfflineDependencyProvider]
//! type defined by the crate as guidance,
//! but you are free to use whatever approach makes sense in your situation.
//!
//! # Solution and error reporting
//!
//! When everything goes well, the algorithm finds and returns the complete
//! set of direct and indirect dependencies satisfying all the constraints.
//! The packages and versions selected are returned as
//! [SelectedDependencies<P, V>](SelectedDependencies).
//! But sometimes there is no solution because dependencies are incompatible.
//! In such cases, [resolve(...)](resolve) returns a
//! [PubGrubError::NoSolution(derivation_tree)](PubGrubError::NoSolution),
//! where the provided derivation tree is a custom binary tree
//! containing the full chain of reasons why there is no solution.
//!
//! All the items in the tree are called incompatibilities
//! and may be of two types, either "external" or "derived".
//! Leaves of the tree are external incompatibilities,
//! and nodes are derived.
//! External incompatibilities have reasons that are independent
//! of the way this algorithm is implemented such as
//!  - dependencies: "package_a" at version 1 depends on "package_b" at version 4
//!  - missing dependencies: dependencies of "package_a" are unavailable
//!  - absence of version: there is no version of "package_a" in the range [3.1.0  4.0.0[
//!
//! Derived incompatibilities are obtained during the algorithm execution by deduction,
//! such as if "a" depends on "b" and "b" depends on "c", "a" depends on "c".
//!
//! This crate defines the following [Reporter] trait:
//! ```
//! # use pubgrub::{Package, DerivationTree, DependencyProvider, ReportFormatter};
//! # use std::fmt::{Debug, Display};
//! #
//! pub trait Reporter<DP: DependencyProvider, M: Clone + Debug + Display> {
//!     /// Output type of the report.
//!     type Output;
//!
//!     /// Generate a report from the derivation tree
//!     /// describing the resolution failure using the default formatter.
//!     fn report(derivation_tree: &DerivationTree<M>, dependency_provider: &DP) -> Self::Output;
//!
//!     /// Generate a report from the derivation tree
//!     /// describing the resolution failure using a custom formatter.
//!     fn report_with_formatter(
//!         derivation_tree: &DerivationTree<M>,
//!         formatter: &impl ReportFormatter<DP, M, Output = Self::Output>,
//!         dependency_provider: &DP,
//!     ) -> Self::Output;
//! }
//! ```
//! Implementing a [Reporter] may involve a lot of heuristics
//! to make the output human-readable and natural.
//! For convenience, we provide a default implementation
//! [DefaultStringReporter] that outputs the report as a [String].
//! You may use it as follows:
//! ```
//! # use pubgrub::{resolve, OfflineDependencyProvider, DefaultStringReporter, Reporter, PubGrubError, Range};
//! #
//! # type NumVS = Range<u32>;
//! #
//! # let mut dependency_provider = OfflineDependencyProvider::<&str, NumVS>::new();
//! # dependency_provider.add_dependencies("root", 1u32, []);
//! # let root_package = "root";
//! # let root_version = 1u32;
//! #
//! match dependency_provider.resolve(&root_package, root_version) {
//!     Ok(solution) => println!("{:?}", solution),
//!     Err(PubGrubError::NoSolution(mut derivation_tree)) => {
//!         derivation_tree.collapse_no_versions();
//!         eprintln!("{}", DefaultStringReporter::report(&derivation_tree, &dependency_provider));
//!     }
//!     Err(err) => panic!("{:?}", err),
//! };
//! ```
//! Notice that we also used
//! [collapse_no_versions()](DerivationTree::collapse_no_versions) above.
//! This method simplifies the derivation tree to get rid of the
//! [NoVersions](External::NoVersions)
//! external incompatibilities in the derivation tree.
//! So instead of seeing things like this in the report:
//! ```txt
//! Because there is no version of foo in 1.0.1 <= v < 2.0.0
//! and foo 1.0.0 depends on bar 2.0.0 <= v < 3.0.0,
//! foo 1.0.0 <= v < 2.0.0 depends on bar 2.0.0 <= v < 3.0.0.
//! ```
//! you may have directly:
//! ```txt
//! foo 1.0.0 <= v < 2.0.0 depends on bar 2.0.0 <= v < 3.0.0.
//! ```
//! Beware though that if you are using some kind of offline mode
//! with a cache, you may want to know that some versions
//! do not exist in your cache.

#![warn(missing_docs)]

mod error;
mod provider;
mod range;
mod report;
mod semantic;
mod solver;
mod term;
mod type_aliases;
mod version;

pub use error::{NoSolutionError, PubGrubError};
pub use provider::{OfflineDependencyProvider, VersionRange};
pub use range::Range;
pub use report::{
    DefaultStringReportFormatter, DefaultStringReporter, DerivationTree, Derived, External,
    ReportFormatter, Reporter,
};
pub use semantic::SemanticVersion;
pub use solver::{resolve, Dependencies, DependencyProvider};
pub use term::Term;
pub use type_aliases::{
    DependencyConstraints, FnvIndexMap, FnvIndexSet, Map, Package, SelectedDependencies, Set,
};
pub use version::{Version, VersionSet};

mod internal;
