use std::cmp::Reverse;
use std::collections::BTreeMap;
use std::convert::Infallible;
use std::fmt::{Debug, Display};
use std::hash::Hash;

use crate::{Dependencies, DependencyProvider, Map, PackageArena, PackageId, VersionSet};

/// A basic implementation of [DependencyProvider].
#[derive(Debug, Clone, Default)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Serialize, serde::Deserialize),
    serde(
        transparent,
        bound(
            serialize = "P: serde::Serialize, VS::V: serde::Serialize, VS: serde::Serialize",
            deserialize = "P: serde::Deserialize<'de>, VS::V: serde::Deserialize<'de>, VS: serde::Deserialize<'de>",
        )
    )
)]
pub struct OfflineDependencyProvider<P: Debug + Display + Clone + Eq + Hash, VS: VersionSet> {
    dependencies: Map<P, BTreeMap<VS::V, Map<P, VS>>>,
}

impl<P: Debug + Display + Clone + Eq + Hash, VS: VersionSet> OfflineDependencyProvider<P, VS> {
    /// Creates an empty OfflineDependencyProvider with no dependencies.
    pub fn new() -> Self {
        Self {
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
        *self
            .dependencies
            .entry(package)
            .or_default()
            .entry(version.into())
            .or_default() = dependencies.into_iter().collect();
    }

    /// Lists packages that have been saved.
    pub fn packages(&self) -> impl Iterator<Item = &P> {
        self.dependencies.keys()
    }

    /// Lists versions of saved packages in sorted order.
    /// Returns [None] if no information is available regarding that package.
    pub fn versions(&self, p: &P) -> Option<impl Iterator<Item = &VS::V> + Clone> {
        Some(self.dependencies.get(p)?.keys())
    }

    /// Lists dependencies of a given package and version.
    pub fn dependencies(&self, p: &P, v: &VS::V) -> Option<&Map<P, VS>> {
        self.dependencies.get(p)?.get(v)
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
    type M = &'static str;

    type Err = Infallible;

    #[inline]
    fn choose_version(
        &mut self,
        package_id: PackageId,
        range: &VS,
        package_store: &PackageArena<Self::P>,
    ) -> Result<Option<VS::V>, Infallible> {
        Ok(package_store
            .pkg(package_id)
            .and_then(|p| self.dependencies.get(p))
            .and_then(|versions| versions.keys().rev().find(|v| range.contains(v)).cloned()))
    }

    type Priority = Reverse<u64>;

    #[inline]
    fn prioritize(
        &mut self,
        package_id: PackageId,
        range: &VS,
        package_store: &PackageArena<Self::P>,
    ) -> Self::Priority {
        let count = package_store
            .pkg(package_id)
            .and_then(|p| self.dependencies.get(p))
            .map(|versions| versions.keys().filter(|v| range.contains(v)).count())
            .unwrap_or(0);

        Reverse(((count as u64) << 32) + package_id.get() as u64)
    }

    #[inline]
    fn get_dependencies(
        &mut self,
        package_id: PackageId,
        version: &VS::V,
        package_store: &mut PackageArena<Self::P>,
    ) -> Result<Dependencies<VS, Self::M>, Infallible> {
        let msg = "dependencies could not be determined";

        let Some(deps) = self
            .dependencies
            .get(package_store.pkg(package_id).unwrap())
            .and_then(|d| d.get(version))
        else {
            return Ok(Dependencies::Unavailable(msg));
        };

        Ok(Dependencies::Available(
            deps.iter()
                .map(|(dep, r)| (package_store.insert(dep.clone()), r.clone()))
                .collect(),
        ))
    }
}
