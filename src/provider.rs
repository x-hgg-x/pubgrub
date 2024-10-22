use std::borrow::Borrow;
use std::cmp::Reverse;
use std::collections::BTreeMap;
use std::convert::Infallible;
use std::fmt::{Debug, Display};
use std::hash::Hash;

use crate::{
    resolve, Dependencies, DependencyConstraints, DependencyProvider, FnvIndexSet, Map, Package,
    PubGrubError, Range, SelectedDependencies, Version, VersionSet,
};

/// Version range.
pub trait VersionRange: Debug + Display {
    /// Associated version type.
    type V: Debug + Display + Clone + Ord;

    /// Returns true if this version range contains the specified values.
    fn contains_many<'s, I, BV>(&'s self, versions: I) -> impl Iterator<Item = bool> + 's
    where
        I: Iterator<Item = BV> + 's,
        BV: Borrow<Self::V> + 's;

    /// Returns a version range for the provided ordered versions.
    fn from_ordered_versions(versions: impl IntoIterator<Item = (Self::V, bool)> + Clone) -> Self;
}

impl<V: Debug + Display + Clone + Ord> VersionRange for Range<V> {
    type V = V;

    fn contains_many<'s, I, BV>(&'s self, versions: I) -> impl Iterator<Item = bool> + 's
    where
        I: Iterator<Item = BV> + 's,
        BV: Borrow<Self::V> + 's,
    {
        self.contains_many(versions)
    }

    fn from_ordered_versions(versions: impl IntoIterator<Item = (Self::V, bool)> + Clone) -> Self {
        let all_iter = versions.clone().into_iter();
        let versions = versions.into_iter();
        let mut range = Range::empty();
        for (v, ok) in versions {
            if ok {
                range = range.r#union(&Range::singleton(v));
            }
        }
        range.simplify(all_iter.map(|(v, _)| v))
    }
}

/// A basic implementation of [DependencyProvider].
#[derive(Debug, Clone, Default)]
pub struct OfflineDependencyProvider<P: Debug + Display + Clone + Eq + Hash, R: VersionRange> {
    arena: FnvIndexSet<P>,
    dependencies: Map<Package, BTreeMap<R::V, Map<Package, R>>>,
}

#[cfg(feature = "serde")]
impl<P: Debug + Display + Clone + Eq + Hash, R: VersionRange> serde::Serialize
    for OfflineDependencyProvider<P, R>
where
    P: serde::Serialize,
    R::V: serde::Serialize,
    R: serde::Serialize,
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
impl<'de, P: Debug + Display + Clone + Eq + Hash, R: VersionRange> serde::Deserialize<'de>
    for OfflineDependencyProvider<P, R>
where
    P: serde::Deserialize<'de>,
    R::V: serde::Deserialize<'de>,
    R: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let dependencies = <Map<P, BTreeMap<R::V, Map<P, R>>>>::deserialize(deserializer)?;

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

impl<P: Debug + Display + Clone + Eq + Hash, R: VersionRange> OfflineDependencyProvider<P, R> {
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
    pub fn add_dependencies<I: IntoIterator<Item = (P, R)>>(
        &mut self,
        package: P,
        version: impl Into<R::V>,
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
    pub fn versions(&self, package: &P) -> Option<impl Iterator<Item = &R::V> + Clone> {
        Some(
            self.dependencies
                .get(&Package(self.arena.get_index_of(package)? as u32))?
                .keys(),
        )
    }

    /// Convert the provided package version to a `Version`.
    pub fn get_package_version(&self, p: &P, v: &R::V) -> Option<Version> {
        self.dependencies
            .get(&self.name_to_package(p)?)?
            .keys()
            .enumerate()
            .find(|&(_, pv)| pv == v)
            .map(|(i, _)| Version::new(i as u8).unwrap())
    }

    /// Lists dependencies of a given package and version.
    /// Returns [None] if no information is available regarding that package and version pair.
    fn dependencies(&self, package: Package, version: Version) -> Option<DependencyConstraints> {
        Some(
            self.dependencies
                .get(&package)?
                .values()
                .nth(version.get() as usize)?
                .iter()
                .map(|(&p, r)| {
                    let vs = self
                        .dependencies
                        .get(&p)
                        .map(|versions| {
                            let mut vs = VersionSet::empty();
                            for (i, ok) in r.contains_many(versions.keys()).enumerate() {
                                if ok {
                                    vs = vs.r#union(VersionSet::singleton(
                                        Version::new(i as u8)
                                            .unwrap_or_else(|| panic!("{}", too_many_versions())),
                                    ));
                                }
                            }
                            vs
                        })
                        .unwrap_or_else(VersionSet::empty);

                    (p, vs)
                })
                .collect(),
        )
    }

    /// Finds a set of packages satisfying dependency bounds for a given package + version pair.
    pub fn resolve(
        &mut self,
        p: &P,
        v: impl Into<R::V>,
    ) -> Result<SelectedDependencies<Self>, PubGrubError<Self>> {
        let Some(version) = self.get_package_version(p, &v.into()) else {
            return Err(PubGrubError::NoRoot);
        };
        resolve(self, p, version)
    }
}

/// An implementation of [DependencyProvider] that
/// contains all dependency information available in memory.
/// Currently packages are picked with the fewest versions contained in the constraints first.
/// But, that may change in new versions if better heuristics are found.
/// Versions are picked with the newest versions first.
impl<P: Debug + Display + Clone + Eq + Hash, R: VersionRange> DependencyProvider
    for OfflineDependencyProvider<P, R>
{
    type P = P;
    type PV = String;
    type PVS = String;
    type M = String;

    type Err = Infallible;

    fn choose_version(
        &mut self,
        _: Package,
        range: VersionSet,
    ) -> Result<Option<Version>, Infallible> {
        Ok(range.last())
    }

    type Priority = Reverse<usize>;
    fn prioritize(&mut self, _: Package, range: VersionSet) -> Self::Priority {
        Reverse(range.count())
    }

    fn get_dependencies(
        &mut self,
        package: Package,
        version: Version,
    ) -> Result<Dependencies<Self::M>, Infallible> {
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

    fn package_version_repr(&self, package: Package, version: Version) -> Self::PV {
        let Some(package_name) = self.package_to_name(package) else {
            return "<unknown>".into();
        };

        let inner = || {
            self.dependencies
                .get(&package)?
                .keys()
                .nth(version.get() as usize)
        };

        match inner() {
            Some(v) => format!("{package_name} @ {v}"),
            None => "<unknown>".into(),
        }
    }

    fn package_version_set_repr(&self, package: Package, version_set: VersionSet) -> Self::PVS {
        let Some(package_name) = self.package_to_name(package) else {
            return "<unknown>".into();
        };

        let versions = self
            .dependencies
            .get(&package)
            .map(|versions| {
                R::from_ordered_versions(versions.keys().enumerate().map(|(i, v)| {
                    let ok = version_set.contains(
                        Version::new(i as u8).unwrap_or_else(|| panic!("{}", too_many_versions())),
                    );
                    (v.clone(), ok)
                }))
            })
            .unwrap_or_else(|| R::from_ordered_versions([]));

        format!("{package_name} @ {versions}")
    }
}

fn too_many_versions() -> String {
    format!(
        "this provider does not support more than {} versions of a package",
        Version::MAX
    )
}
