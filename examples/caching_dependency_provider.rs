// SPDX-License-Identifier: MPL-2.0

use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::hash::Hash;

use pubgrub::{
    resolve, Dependencies, DependencyProvider, Map, OfflineDependencyProvider, Package,
    PubGrubError, Range, SelectedDependencies, Version, VersionRange, VersionSet,
};

type NumVS = Range<u32>;

trait RemoteProvider: DependencyProvider {
    type R: VersionRange;

    fn get_package_version(&self, p: &Self::P, v: &<Self::R as VersionRange>::V)
        -> Option<Version>;

    fn ordered_versions(
        &self,
        package: Package,
    ) -> Option<impl Iterator<Item = &<Self::R as VersionRange>::V> + Clone>;
}

impl<P: Debug + Display + Clone + Eq + Hash, R: VersionRange> RemoteProvider
    for OfflineDependencyProvider<P, R>
{
    type R = R;

    fn get_package_version(
        &self,
        p: &Self::P,
        v: &<Self::R as VersionRange>::V,
    ) -> Option<Version> {
        self.get_package_version(p, v)
    }

    fn ordered_versions(
        &self,
        package: Package,
    ) -> Option<impl Iterator<Item = &<Self::R as VersionRange>::V> + Clone> {
        self.versions(self.package_to_name(package).unwrap())
    }
}

// An example implementing caching dependency provider that will
// store queried dependencies in memory and check them before querying more from remote.
struct CachingDependencyProvider<DP: RemoteProvider<R = R>, R: VersionRange>
where
    DP::P: Debug + Display + Clone + Eq + Hash,
{
    remote_dependencies: DP,
    cached_dependencies: RefCell<OfflineDependencyProvider<DP::P, R>>,
}

impl<DP: RemoteProvider<R = R, M = String>, R: VersionRange> CachingDependencyProvider<DP, R>
where
    DP::P: Debug + Display + Clone + Eq + Hash,
{
    fn new(remote_dependencies_provider: DP) -> Self {
        CachingDependencyProvider {
            remote_dependencies: remote_dependencies_provider,
            cached_dependencies: RefCell::new(OfflineDependencyProvider::new()),
        }
    }

    fn resolve(
        &mut self,
        p: &<Self as DependencyProvider>::P,
        v: impl Into<R::V>,
    ) -> Result<SelectedDependencies<Self>, PubGrubError<Self>> {
        let Some(version) = self.remote_dependencies.get_package_version(p, &v.into()) else {
            return Err(PubGrubError::NoRoot);
        };
        resolve(self, p, version)
    }
}

impl<DP: RemoteProvider<M = String, R = R>, R: VersionRange> DependencyProvider
    for CachingDependencyProvider<DP, R>
where
    DP::P: Debug + Display + Clone + Eq + Hash,
    R::V: Clone,
{
    // Caches dependencies if they were already queried
    fn get_dependencies(
        &mut self,
        package: Package,
        version: Version,
    ) -> Result<Dependencies<DP::M>, DP::Err> {
        let mut cache = self.cached_dependencies.borrow_mut();
        match cache.get_dependencies(package, version) {
            Ok(Dependencies::Unavailable(_)) => {
                let dependencies = self.remote_dependencies.get_dependencies(package, version);
                match dependencies {
                    Ok(Dependencies::Available(dependencies)) => {
                        cache.add_dependencies(
                            self.remote_dependencies
                                .package_to_name(package)
                                .unwrap()
                                .clone(),
                            self.remote_dependencies
                                .ordered_versions(package)
                                .unwrap()
                                .nth(version.get() as usize)
                                .cloned()
                                .unwrap(),
                            dependencies
                                .iter()
                                .map(|(&p, vs)| {
                                    let pn = self
                                        .remote_dependencies
                                        .package_to_name(p)
                                        .cloned()
                                        .unwrap();

                                    let r = R::from_ordered_versions(
                                        self.remote_dependencies
                                            .ordered_versions(p)
                                            .unwrap()
                                            .enumerate()
                                            .map(|(i, v)| {
                                                (
                                                    v.clone(),
                                                    vs.contains(Version::new(i as u8).unwrap()),
                                                )
                                            }),
                                    );

                                    (pn, r)
                                })
                                .collect::<Map<_, _>>(),
                        );
                        Ok(Dependencies::Available(dependencies))
                    }
                    Ok(Dependencies::Unavailable(reason)) => Ok(Dependencies::Unavailable(reason)),
                    error @ Err(_) => error,
                }
            }
            Ok(dependencies) => Ok(dependencies),
            Err(_) => unreachable!(),
        }
    }

    fn choose_version(
        &mut self,
        package: Package,
        range: VersionSet,
    ) -> Result<Option<Version>, DP::Err> {
        self.remote_dependencies.choose_version(package, range)
    }

    type Priority = DP::Priority;

    fn prioritize(&mut self, package: Package, range: VersionSet) -> Self::Priority {
        self.remote_dependencies.prioritize(package, range)
    }

    type Err = DP::Err;

    type P = DP::P;
    type PV = DP::PV;
    type PVS = DP::PVS;
    type M = DP::M;

    fn package_to_name(&self, package: Package) -> Option<&Self::P> {
        self.remote_dependencies.package_to_name(package)
    }

    fn name_to_package(&self, package_name: &Self::P) -> Option<Package> {
        self.remote_dependencies.name_to_package(package_name)
    }

    fn package_version_repr(&self, package: Package, version: Version) -> Self::PV {
        self.remote_dependencies
            .package_version_repr(package, version)
    }

    fn package_version_set_repr(&self, package: Package, version_set: VersionSet) -> Self::PVS {
        self.remote_dependencies
            .package_version_set_repr(package, version_set)
    }
}

fn main() {
    // Simulating remote provider locally.
    let mut remote_dependencies_provider = OfflineDependencyProvider::<&str, NumVS>::new();

    // Add dependencies as needed. Here only root package is added.
    remote_dependencies_provider.add_dependencies("root", 1u32, Vec::new());

    let mut caching_dependencies_provider =
        CachingDependencyProvider::new(remote_dependencies_provider);

    let solution = caching_dependencies_provider.resolve(&"root", 1u32);
    println!("Solution: {:?}", solution);
}
