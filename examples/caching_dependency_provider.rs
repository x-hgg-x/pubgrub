// SPDX-License-Identifier: MPL-2.0

use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::hash::Hash;

use pubgrub::{
    resolve, Dependencies, DependencyProvider, Map, OfflineDependencyProvider, Package, Range,
};

type NumVS = Range<u32>;

// An example implementing caching dependency provider that will
// store queried dependencies in memory and check them before querying more from remote.
struct CachingDependencyProvider<DP: DependencyProvider>
where
    DP::P: Debug + Display + Clone + Eq + Hash,
{
    remote_dependencies: DP,
    cached_dependencies: RefCell<OfflineDependencyProvider<DP::P, DP::VS>>,
}

impl<DP: DependencyProvider> CachingDependencyProvider<DP>
where
    DP::P: Debug + Display + Clone + Eq + Hash,
{
    pub fn new(remote_dependencies_provider: DP) -> Self {
        CachingDependencyProvider {
            remote_dependencies: remote_dependencies_provider,
            cached_dependencies: RefCell::new(OfflineDependencyProvider::new()),
        }
    }
}

impl<DP: DependencyProvider<M = String>> DependencyProvider for CachingDependencyProvider<DP>
where
    DP::P: Debug + Display + Clone + Eq + Hash,
{
    // Caches dependencies if they were already queried
    fn get_dependencies(
        &mut self,
        package: Package,
        version: &DP::V,
    ) -> Result<Dependencies<DP::VS, DP::M>, DP::Err> {
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
                            version.clone(),
                            dependencies
                                .iter()
                                .map(|(&p, vs)| {
                                    (
                                        self.remote_dependencies
                                            .package_to_name(p)
                                            .unwrap()
                                            .clone(),
                                        vs.clone(),
                                    )
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
        range: &DP::VS,
    ) -> Result<Option<DP::V>, DP::Err> {
        self.remote_dependencies.choose_version(package, range)
    }

    type Priority = DP::Priority;

    fn prioritize(&mut self, package: Package, range: &DP::VS) -> Self::Priority {
        self.remote_dependencies.prioritize(package, range)
    }

    type Err = DP::Err;

    type P = DP::P;
    type V = DP::V;
    type VS = DP::VS;
    type M = DP::M;

    fn package_to_name(&self, package: Package) -> Option<&Self::P> {
        self.remote_dependencies.package_to_name(package)
    }

    fn name_to_package(&self, package_name: &Self::P) -> Option<Package> {
        self.remote_dependencies.name_to_package(package_name)
    }
}

fn main() {
    // Simulating remote provider locally.
    let mut remote_dependencies_provider = OfflineDependencyProvider::<&str, NumVS>::new();

    // Add dependencies as needed. Here only root package is added.
    remote_dependencies_provider.add_dependencies("root", 1u32, Vec::new());

    let mut caching_dependencies_provider =
        CachingDependencyProvider::new(remote_dependencies_provider);

    let solution = resolve(&mut caching_dependencies_provider, &"root", 1u32);
    println!("Solution: {:?}", solution);
}
