// SPDX-License-Identifier: MPL-2.0

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;

use pubgrub::{
    resolve, Dependencies, DependencyConstraints, DependencyProvider, Map,
    OfflineDependencyProvider, PackageArena, PackageId, Ranges,
};

type NumVS = Ranges<u32>;
type CachedDeps<V, VS> = RefCell<Map<PackageId, BTreeMap<V, DependencyConstraints<VS>>>>;

// An example implementing caching dependency provider that will
// store queried dependencies in memory and check them before querying more from remote.
struct CachingDependencyProvider<DP: DependencyProvider>
where
    DP::P: Debug + Display + Clone + Eq + Hash,
{
    remote_dependencies: DP,
    cached_dependencies: CachedDeps<DP::V, DP::VS>,
}

impl<DP: DependencyProvider> CachingDependencyProvider<DP>
where
    DP::P: Debug + Display + Clone + Eq + Hash,
{
    pub fn new(remote_dependencies_provider: DP) -> Self {
        CachingDependencyProvider {
            remote_dependencies: remote_dependencies_provider,
            cached_dependencies: Default::default(),
        }
    }
}

impl<DP: DependencyProvider<M = &'static str>> DependencyProvider for CachingDependencyProvider<DP>
where
    DP::P: Debug + Display + Clone + Eq + Hash,
{
    // Cache dependencies if they were already queried
    fn get_dependencies(
        &mut self,
        package_id: PackageId,
        version: &DP::V,
        package_store: &mut PackageArena<Self::P>,
    ) -> Result<Dependencies<DP::VS, DP::M>, DP::Err> {
        let mut cache = self.cached_dependencies.borrow_mut();
        if let Some(deps) = cache.get(&package_id).and_then(|vmap| vmap.get(version)) {
            return Ok(Dependencies::Available(deps.clone()));
        }

        match self
            .remote_dependencies
            .get_dependencies(package_id, version, package_store)
        {
            Ok(Dependencies::Available(deps)) => {
                cache
                    .entry(package_id)
                    .or_default()
                    .insert(version.clone(), deps.clone());
                Ok(Dependencies::Available(deps))
            }

            Ok(Dependencies::Unavailable(reason)) => Ok(Dependencies::Unavailable(reason)),
            error @ Err(_) => error,
        }
    }

    fn choose_version(
        &mut self,
        package_id: PackageId,
        ranges: &DP::VS,
        package_store: &PackageArena<Self::P>,
    ) -> Result<Option<DP::V>, DP::Err> {
        self.remote_dependencies
            .choose_version(package_id, ranges, package_store)
    }

    type Priority = DP::Priority;

    fn prioritize(
        &mut self,
        package_id: PackageId,
        ranges: &DP::VS,
        package_store: &PackageArena<Self::P>,
    ) -> Self::Priority {
        self.remote_dependencies
            .prioritize(package_id, ranges, package_store)
    }

    type Err = DP::Err;

    type P = DP::P;
    type V = DP::V;
    type VS = DP::VS;
    type M = DP::M;
}

fn main() {
    // Simulating remote provider locally.
    let mut remote_dependencies_provider = OfflineDependencyProvider::<&str, NumVS>::new();

    // Add dependencies as needed. Here only root package is added.
    remote_dependencies_provider.add_dependencies("root", 1u32, Vec::new());

    let mut caching_dependencies_provider =
        CachingDependencyProvider::new(remote_dependencies_provider);

    let solution = resolve(&mut caching_dependencies_provider, "root", 1u32);
    println!("Solution: {:?}", solution);
}
