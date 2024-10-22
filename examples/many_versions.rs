// SPDX-License-Identifier: MPL-2.0

use std::cmp::Reverse;
use std::convert::Infallible;
use std::fmt::{self, Display};

use pubgrub::{
    resolve, DefaultStringReporter, Dependencies, DependencyProvider, FnvIndexSet, Map, Package,
    PubGrubError, Reporter, Version, VersionSet,
};

const MAX_VERSIONS: u64 = 10;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum PackageVersionWrapper<const MAX: u64, P, Id: Clone = P> {
    Pkg { pkg: P, quotient: u64 },
    Virtual { id: Id, quotient: u64, count: u64 },
}

impl<const MAX: u64, P, Id: Clone> PackageVersionWrapper<MAX, P, Id> {
    fn new(pkg: P, true_version: u64) -> (Self, u64) {
        (
            Self::Pkg {
                pkg,
                quotient: true_version / MAX,
            },
            true_version % MAX,
        )
    }

    fn inner(&self, version: u64) -> Option<(&P, u64)> {
        match self {
            Self::Pkg { pkg, quotient } => Some((pkg, quotient * MAX + version)),
            Self::Virtual { .. } => None,
        }
    }

    fn dependency<F, E>(&self, version: u64, mut info_fn: F) -> Result<Option<(Self, u64)>, E>
    where
        F: FnMut(&P) -> Result<(Id, u64), E>,
    {
        match *self {
            Self::Pkg { ref pkg, quotient } => {
                let (id, version_count) = info_fn(pkg)?;
                if version_count <= MAX {
                    Ok(None)
                } else {
                    Ok(Some((
                        Self::Virtual {
                            id,
                            quotient,
                            count: version_count / MAX,
                        },
                        version,
                    )))
                }
            }
            Self::Virtual {
                ref id,
                quotient,
                count,
            } => {
                if count == 0 {
                    Ok(None)
                } else {
                    Ok(Some((
                        Self::Virtual {
                            id: id.clone(),
                            quotient: quotient / MAX,
                            count: count / MAX,
                        },
                        quotient % MAX,
                    )))
                }
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct CustomPackage(PackageVersionWrapper<MAX_VERSIONS, &'static str>);

impl CustomPackage {
    fn from_pkg(name: &'static str, true_version: u64) -> (Self, u64) {
        let (pkg, version) = PackageVersionWrapper::new(name, true_version);
        (CustomPackage(pkg), version)
    }
}

impl Display for CustomPackage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            PackageVersionWrapper::Pkg { pkg, quotient } => {
                write!(f, "{pkg} (q={quotient})")
            }
            PackageVersionWrapper::Virtual {
                id,
                quotient,
                count,
            } => {
                write!(f, "Virtual({id}, q={quotient}, c={count})")
            }
        }
    }
}

struct Provider {
    arena: FnvIndexSet<CustomPackage>,
    version_counts: Map<&'static str, u64>,
}

impl DependencyProvider for Provider {
    type P = CustomPackage;
    type PV = String;
    type PVS = String;
    type M = &'static str;
    type Priority = Reverse<u64>;
    type Err = Infallible;

    fn prioritize(&mut self, package: Package, _: VersionSet) -> Self::Priority {
        match self.package_to_name(package).unwrap().0 {
            PackageVersionWrapper::Pkg { .. } => Reverse(1),
            PackageVersionWrapper::Virtual { .. } => Reverse(0),
        }
    }

    fn choose_version(
        &mut self,
        _: Package,
        range: VersionSet,
    ) -> Result<Option<Version>, Self::Err> {
        Ok(range.last())
    }

    fn get_dependencies(
        &mut self,
        package: Package,
        version: Version,
    ) -> Result<Dependencies<Self::M>, Self::Err> {
        let mut map = Map::default();

        let mut add_dep = |arena: &mut FnvIndexSet<_>, (d, v)| {
            let (idx, _) = arena.insert_full(d);
            let p = Package(idx as u32);
            map.extend([(p, VersionSet::singleton(Version::new(v as u8).unwrap()))]);
        };

        let wrapper = &self.package_to_name(package).unwrap().0;
        let inner = wrapper.inner(version.get() as u64).map(|(&p, v)| (p, v));

        let info_fn = |name: &&'static str| {
            let version_count = *self.version_counts.get(name).ok_or(())?;
            Ok::<_, ()>((*name, version_count))
        };

        match wrapper.dependency(version.get() as u64, info_fn) {
            Ok(None) => (),
            Ok(Some((d, v))) => {
                add_dep(&mut self.arena, (CustomPackage(d), v));
            }
            Err(()) => return Ok(Dependencies::Unavailable("unavailable")),
        }

        if let Some((name, true_version)) = inner {
            match (name, true_version) {
                ("root", 1) => {
                    add_dep(&mut self.arena, CustomPackage::from_pkg("dep1", 1));
                    add_dep(&mut self.arena, CustomPackage::from_pkg("dep2", 1));
                }
                ("dep1", 1) => {
                    add_dep(&mut self.arena, CustomPackage::from_pkg("many", 156));
                    // Uncomment this line to produce an error:
                    // add_dep(&mut self.arena, CustomPackage::from_pkg("many", 15));
                }
                ("dep2", 1) | ("many", 15 | 156) => (),
                _ => return Ok(Dependencies::Unavailable("unavailable")),
            }
        };

        Ok(Dependencies::Available(map))
    }

    fn package_to_name(&self, package: Package) -> Option<&Self::P> {
        self.arena.get_index(package.0 as usize)
    }

    fn name_to_package(&self, package_name: &Self::P) -> Option<Package> {
        Some(Package(self.arena.get_index_of(package_name)? as u32))
    }

    fn package_version_repr(&self, package: Package, version: Version) -> Self::PV {
        let Some(p) = self.package_to_name(package) else {
            return "<unknown>".into();
        };

        format!("{p} @ {}", version.get())
    }

    fn package_version_set_repr(&self, package: Package, version_set: VersionSet) -> Self::PVS {
        let Some(p) = self.package_to_name(package) else {
            return "<unknown>".into();
        };

        let mut versions = Vec::new();
        for i in 0..Version::MAX {
            let v = Version::new(i as u8).unwrap();
            if version_set.contains(v) {
                versions.push(i);
            }
        }

        format!("{p} @ {versions:?}")
    }
}

fn main() {
    let (root_pkg, _) = CustomPackage::from_pkg("root", 1);

    let mut provider = Provider {
        arena: FnvIndexSet::from_iter([root_pkg.clone()]),
        version_counts: Map::from_iter([("root", 1), ("dep1", 10), ("dep2", 11), ("many", 200)]),
    };

    match resolve(&mut provider, &root_pkg, Version::new(1).unwrap()) {
        Ok(sol) => {
            for (p, v) in sol {
                let pv = provider.package_version_repr(provider.name_to_package(&p).unwrap(), v);
                println!("{pv}");
            }
        }
        Err(PubGrubError::NoSolution(mut derivation_tree)) => {
            derivation_tree.collapse_no_versions();
            eprintln!(
                "{}",
                DefaultStringReporter::report(&derivation_tree, &provider)
            );
            std::process::exit(1);
        }
        Err(err) => panic!("{:?}", err),
    }
}
