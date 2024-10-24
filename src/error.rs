// SPDX-License-Identifier: MPL-2.0

//! Handling pubgrub errors.

use thiserror::Error;

use crate::{DependencyProvider, DerivationTree};

/// There is no solution for this set of dependencies.
pub type NoSolutionError<DP> = DerivationTree<<DP as DependencyProvider>::M>;

/// Errors that may occur while solving dependencies.
#[derive(Error)]
pub enum PubGrubError<DP: DependencyProvider> {
    /// Root package name doesn't exist.
    #[error("Root package name doesn't exist")]
    NoRoot,

    /// There is no solution for this set of dependencies.
    #[error("No solution")]
    NoSolution(NoSolutionError<DP>),

    /// Error arising when the implementer of [DependencyProvider] returned an error in the method
    /// [get_dependencies](DependencyProvider::get_dependencies).
    #[error("Retrieving dependencies of {package_version} failed")]
    ErrorRetrievingDependencies {
        /// Package and version whose dependencies we want.
        package_version: DP::PV,
        /// Error raised by the implementer of
        /// [DependencyProvider].
        source: DP::Err,
    },

    /// Error arising when the implementer of [DependencyProvider] returned an error in the method
    /// [choose_version](DependencyProvider::choose_version).
    #[error("Decision making failed")]
    ErrorChoosingPackageVersion(#[source] DP::Err),

    /// Error arising when the implementer of [DependencyProvider]
    /// returned an error in the method [should_cancel](DependencyProvider::should_cancel).
    #[error("We should cancel")]
    ErrorInShouldCancel(#[source] DP::Err),

    /// Something unexpected happened.
    #[error("{0}")]
    Failure(String),
}

impl<DP: DependencyProvider> From<NoSolutionError<DP>> for PubGrubError<DP> {
    fn from(err: NoSolutionError<DP>) -> Self {
        Self::NoSolution(err)
    }
}

impl<DP> std::fmt::Debug for PubGrubError<DP>
where
    DP: DependencyProvider,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoRoot => f.debug_struct("NoRoot").finish(),
            Self::NoSolution(err) => f.debug_tuple("NoSolution").field(&err).finish(),
            Self::ErrorRetrievingDependencies {
                package_version,
                source,
            } => f
                .debug_struct("ErrorRetrievingDependencies")
                .field("package_version", package_version)
                .field("source", source)
                .finish(),
            Self::ErrorChoosingPackageVersion(arg0) => f
                .debug_tuple("ErrorChoosingPackageVersion")
                .field(arg0)
                .finish(),
            Self::ErrorInShouldCancel(arg0) => {
                f.debug_tuple("ErrorInShouldCancel").field(arg0).finish()
            }
            Self::Failure(arg0) => f.debug_tuple("Failure").field(arg0).finish(),
        }
    }
}
