// SPDX-License-Identifier: MPL-2.0

/// Type for identifying versions.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct Version(u8);

impl Version {
    /// Maximum possible version.
    pub const MAX: u64 = (u64::BITS - 1) as u64;

    /// Constructor for an empty set containing no version.
    #[inline]
    pub fn new(v: u8) -> Option<Self> {
        if v < Self::MAX as u8 {
            Some(Self(v))
        } else {
            None
        }
    }

    /// Get the inner version number.
    #[inline]
    pub fn get(self) -> u8 {
        self.0
    }

    pub(crate) fn display_simple(self) -> u8 {
        self.0
    }
}

/// Type for identifying a set of versions.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct VersionSet(pub(crate) u64);

impl VersionSet {
    /// Constructor for an empty set containing no version.
    #[inline]
    pub fn empty() -> Self {
        Self(0)
    }

    /// Constructor for the set containing all versions.
    #[inline]
    pub fn full() -> Self {
        Self(u64::MAX & (!1))
    }

    /// Constructor for a set containing exactly one version.
    #[inline]
    pub fn singleton(v: Version) -> Self {
        Self(2 << v.0)
    }

    /// Compute the complement of this set.
    #[inline]
    pub fn complement(self) -> Self {
        Self((!self.0) & (!1))
    }

    /// Compute the intersection with another set.
    #[inline]
    pub fn intersection(self, other: Self) -> Self {
        Self(self.0 & other.0)
    }

    /// Compute the union with another set.
    #[inline]
    pub fn r#union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    /// Evaluate membership of a version in this set.
    #[inline]
    pub fn contains(self, v: Version) -> bool {
        self.intersection(Self::singleton(v)) != Self::empty()
    }

    /// Whether the set has no overlapping versions.
    #[inline]
    pub fn is_disjoint(self, other: Self) -> bool {
        self.intersection(other) == Self::empty()
    }

    /// Whether all versions of `self` are contained in `other`.
    #[inline]
    pub fn subset_of(self, other: Self) -> bool {
        self == self.intersection(other)
    }

    /// Get the first version of the set.
    #[inline]
    pub fn first(self) -> Option<Version> {
        if self != Self::empty() {
            Some(Version((self.0 >> 1).trailing_zeros() as u8))
        } else {
            None
        }
    }

    /// Get the last version of the set.
    #[inline]
    pub fn last(self) -> Option<Version> {
        if self != Self::empty() {
            Some(Version(
                (Version::MAX - (self.0 >> 1).leading_zeros() as u64) as u8,
            ))
        } else {
            None
        }
    }

    /// Count set elements.
    #[inline]
    pub fn count(self) -> usize {
        self.0.count_ones() as usize
    }
}
