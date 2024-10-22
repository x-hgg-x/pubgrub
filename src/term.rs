// SPDX-License-Identifier: MPL-2.0

//! A term is the fundamental unit of operation of the PubGrub algorithm.
//! It is a positive or negative expression regarding a set of versions.

use std::fmt::{self, Display};

use crate::{Version, VersionSet};

/// A positive or negative expression regarding a set of versions.
///
/// If a version is selected then `Positive(r)` and `Negative(r.complement())` are equivalent, but
/// they have different semantics when no version is selected. A `Positive` term in the partial
/// solution requires a version to be selected. But a `Negative` term allows for a solution that
/// does not have that package selected. Specifically, `Positive(VS::empty())` means that there was
/// a conflict, we need to select a version for the package but can't pick any, while
/// `Negative(VS::full())` would mean it is fine as long as we don't select the package.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct Term(u64);

impl Term {
    /// Contruct a positive `Term`.
    /// For example, "1.0.0 <= v < 2.0.0" is a positive expression
    /// that is evaluated true if a version is selected
    /// and comprised between version 1.0.0 and version 2.0.0.
    pub(crate) fn positive(vs: VersionSet) -> Self {
        Self(vs.0 | 1)
    }

    /// Contruct a negative `Term`.
    /// For example, "not v < 3.0.0" is a negative expression
    /// that is evaluated true if a version is selected >= 3.0.0
    /// or if no version is selected at all.
    pub(crate) fn negative(vs: VersionSet) -> Self {
        Self(vs.0 & (!1))
    }

    /// A term that is always true.
    pub(crate) fn any() -> Self {
        Self::negative(VersionSet::empty())
    }

    /// A term that is never true.
    pub(crate) fn empty() -> Self {
        Self::positive(VersionSet::empty())
    }

    /// A positive term containing exactly that version.
    pub(crate) fn exact(version: Version) -> Self {
        Self::positive(VersionSet::singleton(version))
    }

    /// Simply check if a term is positive.
    pub fn is_positive(self) -> bool {
        self.0 & 1 != 0
    }

    /// Simply check if a term is negative.
    pub fn is_negative(self) -> bool {
        self.0 & 1 == 0
    }

    /// Negate a term.
    /// Evaluation of a negated term always returns
    /// the opposite of the evaluation of the original one.
    pub(crate) fn negate(self) -> Self {
        Self(self.0 ^ 1)
    }

    /// Get the inner version set.
    pub fn version_set(self) -> VersionSet {
        VersionSet(self.0 & (!1))
    }

    /// Evaluate a term regarding a given choice of version.
    pub(crate) fn contains(self, v: Version) -> bool {
        let set = self.version_set();
        if self.is_positive() {
            set.contains(v)
        } else {
            !set.contains(v)
        }
    }

    /// Unwrap the set contained in a positive term.
    /// Will panic if used on a negative set.
    pub(crate) fn unwrap_positive(self) -> VersionSet {
        if self.is_positive() {
            self.version_set()
        } else {
            panic!("Negative term cannot unwrap positive set")
        }
    }

    /// Unwrap the set contained in a negative term.
    /// Will panic if used on a positive set.
    pub(crate) fn unwrap_negative(self) -> VersionSet {
        if self.is_negative() {
            self.version_set()
        } else {
            panic!("Positive term cannot unwrap negative set")
        }
    }

    /// Compute the intersection of two terms.
    ///
    /// The intersection is positive if at least one of the two terms is positive.
    pub(crate) fn intersection(self, other: Self) -> Self {
        let self_set = self.version_set();
        let other_set = other.version_set();

        match (self.is_positive(), other.is_positive()) {
            (true, true) => Self::positive(self_set.intersection(other_set)),
            (true, false) => Self::positive(other_set.complement().intersection(self_set)),
            (false, true) => Self::positive(self_set.complement().intersection(other_set)),
            (false, false) => Self::negative(self_set.r#union(other_set)),
        }
    }

    /// Check whether two terms are mutually exclusive.
    ///
    /// An optimization for the native implementation of checking whether the intersection of two sets is empty.
    pub(crate) fn is_disjoint(self, other: Self) -> bool {
        let self_set = self.version_set();
        let other_set = other.version_set();

        match (self.is_positive(), other.is_positive()) {
            (true, true) => self_set.is_disjoint(other_set),
            (false, false) => self_set == VersionSet::empty() && other_set == VersionSet::empty(),
            // If the positive term is a subset of the negative term,
            // it lies fully in the region that the negative term excludes.
            (true, false) => self_set.subset_of(other_set),
            (false, true) => other_set.subset_of(self_set),
        }
    }

    /// Compute the union of two terms.
    /// If at least one term is negative, the union is also negative.
    pub(crate) fn r#union(self, other: Self) -> Self {
        let self_set = self.version_set();
        let other_set = other.version_set();

        match (self.is_positive(), other.is_positive()) {
            (true, true) => Self::positive(self_set.r#union(other_set)),
            (true, false) => Self::negative(self_set.complement().intersection(other_set)),
            (false, true) => Self::negative(other_set.complement().intersection(self_set)),
            (false, false) => Self::negative(self_set.intersection(other_set)),
        }
    }

    /// Indicate if this term is a subset of another term.
    /// Just like for sets, we say that t1 is a subset of t2
    /// if and only if t1 ∩ t2 = t1.
    pub(crate) fn subset_of(self, other: Self) -> bool {
        let self_set = self.version_set();
        let other_set = other.version_set();

        match (self.is_positive(), other.is_positive()) {
            (true, true) => self_set.subset_of(other_set),
            (true, false) => self_set.is_disjoint(other_set),
            (false, true) => false,
            (false, false) => other_set.subset_of(self_set),
        }
    }

    /// Check if a set of terms satisfies or contradicts a given term.
    /// Otherwise the relation is inconclusive.
    pub(crate) fn relation_with(self, other_terms_intersection: Self) -> Relation {
        if other_terms_intersection.subset_of(self) {
            Relation::Satisfied
        } else if self.is_disjoint(other_terms_intersection) {
            Relation::Contradicted
        } else {
            Relation::Inconclusive
        }
    }
}

/// Describe a relation between a set of terms S and another term t.
///
/// As a shorthand, we say that a term v
/// satisfies or contradicts a term t if {v} satisfies or contradicts it.
pub(crate) enum Relation {
    /// We say that a set of terms S "satisfies" a term t
    /// if t must be true whenever every term in S is true.
    Satisfied,
    /// Conversely, S "contradicts" t if t must be false
    /// whenever every term in S is true.
    Contradicted,
    /// If neither of these is true we say that S is "inconclusive" for t.
    Inconclusive,
}

impl Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_negative() {
            write!(f, "Not ( ")?;
        }

        let mut list = f.debug_list();
        let mut v = 0u8;
        let mut current = self.0 >> 1;
        while current != 0 {
            if current & 1 != 0 {
                list.entry(&v);
            }
            v += 1;
            current >>= 1;
        }
        list.finish()?;

        if self.is_negative() {
            write!(f, " )")?;
        }

        Ok(())
    }
}

#[cfg(test)]
pub mod tests {
    use proptest::prelude::*;

    use super::*;

    impl Term {
        /// Check if a set of terms satisfies this term.
        ///
        /// We say that a set of terms S "satisfies" a term t
        /// if t must be true whenever every term in S is true.
        ///
        /// It turns out that this can also be expressed with set operations:
        ///    S satisfies t if and only if  ⋂ S ⊆ t
        fn satisfied_by(self, terms_intersection: Self) -> bool {
            terms_intersection.subset_of(self)
        }

        /// Check if a set of terms contradicts this term.
        ///
        /// We say that a set of terms S "contradicts" a term t
        /// if t must be false whenever every term in S is true.
        ///
        /// It turns out that this can also be expressed with set operations:
        ///    S contradicts t if and only if ⋂ S is disjoint with t
        ///    S contradicts t if and only if  (⋂ S) ⋂ t = ∅
        fn contradicted_by(self, terms_intersection: Self) -> bool {
            terms_intersection.intersection(self) == Self::empty()
        }
    }

    pub fn strategy() -> impl Strategy<Value = Term> {
        any::<u64>().prop_map(Term)
    }

    proptest! {
        /// Testing relation
        #[test]
        fn relation_with(term1 in strategy(), term2 in strategy()) {
            match term1.relation_with(term2) {
                Relation::Satisfied => assert!(term1.satisfied_by(term2)),
                Relation::Contradicted => assert!(term1.contradicted_by(term2)),
                Relation::Inconclusive => {
                    assert!(!term1.satisfied_by(term2));
                    assert!(!term1.contradicted_by(term2));
                }
            }
        }

        /// Ensure that we don't wrongly convert between positive and negative ranges
        #[test]
        fn positive_negative(term1 in strategy(), term2 in strategy()) {
            let intersection_positive = term1.is_positive() || term2.is_positive();
            let union_positive = term1.is_positive() & term2.is_positive();
            assert_eq!(term1.intersection(term2).is_positive(), intersection_positive);
            assert_eq!(term1.r#union(term2).is_positive(), union_positive);
        }

        #[test]
        fn is_disjoint_through_intersection(r1 in strategy(), r2 in strategy()) {
            let disjoint_def = r1.intersection(r2) == Term::empty();
            assert_eq!(r1.is_disjoint(r2), disjoint_def);
        }

        #[test]
        fn subset_of_through_intersection(r1 in strategy(), r2 in strategy()) {
            let disjoint_def = r1.intersection(r2) == r1;
            assert_eq!(r1.subset_of(r2), disjoint_def);
        }

        #[test]
        fn union_through_intersection(r1 in strategy(), r2 in strategy()) {
            let union_def = r1
                .negate()
                .intersection(r2.negate())
                .negate();
            assert_eq!(r1.r#union(r2), union_def);
        }
    }
}
