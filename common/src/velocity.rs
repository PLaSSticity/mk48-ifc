// SPDX-FileCopyrightText: 2021 Softbear, Inc.
// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::ticks::{Ticks, TicksRepr};
use core_protocol::serde_util::{F32Visitor, I16Visitor};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fmt;
use std::ops::{Add, AddAssign, Mul, Neg, Sub, SubAssign};

//CSE5349: add import
use secret_macros::InvisibleSideEffectFreeDerive;

type VelocityRepr = i16;

// Note: pub(crate) is intentional.
//CSE5349: Make struct Velocity InvisibleSideEffectFree
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, InvisibleSideEffectFreeDerive)]
pub struct Velocity{pub v: VelocityRepr}

/// Velocity efficiently stores a signed speed.
#[allow(dead_code)]
impl Velocity {
    //CSE5349: Make struct Velocity have named fields
    /// Zero velocity (at rest).
    pub const ZERO: Self = Self{v: 0};
    /// Smallest representable positive velocity.
    pub const UNIT: Self = Self{v: 1};
    /// Minimum (negative) velocity.
    pub const MIN: Self = Self{v: VelocityRepr::MIN};
    /// Maximum possible velocity.
    pub const MAX: Self = Self{v: VelocityRepr::MAX};
    /// Inverse of scale.
    const INV_SCALE: u32 = 1 << 5;
    /// How many meters per second per unit of velocity.
    const SCALE: f32 = 1.0 / Self::INV_SCALE as f32;
    /// How many knots per unit of velocity.
    const KNOTS_SCALE: f32 = Self::SCALE * 1.94384;
    /// Max reverse velocity as a function of max forward velocity.
    pub const MAX_REVERSE_SCALE: f32 = -1.0 / 3.0;

    /// new returns zero Velocity.
    pub fn new() -> Self {
        Self::ZERO
    }

    /// to_mps returns an amount of meters per second corresponding to the Velocity.
    #[inline]
    pub fn to_mps(self) -> f32 {
        //CSE5349: Make struct Velocity have named fields
        self.v as f32 * Self::SCALE
    }

    /// from_mps returns a Velocity from a given amount of meters per second.
    #[inline]
    pub fn from_mps(mps: f32) -> Self {
        //CSE5349: Make struct Velocity have named fields
        Self{v: (mps * (1.0 / Self::SCALE)) as VelocityRepr}
    }

    /// from_mps returns a Velocity from a given amount of centimeters per second.
    pub const fn from_whole_cmps(cmps: u32) -> Self {
        let scaled = cmps * Self::INV_SCALE / 100;
        if scaled > VelocityRepr::MAX as u32 {
            debug_assert!(false, "from_whole_cmps overflow");
            Self::MAX
        } else {
            //CSE5349: Make struct Velocity have named fields
            Self{v: scaled as VelocityRepr}
        }
    }

    /// to_knots returns an amount of knots corresponding to the Velocity.
    #[inline]
    pub fn to_knots(self) -> f32 {
        //CSE5349: Make struct Velocity have named fields
        self.v as f32 * Self::KNOTS_SCALE
    }

    /// from_knots returns a velocity from a given amount of knots.
    #[inline]
    pub fn from_knots(knots: f32) -> Self {
        //CSE5349: Make struct Velocity have named fields
        Self{v: (knots * (1.0 / Self::KNOTS_SCALE)) as VelocityRepr}
    }

    /// clamp returns the velocity, clamped between min and max.
    pub fn clamp(self, min: Self, max: Self) -> Self {
        //CSE5349: Make struct Velocity have named fields
        Self{v: self.v.clamp(min.v, max.v) as VelocityRepr}
    }

    /// clamp_magnitude returns the original Velocity such that its magnitude is less than or
    /// equal to max (which must be non-negative).
    pub fn clamp_magnitude(self, max: Self) -> Self {
        //CSE5349: Make struct Velocity have named fields
        debug_assert!(max.v >= 0);
        self.clamp(-max, max)
    }

    /// abs returns the absolute value of a Velocity.
    pub fn abs(self) -> Self {
        //CSE5349: Make struct Velocity have named fields
        Self{v: self.v.abs() as VelocityRepr}
    }

    /// difference returns the positive difference between two velocities.
    pub fn difference(self, other: Self) -> Self {
        if self < other {
            other - self
        } else {
            self - other
        }
    }

    /// lerp linearly interpolates between velocities.
    pub fn lerp(self, other: Self, value: f32) -> Self {
        self + (other - self) * value
    }
}

impl Default for Velocity {
    /// default returns zero Velocity.
    fn default() -> Self {
        Self::ZERO
    }
}

impl Add for Velocity {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        //CSE5349: Make struct Velocity have named fields
        Self{v: self.v.saturating_add(other.v)}
    }
}

impl AddAssign for Velocity {
    fn add_assign(&mut self, other: Self) {
        //CSE5349: Make struct Velocity have named fields
        self.v = self.v.saturating_add(other.v);
    }
}

impl Sub for Velocity {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        //CSE5349: Make struct Velocity have named fields
        Self{v: self.v.saturating_sub(other.v)}
    }
}

impl SubAssign for Velocity {
    fn sub_assign(&mut self, other: Self) {
        //CSE5349: Make struct Velocity have named fields
        self.v = self.v.saturating_sub(other.v);
    }
}

impl Neg for Velocity {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::ZERO - self
    }
}

impl Mul<f32> for Velocity {
    type Output = Self;

    fn mul(self, other: f32) -> Self::Output {
        //CSE5349: Make struct Velocity have named fields
        Self{v: (self.v as f32 * other) as VelocityRepr}
    }
}

impl Mul<Ticks> for Velocity {
    type Output = Self;

    fn mul(self, other: Ticks) -> Self::Output {
        debug_assert!(other.0 < VelocityRepr::MAX as TicksRepr);
        //CSE5349: Make struct Velocity have named fields
        Velocity{v: (self.v.saturating_mul(other.0 as VelocityRepr)) as VelocityRepr}
    }
}

impl fmt::Debug for Velocity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_mps())
    }
}

impl Serialize for Velocity {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if serializer.is_human_readable() {
            serializer.serialize_f32(self.to_mps())
        } else {
            //CSE5349: Make struct Velocity have named fields
            serializer.serialize_i16(self.v)
        }
    }
}

impl<'de> Deserialize<'de> for Velocity {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if deserializer.is_human_readable() {
            deserializer.deserialize_f32(F32Visitor).map(Self::from_mps)
        } else {
            //CSE5349: Make struct Velocity have named fields
            deserializer.deserialize_i16(I16Visitor).map(|v| Velocity{v: v})
        }
    }
}

#[cfg(test)]
mod tests {
    // use crate::velocity::Velocity;

    // TODO: Test velocity.
}
