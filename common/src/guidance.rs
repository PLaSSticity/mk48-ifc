// SPDX-FileCopyrightText: 2021 Softbear, Inc.
// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::angle::Angle;

use crate::velocity::Velocity;
//CSE5349: add import
use secret_macros::InvisibleSideEffectFreeDerive;
//CSE5349: Added import
use secret_structs::secret::InvisibleSideEffectFree;
use serde::{Deserialize, Serialize};

//CSE5349: make Guidance InvisibleSideEffectFree, and have named fields
#[derive(Copy, Clone, Debug, Default, PartialEq, Serialize, Deserialize, InvisibleSideEffectFreeDerive)]
pub struct Guidance {
    pub direction_target: Angle,
    pub velocity_target: Velocity,
}

impl Guidance {
    /// new returns a zero Guidance.
    pub fn new() -> Self {
        Self::default()
    }
}
