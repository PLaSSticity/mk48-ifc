// SPDX-FileCopyrightText: 2021 Softbear, Inc.
// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::bot::*;
use crate::entity_extension::EntityExtension;
use crate::player::*;
use crate::protocol::*;
use crate::world::World;
use crate::entity::Entity;
use common::world::{clamp_y_to_strict_area_border, outside_strict_area, ARCTIC};
use common::entity::EntityType;
use common::protocol::{Command, Update};
use common::terrain::ChunkSet;
use common::ticks::Ticks;
use common::util::level_to_score;
use core_protocol::id::*;
use game_server::context::Context;
use game_server::game_service::GameArenaService;
use game_server::player::{PlayerRepo, PlayerTuple};
use log::{error, warn};
use secret_structs::info_flow_block_declassify_dynamic_integrity;
use std::cell::UnsafeCell;
use std::sync::Arc;
use std::time::Duration;
use std::num::NonZeroU32;
use std::collections::HashMap;
use secret_macros::*;
use secret_structs::secret::*;
use secret_structs::integrity_lattice as int_lat;
use secret_structs::ternary_lattice as sec_lat;
use secret_structs::info_flow_block_dynamic_integrity;
use rand::{thread_rng, Rng};
use common::util::score_to_level;
use glam::Vec2;
use common_util::range::map_ranges;
use common::entity::*;

/// A game server.
pub struct Server {
    pub world: World,
    pub counter: Ticks,
    pub map: HashMap<NonZeroU32, DynamicIntegrityLabel>,
}

/// Stores a player, and metadata related to it. Data stored here may only be accessed when processing,
/// this client (i.e. not when processing other entities). Bots don't use this.
#[derive(Default, Debug)]
pub struct ClientData {
    pub loaded_chunks: ChunkSet,
}

#[derive(Default)]
pub struct PlayerExtension(pub UnsafeCell<EntityExtension>);

/// This is sound because access is limited to when the entity is in scope.
unsafe impl Send for PlayerExtension {}
unsafe impl Sync for PlayerExtension {}

pub trait Combined {}
impl<T: CommandTrait + SecretValueSafe> Combined for T {}
unsafe impl<T: Combined> InvisibleSideEffectFree for T {}
fn as_command_integrity(param: InfoFlowStruct<Command, sec_lat::Label_Empty, int_lat::Label_All, (), DynamicIntegrityLabel>) -> InfoFlowStruct<&dyn Combined, sec_lat::Label_Empty, int_lat::Label_All, (), DynamicIntegrityLabel> {

}

impl GameArenaService for Server {
    const GAME_ID: GameId = GameId::Mk48;
    const TICK_PERIOD_SECS: f32 = Ticks::PERIOD_SECS;

    /// How long a player can remain in limbo after they lose connection.
    const LIMBO: Duration = Duration::from_secs(6);

    //const TEAM_MEMBERS_MAX: usize = 2;
    //const TEAM_JOINERS_MAX: usize = 2;

    type Bot = Bot;
    type ClientData = ClientData;
    type GameUpdate = Update;
    type GameRequest = Command;
    type PlayerData = Player;
    type PlayerExtension = PlayerExtension;

    /// new returns a game server with the specified parameters.
    fn new(min_players: usize) -> Self {
        let mut m = HashMap::<NonZeroU32, DynamicIntegrityLabel>::new();
        Self {
            world: World::new(World::target_radius(
                min_players as f32 * EntityType::FairmileD.data().visual_area(),
            )),
            counter: Ticks::ZERO,
            map: m,
        }
    }

    fn team_members_max(players: usize) -> usize {
        match players {
            100..=usize::MAX => 8,
            80..=99 => 7,
            60..=79 => 6,
            50..=59 => 5,
            _ => 4,
        }
    }

    fn player_joined(
        &mut self,
        player_tuple: &Arc<PlayerTuple<Self>>,
        _players: &PlayerRepo<Server>,
    ) {
        let mut player = player_tuple.borrow_player_mut();
        player.data.flags.left_game = false;
        #[cfg(debug_assertions)]
        {
            use common::entity::EntityData;
            //use common::util::level_to_score;
            use rand::{thread_rng, Rng};
            let highest_level_score = level_to_score(EntityData::MAX_BOAT_LEVEL);
            player.score = if player.is_bot() {
                thread_rng().gen_range(0..=highest_level_score)
            } else {
                highest_level_score
            };
        }
    }

    fn player_command(
        &mut self,
        update: Self::GameRequest,
        player: &Arc<PlayerTuple<Self>>,
        _players: &PlayerRepo<Server>,
    ) -> Option<Update> {
        let player_label = {
            if !self.map.contains_key(&player.player.borrow().player_id.0) {
                let new_label = new_dynamic_integrity_label(vec![get_new_integrity_tag()]);
                self.map.insert(player.player.borrow().player_id.0, new_label);
        
            }
            self.map.get(&player.player.borrow().player_id.0).unwrap()
        };
        let protected_update = info_flow_block_dynamic_integrity!(sec_lat::Label_Empty, int_lat::Label_All, player_label, {
            wrap_secret(update)
        });
        if let Err(e) = protected_update.as_command().apply(&mut self.world, player) {
            warn!("Command resulted in {}", e);
        }
        
        //as_command_integrity(param: InfoFlowStruct<Command>) -> InfoFlowStruct<&dyn CommandTrait + SecretValueSafe>
        fn as_command_apply(
            param: InfoFlowStruct<Self::GameRequest, sec_lat:Label_Empty, int_lat:Label_All, (), DynamicIntegrityLabel>, 
            world: &mut World,
            player_tuple: &Arc<PlayerTuple<Server>>
        ) -> Result<(), &'static str> {
            let a = info_flow_block_declassify_dynamic_integrity!(sec_lat::Label_Empty, int_lat::Label_All, param.get_dynamic_secret_label_clone(), {
                let unwrapped = unwrap_secret_ref(&param);
                match *unwrapped {
                    Control(Control) => {
                        1
                    },
                    Spawn(Spawn) => {
                        2
                    },
                    Upgrade(Upgrade) => {
                        3
                    },
                }
            });
            match a {
                1 => {
                    let mut player = player_tuple.borrow_player_mut();

                    // Pre-borrow.
                    let world_radius = world.radius;

                    return if let Status::Alive {
                        entity_index,
                        aim_target,
                        ..
                    } = &mut player.data.status
                    {
                        let entity = &mut world.entities[*entity_index];

                        // Movement
                        let cond = info_flow_block_declassify_dynamic_integrity!(sec_lat::Label_Empty, int_lat::Label_All, param.get_dynamic_integrity_label_clone(), {
                            let unwrapped = unwrap_secret_ref(&param);
                            match unwrapped.guidance {
                                Some => {
                                    true
                                },
                                None => {
                                    false
                                }
                            }
                        });
                        if cond {
                            entity.guidance = info_flow_block_declassify_dynamic_integrity!(sec_lat::Label_Empty, int_lat::Label_All, param.get_dynamic_integrity_label_clone(), {
                                let unwrapped = unwrap_secret_ref(&param);
                                std::option::Option::unwrap(unwrapped.guidance)
                            });
                        }
                        /*if let Some(guidance) = self.guidance {
                            entity.guidance = guidance;
                        }*/
                        let cond = info_flow_block_declassify_dynamic_integrity!(sec_lat::Label_Empty, int_lat::Label_All, param.get_dynamic_integrity_label_clone(), {
                            let unwrapped = unwrap_secret_ref(&param);
                            match unwrapped.aim_target {
                                Some => {
                                    true
                                },
                                None => {
                                    false
                                }
                            }
                        });
                        let entity_range = entity.data().sensors.max_range();
                        *aim_target = if cond {
                            entity.guidance = info_flow_block_declassify_dynamic_integrity!(sec_lat::Label_Empty, int_lat::Label_All, param.get_dynamic_integrity_label_clone(), {
                                let unwrapped = unwrap_secret_ref(&param);
                                let aim = std::option::Option::unwrap(unwrapped.aim_target);
                                let new_aim = sanitize_floats_integrity(aim, -world_radius * 2.0..world_radius * 2.0);
                                Some(::clamp_length_max(new_aim - entity.transform.position, entity_range) + entity.transform.position)
                            });
                        } else {
                            None
                        }
                        /**aim_target = if let Some(mut aim_target) = self.aim_target {
                            sanitize_floats(aim_target.as_mut(), -world_radius * 2.0..world_radius * 2.0)?;
                            Some(
                                (aim_target - entity.transform.position)
                                    .clamp_length_max(entity.data().sensors.max_range())
                                    + entity.transform.position,
                            )
                        } else {
                            None
                        };*/
                        let extension = entity.extension_mut();
                        let submerge = info_flow_block_declassify_dynamic_integrity!(sec_lat::Label_Empty, int_lat::Label_All, param.get_dynamic_integrity_label_clone(), {
                            let unwrapped = unwrap_secret_ref(&param);
                            param.submerge
                        });
                        extension.set_submerge(submerge);
                        let active = info_flow_block_declassify_dynamic_integrity!(sec_lat::Label_Empty, int_lat::Label_All, param.get_dynamic_integrity_label_clone(), {
                            let unwrapped = unwrap_secret_ref(&param);
                            param.active
                        });
                        extension.set_active(active);

                        drop(player);
                        let cond = info_flow_block_declassify_dynamic_integrity!(sec_lat::Label_Empty, int_lat::Label_All, param.get_dynamic_integrity_label_clone(), {
                            let unwrapped = unwrap_secret_ref(&param);
                            match unwrapped.fire {
                                Some => { true },
                                None => { false }
                            }
                        });
                        let fire = info_flow_block_dynamic_integrity!(sec_lat::Label_Empty, int_lat::Label_All, param.get_dynamic_integrity_label_clone(), {
                            let unwrapped = unwrap_secret_ref(&param);
                            wrap_secret(param.fire)
                        });
                            

                        if let Some(fire) = &self.fire {
                            fire.as_command_apply(world, player_tuple)?;
                        }

                        if let Some(pay) = &self.pay {
                            pay.apply(world, player_tuple)?;
                        }

                        if let Some(hint) = &self.hint {
                            hint.apply(world, player_tuple)?;
                        }

                        Ok(())
                    } else {
                        Err("cannot control while not alive")
                    };
                },
                2 => {
                    let mut player = player_tuple.borrow_player();

                    if player.data.flags.left_game {
                        debug_assert!(
                            false,
                            "should never happen, since messages should not be accepted"
                        );
                        return Err("cannot spawn after left game");
                    }
            
                    if player.data.status.is_alive() {
                        return Err("cannot spawn while already alive");
                    }

                    let spawn_entity_type = info_flow_block_declassify_dynamic_integrity!(sec_lat::Label_Empty, int_lat::Label_All, param.get_dynamic_integrity_label_clone(), {
                        let unwrapped = unwrap_secret_ref(&param);
                        param.entity_type;
                    });

                    if !spawn_entity_type.can_spawn_as(player.score, player.is_bot()) {
                        return Err("cannot spawn as given entity type");
                    }

                    // These initial positions may be overwritten later.
                    let mut spawn_position = Vec2::ZERO;
                    let mut spawn_radius = 0.8 * world.radius;

                    let mut rng = thread_rng();

                    if !(player.is_bot() && rng.gen()) {
                        let raw_spawn_y = map_ranges(
                            score_to_level(player.score) as f32,
                            1.5..(EntityData::MAX_BOAT_LEVEL - 1) as f32,
                            -0.75 * world.radius..ARCTIC.min(0.75 * world.radius),
                            true,
                        );
                        debug_assert!((-world.radius..=world.radius).contains(&raw_spawn_y));
            
                        // Don't spawn in wrong area.
                        let spawn_y = clamp_y_to_strict_area_border(spawn_entity_type, raw_spawn_y);
            
                        if spawn_y.abs() > world.radius {
                            return Err("unable to spawn this type of boat");
                        }
            
                        // Solve circle equation.
                        let world_half_width_at_spawn_y = (world.radius.powi(2) - spawn_y.powi(2)).sqrt();
                        debug_assert!(world_half_width_at_spawn_y <= world.radius);
            
                        // Randomize horizontal a bit. This value will end up in the range
                        // [-world_half_width_at_spawn_y / 2, world_half_width_at_spawn_y / 2].
                        let spawn_x = (rng.gen::<f32>() - 0.5) * world_half_width_at_spawn_y;
            
                        spawn_position = Vec2::new(spawn_x, spawn_y);
                        spawn_radius = world.radius * (1.0 / 3.0);
                    }

                    debug_assert!(spawn_position.length() <= world.radius);

                    let exclusion_zone = match &player.data.status {
                        // Player is excluded from spawning too close to where another player sunk them, for
                        // fairness reasons.
                        Status::Dead {
                            reason,
                            position,
                            time,
                            ..
                        } => {
                            // Don't spawn too far away from where you died.
                            spawn_position = *position;
                            spawn_radius = (0.4 * world.radius).clamp(1200.0, 3000.0).min(world.radius);
            
                            // Don't spawn right where you died either.
                            let exclusion_seconds =
                                if player.score > level_to_score(EntityData::MAX_BOAT_LEVEL / 2) {
                                    20
                                } else {
                                    10
                                };
            
                            if reason.is_due_to_player()
                                && time.elapsed() < Duration::from_secs(exclusion_seconds)
                            {
                                Some(*position)
                            } else {
                                None
                            }
                        }
                        _ => None,
                    };

                    if player.team_id().is_some() || player.invitation_accepted().is_some() {
                        // TODO: Inefficient to scan all entities; only need to scan all players. Unfortunately,
                        // that data is not available here, currently.
                        if let Some((_, team_boat)) = world
                            .entities
                            .par_iter()
                            .into_maybe_parallel_iter()
                            .find_any(|(_, entity)| {
                                let data = entity.data();
                                if data.kind != EntityKind::Boat {
                                    return false;
                                }
            
                                if let Some(exclusion_zone) = exclusion_zone {
                                    if entity.transform.position.distance_squared(exclusion_zone)
                                        < 1100f32.powi(2)
                                    {
                                        return false;
                                    }
                                }
            
                                let is_team_member = player.team_id().is_some()
                                    && entity.borrow_player().team_id() == player.team_id();
            
                                let was_invited_by = player.invitation_accepted().is_some()
                                    && entity.borrow_player().player_id
                                        == player.invitation_accepted().as_ref().unwrap().player_id;
            
                                is_team_member || was_invited_by
                            })
                        {
                            spawn_position = team_boat.transform.position;
                            spawn_radius = team_boat.data().radius + 25.0;
                        }
                    }
            
                    drop(player);
            
                    let mut boat = Entity::new(spawn_entity_type, Some(Arc::clone(player_tuple)));
                    boat.transform.position = spawn_position;
                    //#[cfg(debug_assertions)]
                    //let begin = std::time::Instant::now();
                    if world.spawn_here_or_nearby(boat, spawn_radius, exclusion_zone) {
                        /*
                        #[cfg(debug_assertions)]
                        println!(
                            "took {:?} to spawn a {:?}",
                            begin.elapsed(),
                            self.entity_type
                        );
                         */
                        Ok(())
                    } else {
                        Err("failed to find enough space to spawn")
                    }

                }, 
                3 => {
                    let mut player = player_tuple.borrow_player_mut();
                    let status = &mut player.data.status;
                    
                    let upgrade_entity_type = info_flow_block_declassify_dynamic_integrity!(sec_lat::Label_Empty, int_lat::Label_All, param.get_dynamic_integrity_label_clone(), {
                        let unwrapped = unwrap_secret_ref(&param);
                        param.entity_type;
                    });

                    if let Status::Alive { entity_index, .. } = status {
                        let entity = &mut world.entities[*entity_index];
                        if !entity
                            .entity_type
                            .can_upgrade_to(upgrade_entity_type, player.score, player.is_bot())
                        {
                            return Err("cannot upgrade to provided entity type");
                        }

                        if outside_strict_area(upgrade_entity_type, entity.transform.position) {
                            return Err("cannot upgrade outside the correct area");
                        }

                        player.data.flags.upgraded = true;

                        let below_full_potential = upgrade_entity_type.data().level < score_to_level(player.score);

                        drop(player);

                        entity.change_entity_type(upgrade_entity_type, &mut world.arena, below_full_potential);

                        Ok(())
                    } else {
                        Err("cannot upgrade while not alive")
                    }
                }
            }
            info_flow_block_declassify_dynamic_integrity!(sec_lat::Label_Empty, int_lat::Label_All, param.get_dynamic_secret_label_clone(), {
                unwrap_secret(a)
            })
        }
    }

    


    fn player_changed_team(
        &mut self,
        player_tuple: &Arc<PlayerTuple<Self>>,
        old_team: Option<TeamId>,
        _players: &PlayerRepo<Server>,
    ) {
        if old_team.is_some() {
            player_tuple
                .borrow_player_mut()
                .data
                .flags
                .left_populated_team = true;
        }
    }

    fn player_left(
        &mut self,
        player_tuple: &Arc<PlayerTuple<Self>>,
        _players: &PlayerRepo<Server>,
    ) {
        let mut player = player_tuple.borrow_player_mut();
        if player.status.is_alive() {
            drop(player);
        } else {
            player.data.status = Status::Spawning;
            drop(player);
        }

        let mut player = player_tuple.borrow_player_mut();

        // Clear player's score.
        player.score = 0;

        // Delete all player's entities (efficiently, in the next update cycle).
        player.data.flags.left_game = true;
    }

    fn get_game_update(
        &self,
        player: &Arc<PlayerTuple<Self>>,
        client_data: &mut Self::ClientData,
        _players: &PlayerRepo<Server>,
    ) -> Option<Self::GameUpdate> {
        Some(
            self.world
                .get_player_complete(player)
                .into_update(self.counter, &mut client_data.loaded_chunks),
        )
    }

    fn is_alive(&self, player_tuple: &Arc<PlayerTuple<Self>>) -> bool {
        let player = player_tuple.borrow_player();
        !player.data.flags.left_game && player.data.status.is_alive()
    }

    /// update runs server ticks.
    fn tick(&mut self, context: &mut Context<Self>) {
        self.counter = self.counter.next();

        self.world.update(Ticks::ONE);

        // Needs to be called before clients receive updates, but after World::update.
        self.world.terrain.pre_update();

        if self.counter.every(Ticks::from_whole_secs(60)) {
            use std::collections::{BTreeMap, HashMap};
            use std::fs::OpenOptions;
            use std::io::{Read, Seek, Write};

            let mut count_score = HashMap::<EntityType, (usize, f32)>::new();

            for player in context.players.iter_borrow() {
                if let Status::Alive { entity_index, .. } = player.status {
                    let entity = &self.world.entities[entity_index];
                    debug_assert!(entity.is_boat());

                    let (current_count, current_score) =
                        count_score.entry(entity.entity_type).or_default();
                    *current_count += 1;

                    let level = entity.data().level;
                    let level_score = level_to_score(level);
                    let next_level_score = level_to_score(level + 1);
                    let progress = common_util::range::map_ranges(
                        player.score as f32,
                        level_score as f32..next_level_score as f32,
                        0.0..1.0,
                        false,
                    );
                    if progress.is_finite() {
                        *current_score += progress;
                    }
                }
            }

            tokio::task::spawn_blocking(move || {
                if let Err(e) = OpenOptions::new()
                    .create(true)
                    .read(true)
                    .write(true)
                    .open(&*"playtime.json")
                    .and_then(move |mut file| {
                        let mut buf = Vec::new();
                        file.read_to_end(&mut buf)?;
                        let mut old = if let Ok(old) =
                            serde_json::from_slice::<BTreeMap<EntityType, (u64, f32)>>(&buf)
                        {
                            old
                        } else {
                            error!("error loading old playtime.");
                            BTreeMap::new()
                        };

                        for (entity_type, (new_count, new_score)) in count_score {
                            if new_count > 0 {
                                let (old_count, old_score) = old.entry(entity_type).or_default();
                                *old_count = old_count.saturating_add(new_count as u64);
                                *old_score += new_score;
                            }
                        }

                        file.set_len(0)?;
                        file.rewind()?;

                        let serialized = serde_json::to_vec(&old).unwrap_or_default();
                        file.write_all(&serialized)
                    })
                {
                    error!("error logging playtime: {:?}", e);
                }
            });
        }
    }

    fn post_update(&mut self, _context: &mut Context<Self>) {
        // Needs to be after clients receive updates.
        self.world.terrain.post_update();
    }
}
