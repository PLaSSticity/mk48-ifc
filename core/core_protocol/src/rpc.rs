// SPDX-FileCopyrightText: 2021 Softbear, Inc.
// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::dto::*;
use crate::id::*;
use crate::name::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;

// Client requests are from the browser to the core service.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ClientRequest {
    AcceptPlayer {
        player_id: PlayerId,
    },
    AssignCaptain {
        player_id: PlayerId,
    },
    CreateInvitation,
    CreateSession {
        alias: Option<PlayerAlias>,
        game_id: GameId,
        invitation_id: Option<InvitationId>,
        language_pref: Option<LanguageId>,
        referer: Option<Referer>,
        region_pref: Option<RegionId>,
        saved_session_tuple: Option<(ArenaId, SessionId)>,
    },
    CreateTeam {
        team_name: TeamName,
    },
    KickPlayer {
        player_id: PlayerId,
    },
    MuteSender {
        enable: bool,
        player_id: PlayerId,
    },
    QuitTeam,
    RejectPlayer {
        player_id: PlayerId,
    },
    RequestJoin {
        team_id: TeamId,
    },
    SendChat {
        message: String,
        whisper: bool,
    },
    Trace {
        message: String,
    },
}

// Metric requests are from the reporting interface to the core service.
#[derive(Debug, Serialize, Deserialize)]
pub enum MetricRequest {
    RequestMetrics,
}

// Server requests are from the game server to the core service.
#[derive(Debug, Serialize, Deserialize)]
pub enum ServerRequest {
    BotRequest {
        session_id: SessionId,
        request: ClientRequest,
    },
    // This should be called when a web socket it dropped regardless of whether client is playing.
    DropSession {
        session_id: SessionId,
    },
    SetStatus {
        session_id: SessionId,
        #[serde(default)]
        location: Option<Location>,
        #[serde(default)]
        score: Option<u32>,
    },
    StartArena {
        game_id: GameId,
        region: RegionId,
        rules: Option<RulesDto>,
        saved_arena_id: Option<ArenaId>,
        server_addr: ServerAddr,
    },
    StartPlay {
        session_id: SessionId,
    },
    StopArena,
    StopPlay {
        session_id: SessionId,
        // In the future, may also add Option<ExitState>
    },
    ValidateSession {
        session_id: SessionId,
    },
}

#[cfg_attr(feature = "server", derive(actix::Message))]
#[cfg_attr(feature = "server", rtype(result = "()"))]
#[derive(Serialize, Deserialize)]
pub enum ClientUpdate {
    CaptainAssigned {
        player_id: PlayerId,
    },
    ChatSent {
        player_id: Option<PlayerId>,
    },
    InvitationCreated {
        invitation_id: InvitationId,
    },
    JoinRequested {
        team_id: TeamId,
    },
    // The following is for the team captain only.
    JoinersUpdated {
        added: Arc<[PlayerId]>,
        removed: Arc<[PlayerId]>,
    },
    // The following is for the joiner only.
    JoinsUpdated {
        added: Arc<[TeamId]>,
        removed: Arc<[TeamId]>,
    },
    // The leaderboard contains high score players, but not teams, for prior periods.
    LeaderboardUpdated {
        leaderboard: Arc<[LeaderboardDto]>,
        period: PeriodId,
    },
    // The liveboard contains high score players and their teams in the current game.
    LiveboardUpdated {
        liveboard: Arc<[LiveboardDto]>,
    },
    MessagesUpdated {
        added: Arc<[MessageDto]>,
    },
    PlayerAccepted {
        player_id: PlayerId,
    },
    PlayerKicked {
        player_id: PlayerId,
    },
    PlayerRejected {
        player_id: PlayerId,
    },
    PlayersUpdated {
        added: Arc<[PlayerDto]>,
        removed: Arc<[PlayerId]>,
    },
    RegionsUpdated {
        added: Arc<[RegionDto]>,
        removed: Arc<[RegionId]>,
    },
    SenderMuted {
        enable: bool,
        player_id: PlayerId,
    },
    SessionCreated {
        arena_id: ArenaId,
        language: LanguageId,
        region: RegionId,
        server_addr: ServerAddr,
        session_id: SessionId,
    },
    TeamCreated {
        team_id: TeamId,
    },
    TeamQuit,
    TeamsUpdated {
        added: Arc<[TeamDto]>,
        removed: Arc<[TeamId]>,
    },
    Traced,
}

#[cfg_attr(feature = "server", derive(actix::Message))]
#[cfg_attr(feature = "server", rtype(result = "()"))]
#[cfg_attr(feature = "client", derive(actix::Message))]
#[cfg_attr(feature = "client", rtype(result = "()"))]
#[derive(Serialize, Deserialize)]
pub enum MetricUpdate {
    MetricsRequested {
        metrics: HashMap<GameId, MetricsDto>,
    },
}

#[cfg_attr(feature = "server", derive(actix::Message))]
#[cfg_attr(feature = "server", rtype(result = "()"))]
#[derive(Debug, Serialize, Deserialize)]
pub enum ServerUpdate {
    ArenaStarted {
        arena_id: ArenaId,
    },
    ArenaStopped,
    BotReady {
        player_id: PlayerId,
        session_id: SessionId,
    },
    MembersChanged {
        changes: Arc<[MemberDto]>,
    },
    PlayStarted {
        player_id: PlayerId,
        // In the future, may also add Option<ExitState>
    },
    PlayStopped,
    SessionDropped,
    SessionValid {
        elapsed: u32,
        player_id: PlayerId,
        score: u32,
    },
    StatusSet,
}
