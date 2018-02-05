
;; https://wiki.secondlife.com/wiki/Emacs_LSL_Mode
;;

;; ; This software has been given to the PUBLIC DOMAIN. Use it as you see
;; fit. As usual, no warrantees are available, neither expressed nor
;; implied. You use this software at your own risk.


;; This is derived from C-mode, since LSL is so similar. There'll
;; probably be a couple gotchas as a result. I haven't tested this
;; thorougly, but it works well enough for me. If you have any
;; improvements, feel free to contact Brain Curry in game.
;;
;; (2013/07/15) Updated lsl constants
;; Kittin Ninetails 
;;
;;
;; additions for lslstddef.h and some modifications
;; Ratany Resident, 2014-01-05
;;
;; added lsl-functions:
;;
;; The list of functions originates from version 1.5.13 of Xah Lees´
;; xlsl-mode which is licenced under GPL.  Over time, I added to the
;; list.
;;
;; Ratany Resident, 2014-01-12
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; I have made a few addtions, and changes are listed in the changelog
;; in the repo (https://github.com/Ratany/lsl-repo).
;; All changes I made are licensed under GPL.
;;   Ratany Resident, 2014-03-09
;;


(defcustom lsl-keywords
  '("default" "do" "else" "for" "if" "jump" "return" "state" "while")
  "LSL keywords."
  :type 'list
  :group 'lsl-font-lock)

(defcustom lsl-types
  '("float" "integer" "key" "list" "quaternion" "rotation" "string" "vector")
  "LSL types."
  :type 'list
  :group 'lsl-font-lock)

(defcustom lsl-constants
  '("ACTIVE" "AGENT" "AGENT_ALWAYS_RUN" "AGENT_ATTACHMENTS" "AGENT_AUTOPILOT" "AGENT_AWAY" "AGENT_BUSY" "AGENT_BY_LEGACY_NAME" "AGENT_BY_USERNAME" "AGENT_CROUCHING" "AGENT_FLYING" "AGENT_IN_AIR" "AGENT_LIST_PARCEL" "AGENT_LIST_PARCEL_OWNER" "AGENT_LIST_REGION" "AGENT_MOUSELOOK" "AGENT_ON_OBJECT" "AGENT_SCRIPTED" "AGENT_SITTING" "AGENT_TYPING" "AGENT_WALKING" "ALL_SIDES" "ANIM_ON" "ATTACH_AVATAR_CENTER" "ATTACH_BACK" "ATTACH_BELLY" "ATTACH_CHEST" "ATTACH_CHIN" "ATTACH_HEAD" "ATTACH_HUD_BOTTOM" "ATTACH_HUD_BOTTOM_LEFT" "ATTACH_HUD_BOTTOM_RIGHT" "ATTACH_HUD_CENTER_1" "ATTACH_HUD_CENTER_2" "ATTACH_HUD_TOP_CENTER" "ATTACH_HUD_TOP_LEFT" "ATTACH_HUD_TOP_RIGHT" "ATTACH_LEAR" "ATTACH_LEFT_PEC" "ATTACH_LEYE" "ATTACH_LFOOT" "ATTACH_LHAND" "ATTACH_LHIP" "ATTACH_LLARM" "ATTACH_LLLEG" "ATTACH_LPEC" "ATTACH_LSHOULDER" "ATTACH_LUARM" "ATTACH_LULEG" "ATTACH_MOUTH" "ATTACH_NECK" "ATTACH_NOSE" "ATTACH_PELVIS" "ATTACH_REAR" "ATTACH_REYE" "ATTACH_RFOOT" "ATTACH_RHAND" "ATTACH_RHIP" "ATTACH_RIGHT_PEC" "ATTACH_RLARM" "ATTACH_RLLEG" "ATTACH_RPEC" "ATTACH_RSHOULDER" "ATTACH_RUARM" "ATTACH_RULEG" "AVOID_CHARACTERS" "AVOID_DYNAMIC_OBSTACLES" "AVOID_NONE" "CAMERA_ACTIVE" "CAMERA_BEHINDNESS_ANGLE" "CAMERA_BEHINDNESS_LAG" "CAMERA_DISTANCE" "CAMERA_FOCUS" "CAMERA_FOCUS_LAG" "CAMERA_FOCUS_LOCKED" "CAMERA_FOCUS_OFFSET" "CAMERA_FOCUS_THRESHOLD" "CAMERA_PITCH" "CAMERA_POSITION" "CAMERA_POSITION_LAG" "CAMERA_POSITION_LOCKED" "CAMERA_POSITION_THRESHOLD" "CHANGED_ALLOWED_DROP" "CHANGED_COLOR" "CHANGED_INVENTORY" "CHANGED_LINK" "CHANGED_MEDIA" "CHANGED_OWNER" "CHANGED_REGION" "CHANGED_REGION_START" "CHANGED_SCALE" "CHANGED_SHAPE" "CHANGED_TELEPORT" "CHANGED_TEXTURE" "CHARACTER_ACCOUNT_FOR_SKIPPED_FRAMES" "CHARACTER_AVOIDANCE_MODE" "CHARACTER_CMD_JUMP" "CHARACTER_CMD_SMOOTH_STOP" "CHARACTER_CMD_STOP" "CHARACTER_DESIRED_SPEED" "CHARACTER_DESIRED_TURN_SPEED" "CHARACTER_LENGTH" "CHARACTER_MAX_ACCEL" "CHARACTER_MAX_DECEL" "CHARACTER_MAX_SPEED" "CHARACTER_MAX_TURN_RADIUS" "CHARACTER_ORIENTATION" "CHARACTER_RADIUS" "CHARACTER_STAY_WITHIN_PARCEL" "CHARACTER_TYPE" "CHARACTER_TYPE_A" "CHARACTER_TYPE_B" "CHARACTER_TYPE_C" "CHARACTER_TYPE_D" "CHARACTER_TYPE_NONE" "CLICK_ACTION_BUY" "CLICK_ACTION_NONE" "CLICK_ACTION_OPEN" "CLICK_ACTION_OPEN_MEDIA" "CLICK_ACTION_PAY" "CLICK_ACTION_PLAY" "CLICK_ACTION_SIT" "CLICK_ACTION_TOUCH" "CONTENT_TYPE_ATOM" "CONTENT_TYPE_FORM" "CONTENT_TYPE_HTML" "CONTENT_TYPE_JSON" "CONTENT_TYPE_LLSD" "CONTENT_TYPE_RSS" "CONTENT_TYPE_TEXT" "CONTENT_TYPE_XHTML" "CONTENT_TYPE_XML" "CONTROL_BACK" "CONTROL_DOWN" "CONTROL_FWD" "CONTROL_LBUTTON" "CONTROL_LEFT" "CONTROL_ML_LBUTTON" "CONTROL_RIGHT" "CONTROL_ROT_LEFT" "CONTROL_ROT_RIGHT" "CONTROL_UP" "DATA_BORN" "DATA_NAME" "DATA_ONLINE" "DATA_PAYINFO" "DATA_RATING" "DATA_SIM_POS" "DATA_SIM_RATING" "DATA_SIM_STATUS" "DEBUG_CHANNEL" "DEG_TO_RAD" "DENSITY" "EOF" "ERR_GENERIC" "ERR_MALFORMED_PARAMS" "ERR_PARCEL_PERMISSIONS" "ERR_RUNTIME_PERMISSIONS" "ERR_THROTTLED" "ESTATE_ACCESS_ALLOWED_AGENT_ADD" "ESTATE_ACCESS_ALLOWED_AGENT_REMOVE" "ESTATE_ACCESS_ALLOWED_GROUP_ADD" "ESTATE_ACCESS_ALLOWED_GROUP_REMOVE" "ESTATE_ACCESS_BANNED_AGENT_ADD" "ESTATE_ACCESS_BANNED_AGENT_REMOVE" "FALSE" "FORCE_DIRECT_PATH" "FRICTION" "GCNP_RADIUS" "GCNP_STATIC" "GRAVITY_MULTIPLIER" "HORIZONTAL" "HTTP_BODY_MAXLENGTH" "HTTP_BODY_TRUNCATED" "HTTP_CUSTOM_HEADER" "HTTP_METHOD" "HTTP_MIMETYPE" "HTTP_PRAGMA_NO_CACHE" "HTTP_VERBOSE_THROTTLE" "HTTP_VERIFY_CERT" "INVENTORY_ALL" "INVENTORY_ANIMATION" "INVENTORY_BODYPART" "INVENTORY_CLOTHING" "INVENTORY_GESTURE" "INVENTORY_LANDMARK" "INVENTORY_NONE" "INVENTORY_NOTECARD" "INVENTORY_OBJECT" "INVENTORY_SCRIPT" "INVENTORY_SOUND" "INVENTORY_TEXTURE" "JSON_APPEND" "JSON_ARRAY" "JSON_DELETE" "JSON_FALSE" "JSON_INVALID" "JSON_NULL" "JSON_NUMBER" "JSON_OBJECT" "JSON_STRING" "JSON_TRUE" "KFM_CMD_PAUSE" "KFM_CMD_PLAY" "KFM_CMD_STOP" "KFM_COMMAND" "KFM_DATA" "KFM_FORWARD" "KFM_LOOP" "KFM_MODE" "KFM_PING_PONG" "KFM_REVERSE" "KFM_ROTATION" "KFM_TRANSLATION" "LAND_LARGE_BRUSH" "LAND_LEVEL" "LAND_LOWER" "LAND_MEDIUM_BRUSH" "LAND_NOISE" "LAND_RAISE" "LAND_REVERT" "LAND_SMALL_BRUSH" "LAND_SMOOTH" "LINK_ALL_CHILDREN" "LINK_ALL_OTHERS" "LINK_ROOT" "LINK_SET" "LINK_THIS" "LIST_STAT_GEOMETRIC_MEAN" "LIST_STAT_MAX" "LIST_STAT_MEAN" "LIST_STAT_MEDIAN" "LIST_STAT_MIN" "LIST_STAT_NUM_COUNT" "LIST_STAT_RANGE" "LIST_STAT_STD_DEV" "LIST_STAT_SUM" "LIST_STAT_SUM_SQUARES" "LOOP" "MASK_BASE" "MASK_EVERYONE" "MASK_GROUP" "MASK_NEXT" "MASK_OWNER" "NULL_KEY" "OBJECT_ATTACHED_POINT" "OBJECT_CHARACTER_TIME" "OBJECT_CREATOR" "OBJECT_DESC" "OBJECT_GROUP" "OBJECT_NAME" "OBJECT_OWNER" "OBJECT_PATHFINDING_TYPE" "OBJECT_PHANTOM" "OBJECT_PHYSICS" "OBJECT_PHYSICS_COST" "OBJECT_POS" "OBJECT_PRIM_EQUIVALENCE" "OBJECT_RENDER_WEIGHT" "OBJECT_RETURN_PARCEL" "OBJECT_RETURN_PARCEL_OWNER" "OBJECT_RETURN_REGION" "OBJECT_ROOT" "OBJECT_ROT" "OBJECT_RUNNING_SCRIPT_COUNT" "OBJECT_SCRIPT_MEMORY" "OBJECT_SCRIPT_TIME" "OBJECT_SERVER_COST" "OBJECT_STREAMING_COST" "OBJECT_TEMP_ON_REZ" "OBJECT_TOTAL_SCRIPT_COUNT" "OBJECT_UNKNOWN_DETAIL" "OBJECT_VELOCITY" "OPT_AVATAR" "OPT_CHARACTER" "OPT_EXCLUSION_VOLUME" "OPT_LEGACY_LINKSET" "OPT_MATERIAL_VOLUME" "OPT_OTHER" "OPT_STATIC_OBSTACLE" "OPT_WALKABLE" "PARCEL_COUNT_GROUP" "PARCEL_COUNT_OTHER" "PARCEL_COUNT_OWNER" "PARCEL_COUNT_SELECTED" "PARCEL_COUNT_TEMP" "PARCEL_COUNT_TOTAL" "PARCEL_DETAILS_AREA" "PARCEL_DETAILS_DESC" "PARCEL_DETAILS_GROUP" "PARCEL_DETAILS_ID" "PARCEL_DETAILS_NAME" "PARCEL_DETAILS_OWNER" "PARCEL_DETAILS_SEE_AVATARS" "PARCEL_FLAG_ALLOW_ALL_OBJECT_ENTRY" "PARCEL_FLAG_ALLOW_CREATE_GROUP_OBJECTS" "PARCEL_FLAG_ALLOW_CREATE_OBJECTS" "PARCEL_FLAG_ALLOW_DAMAGE" "PARCEL_FLAG_ALLOW_FLY" "PARCEL_FLAG_ALLOW_GROUP_OBJECT_ENTRY" "PARCEL_FLAG_ALLOW_GROUP_SCRIPTS" "PARCEL_FLAG_ALLOW_LANDMARK" "PARCEL_FLAG_ALLOW_SCRIPTS" "PARCEL_FLAG_ALLOW_TERRAFORM" "PARCEL_FLAG_LOCAL_SOUND_ONLY" "PARCEL_FLAG_RESTRICT_PUSHOBJECT" "PARCEL_FLAG_USE_ACCESS_GROUP" "PARCEL_FLAG_USE_ACCESS_LIST" "PARCEL_FLAG_USE_BAN_LIST" "PARCEL_FLAG_USE_LAND_PASS_LIST" "PARCEL_MEDIA_COMMAND_AGENT" "PARCEL_MEDIA_COMMAND_AUTO_ALIGN" "PARCEL_MEDIA_COMMAND_DESC" "PARCEL_MEDIA_COMMAND_LOOP" "PARCEL_MEDIA_COMMAND_LOOP_SET" "PARCEL_MEDIA_COMMAND_PAUSE" "PARCEL_MEDIA_COMMAND_PLAY" "PARCEL_MEDIA_COMMAND_SIZE" "PARCEL_MEDIA_COMMAND_STOP" "PARCEL_MEDIA_COMMAND_TEXTURE" "PARCEL_MEDIA_COMMAND_TIME" "PARCEL_MEDIA_COMMAND_TYPE" "PARCEL_MEDIA_COMMAND_UNLOAD" "PARCEL_MEDIA_COMMAND_URL" "PASSIVE" "PATROL_PAUSE_AT_WAYPOINTS" "PAY_DEFAULT" "PAY_HIDE" "PAYMENT_INFO_ON_FILE" "PAYMENT_INFO_USED" "PERM_ALL" "PERM_COPY" "PERM_MODIFY" "PERM_MOVE" "PERM_TRANSFER" "PERMISSION_ATTACH" "PERMISSION_CHANGE_JOINTS" "PERMISSION_CHANGE_LINKS" "PERMISSION_CHANGE_PERMISSIONS" "PERMISSION_CONTROL_CAMERA" "PERMISSION_DEBIT" "PERMISSION_OVERRIDE_ANIMATIONS" "PERMISSION_RELEASE_OWNERSHIP" "PERMISSION_REMAP_CONTROLS" "PERMISSION_RETURN_OBJECTS" "PERMISSION_SILENT_ESTATE_MANAGEMENT" "PERMISSION_TAKE_CONTROLS" "PERMISSION_TELEPORT" "PERMISSION_TRACK_CAMERA" "PERMISSION_TRIGGER_ANIMATION" "PI" "PI_BY_TWO" "PING_PONG" "PRIM_BUMP_BARK" "PRIM_BUMP_BLOBS" "PRIM_BUMP_BRICKS" "PRIM_BUMP_BRIGHT" "PRIM_BUMP_CHECKER" "PRIM_BUMP_CONCRETE" "PRIM_BUMP_DARK" "PRIM_BUMP_DISKS" "PRIM_BUMP_GRAVEL" "PRIM_BUMP_LARGETILE" "PRIM_BUMP_NONE" "PRIM_BUMP_SHINY" "PRIM_BUMP_SIDING" "PRIM_BUMP_STONE" "PRIM_BUMP_STUCCO" "PRIM_BUMP_SUCTION" "PRIM_BUMP_TILE" "PRIM_BUMP_WEAVE" "PRIM_BUMP_WOOD" "PRIM_CAST_SHADOWS" "PRIM_COLOR" "PRIM_DESC" "PRIM_FLEXIBLE" "PRIM_FULLBRIGHT" "PRIM_GLOW" "PRIM_HOLE_CIRCLE" "PRIM_HOLE_DEFAULT" "PRIM_HOLE_SQUARE" "PRIM_HOLE_TRIANGLE" "PRIM_LINK_TARGET" "PRIM_MATERIAL" "PRIM_MATERIAL_FLESH" "PRIM_MATERIAL_GLASS" "PRIM_MATERIAL_LIGHT" "PRIM_MATERIAL_METAL" "PRIM_MATERIAL_PLASTIC" "PRIM_MATERIAL_RUBBER" "PRIM_MATERIAL_STONE" "PRIM_MATERIAL_WOOD" "PRIM_MEDIA_ALT_IMAGE_ENABLE" "PRIM_MEDIA_AUTO_LOOP" "PRIM_MEDIA_AUTO_PLAY" "PRIM_MEDIA_AUTO_SCALE" "PRIM_MEDIA_AUTO_ZOOM" "PRIM_MEDIA_CONTROLS" "PRIM_MEDIA_CONTROLS_MINI" "PRIM_MEDIA_CONTROLS_STANDARD" "PRIM_MEDIA_CURRENT_URL" "PRIM_MEDIA_FIRST_CLICK_INTERACT" "PRIM_MEDIA_HEIGHT_PIXELS" "PRIM_MEDIA_HOME_URL" "PRIM_MEDIA_MAX_HEIGHT_PIXELS" "PRIM_MEDIA_MAX_URL_LENGTH" "PRIM_MEDIA_MAX_WHITELIST_COUNT" "PRIM_MEDIA_MAX_WHITELIST_SIZE" "PRIM_MEDIA_MAX_WIDTH_PIXELS" "PRIM_MEDIA_PARAM_MAX" "PRIM_MEDIA_PERM_ANYONE" "PRIM_MEDIA_PERM_GROUP" "PRIM_MEDIA_PERM_NONE" "PRIM_MEDIA_PERM_OWNER" "PRIM_MEDIA_PERMS_CONTROL" "PRIM_MEDIA_PERMS_INTERACT" "PRIM_MEDIA_WHITELIST" "PRIM_MEDIA_WHITELIST_ENABLE" "PRIM_MEDIA_WIDTH_PIXELS" "PRIM_NAME" "PRIM_OMEGA" "PRIM_PHANTOM" "PRIM_PHYSICS" "PRIM_PHYSICS_SHAPE_CONVEX" "PRIM_PHYSICS_SHAPE_NONE" "PRIM_PHYSICS_SHAPE_PRIM" "PRIM_PHYSICS_SHAPE_TYPE" "PRIM_POINT_LIGHT" "PRIM_POS_LOCAL" "PRIM_POSITION" "PRIM_ROT_LOCAL" "PRIM_ROTATION" "PRIM_SCULPT_FLAG_INVERT" "PRIM_SCULPT_FLAG_MIRROR" "PRIM_SCULPT_TYPE_CYLINDER" "PRIM_SCULPT_TYPE_MASK" "PRIM_SCULPT_TYPE_PLANE" "PRIM_SCULPT_TYPE_SPHERE" "PRIM_SCULPT_TYPE_TORUS" "PRIM_SHINY_HIGH" "PRIM_SHINY_LOW" "PRIM_SHINY_MEDIUM" "PRIM_SHINY_NONE" "PRIM_SIZE" "PRIM_SLICE" "PRIM_TEMP_ON_REZ" "PRIM_TEXGEN" "PRIM_TEXGEN_DEFAULT" "PRIM_TEXGEN_PLANAR" "PRIM_TEXT" "PRIM_TEXTURE" "PRIM_TYPE" "PRIM_TYPE_BOX" "PRIM_TYPE_CYLINDER" "PRIM_TYPE_PRISM" "PRIM_TYPE_RING" "PRIM_TYPE_SCULPT" "PRIM_TYPE_SPHERE" "PRIM_TYPE_TORUS" "PRIM_TYPE_TUBE" "PROFILE_NONE" "PROFILE_SCRIPT_MEMORY" "PSYS_PART_BF_DEST_COLOR" "PSYS_PART_BF_ONE" "PSYS_PART_BF_ONE_MINUS_DEST_COLOR" "PSYS_PART_BF_SOURCE_ALPHA" "PSYS_PART_BF_SOURCE_COLOR" "PSYS_PART_BF_ZERO" "PSYS_PART_BLEND_FUNC_DEST" "PSYS_PART_BLEND_FUNC_SOURCE" "PSYS_PART_BOUNCE_MASK" "PSYS_PART_EMISSIVE_MASK" "PSYS_PART_END_ALPHA" "PSYS_PART_END_COLOR" "PSYS_PART_END_GLOW" "PSYS_PART_END_SCALE" "PSYS_PART_FLAGS" "PSYS_PART_FOLLOW_SRC_MASK" "PSYS_PART_FOLLOW_VELOCITY_MASK" "PSYS_PART_INTERP_COLOR_MASK" "PSYS_PART_INTERP_SCALE_MASK" "PSYS_PART_MAX_AGE" "PSYS_PART_RIBBON_MASK" "PSYS_PART_START_ALPHA" "PSYS_PART_START_COLOR" "PSYS_PART_START_GLOW" "PSYS_PART_START_SCALE" "PSYS_PART_TARGET_LINEAR_MASK" "PSYS_PART_TARGET_POS_MASK" "PSYS_PART_WIND_MASK" "PSYS_SRC_ACCEL" "PSYS_SRC_ANGLE_BEGIN" "PSYS_SRC_ANGLE_END" "PSYS_SRC_BURST_PART_COUNT" "PSYS_SRC_BURST_RADIUS" "PSYS_SRC_BURST_RATE" "PSYS_SRC_BURST_SPEED_MAX" "PSYS_SRC_BURST_SPEED_MIN" "PSYS_SRC_INNERANGLE" "PSYS_SRC_MAX_AGE" "PSYS_SRC_OMEGA" "PSYS_SRC_OUTERANGLE" "PSYS_SRC_PATTERN" "PSYS_SRC_PATTERN_ANGLE" "PSYS_SRC_PATTERN_ANGLE_CONE" "PSYS_SRC_PATTERN_ANGLE_CONE_EMPTY" "PSYS_SRC_PATTERN_DROP" "PSYS_SRC_PATTERN_EXPLODE" "PSYS_SRC_TARGET_KEY" "PSYS_SRC_TEXTURE" "PU_EVADE_HIDDEN" "PU_EVADE_SPOTTED" "PU_FAILURE_DYNAMIC_PATHFINDING_DISABLED" "PU_FAILURE_INVALID_GOAL" "PU_FAILURE_INVALID_START" "PU_FAILURE_NO_NAVMESH" "PU_FAILURE_NO_VALID_DESTINATION" "PU_FAILURE_OTHER" "PU_FAILURE_PARCEL_UNREACHABLE" "PU_FAILURE_TARGET_GONE" "PU_FAILURE_UNREACHABLE" "PU_GOAL_REACHED" "PU_SLOWDOWN_DISTANCE_REACHED" "PUBLIC_CHANNEL" "PURSUIT_FUZZ_FACTOR" "PURSUIT_GOAL_TOLERANCE" "PURSUIT_INTERCEPT" "PURSUIT_OFFSET" "RAD_TO_DEG" "RC_DATA_FLAGS" "RC_DETECT_PHANTOM" "RC_GET_LINK_NUM" "RC_GET_NORMAL" "RC_GET_ROOT_KEY" "RC_MAX_HITS" "RC_REJECT_AGENTS" "RC_REJECT_LAND" "RC_REJECT_NONPHYSICAL" "RC_REJECT_PHYSICAL" "RC_REJECT_TYPES" "RCERR_CAST_TIME_EXCEEDED" "RCERR_SIM_PERF_LOW" "RCERR_UNKNOWN" "REGION_FLAG_ALLOW_DAMAGE" "REGION_FLAG_ALLOW_DIRECT_TELEPORT" "REGION_FLAG_BLOCK_FLY" "REGION_FLAG_BLOCK_TERRAFORM" "REGION_FLAG_DISABLE_COLLISIONS" "REGION_FLAG_DISABLE_PHYSICS" "REGION_FLAG_FIXED_SUN" "REGION_FLAG_RESTRICT_PUSHOBJECT" "REGION_FLAG_SANDBOX" "REMOTE_DATA_CHANNEL" "REMOTE_DATA_REPLY" "REMOTE_DATA_REQUEST" "REQUIRE_LINE_OF_SIGHT" "RESTITUTION" "REVERSE" "ROTATE" "SCALE" "SCRIPTED" "SIM_STAT_PCT_CHARS_STEPPED" "SMOOTH" "SQRT2" "STATUS_BLOCK_GRAB" "STATUS_BLOCK_GRAB_OBJECT" "STATUS_BOUNDS_ERROR" "STATUS_CAST_SHADOWS" "STATUS_DIE_AT_EDGE" "STATUS_INTERNAL_ERROR" "STATUS_MALFORMED_PARAMS" "STATUS_NOT_FOUND" "STATUS_NOT_SUPPORTED" "STATUS_OK" "STATUS_PHANTOM" "STATUS_PHYSICS" "STATUS_RETURN_AT_EDGE" "STATUS_ROTATE_X" "STATUS_ROTATE_Y" "STATUS_ROTATE_Z" "STATUS_SANDBOX" "STATUS_TYPE_MISMATCH" "STATUS_WHITELIST_FAILED" "STRING_TRIM" "STRING_TRIM_HEAD" "STRING_TRIM_TAIL" "TEXTURE_BLANK" "TEXTURE_DEFAULT" "TEXTURE_MEDIA" "TEXTURE_PLYWOOD" "TEXTURE_TRANSPARENT" "TOUCH_INVALID_FACE" "TOUCH_INVALID_TEXCOORD" "TOUCH_INVALID_VECTOR" "TRAVERSAL_TYPE" "TRAVERSAL_TYPE_FAST" "TRAVERSAL_TYPE_NONE" "TRAVERSAL_TYPE_SLOW" "TRUE" "TWO_PI" "TYPE_FLOAT" "TYPE_INTEGER" "TYPE_INVALID" "TYPE_KEY" "TYPE_ROTATION" "TYPE_STRING" "TYPE_VECTOR" "URL_REQUEST_DENIED" "URL_REQUEST_GRANTED" "VEHICLE_ANGULAR_DEFLECTION_EFFICIENCY" "VEHICLE_ANGULAR_DEFLECTION_TIMESCALE" "VEHICLE_ANGULAR_FRICTION_TIMESCALE" "VEHICLE_ANGULAR_MOTOR_DECAY_TIMESCALE" "VEHICLE_ANGULAR_MOTOR_DIRECTION" "VEHICLE_ANGULAR_MOTOR_TIMESCALE" "VEHICLE_BANKING_EFFICIENCY" "VEHICLE_BANKING_MIX" "VEHICLE_BANKING_TIMESCALE" "VEHICLE_BUOYANCY" "VEHICLE_FLAG_CAMERA_DECOUPLED" "VEHICLE_FLAG_HOVER_GLOBAL_HEIGHT" "VEHICLE_FLAG_HOVER_TERRAIN_ONLY" "VEHICLE_FLAG_HOVER_UP_ONLY" "VEHICLE_FLAG_HOVER_WATER_ONLY" "VEHICLE_FLAG_LIMIT_MOTOR_UP" "VEHICLE_FLAG_LIMIT_ROLL_ONLY" "VEHICLE_FLAG_MOUSELOOK_BANK" "VEHICLE_FLAG_MOUSELOOK_STEER" "VEHICLE_FLAG_NO_DEFLECTION_UP" "VEHICLE_FLAG_NO_FLY_UP" "VEHICLE_HOVER_EFFICIENCY" "VEHICLE_HOVER_HEIGHT" "VEHICLE_HOVER_TIMESCALE" "VEHICLE_LINEAR_DEFLECTION_EFFICIENCY" "VEHICLE_LINEAR_DEFLECTION_TIMESCALE" "VEHICLE_LINEAR_FRICTION_TIMESCALE" "VEHICLE_LINEAR_MOTOR_DECAY_TIMESCALE" "VEHICLE_LINEAR_MOTOR_DIRECTION" "VEHICLE_LINEAR_MOTOR_OFFSET" "VEHICLE_LINEAR_MOTOR_TIMESCALE" "VEHICLE_REFERENCE_FRAME" "VEHICLE_TYPE_AIRPLANE" "VEHICLE_TYPE_BALLOON" "VEHICLE_TYPE_BOAT" "VEHICLE_TYPE_CAR" "VEHICLE_TYPE_NONE" "VEHICLE_TYPE_SLED" "VEHICLE_VERTICAL_ATTRACTION_EFFICIENCY" "VEHICLE_VERTICAL_ATTRACTION_TIMESCALE" "VERTICAL" "WANDER_PAUSE_AT_WAYPOINTS" "ZERO_ROTATION" "ZERO_VECTOR")
 "LSL constants."
  :type 'list
  :group 'lsl-font-lock)

(defcustom lsl-functions
  '("llAbs" "llAcos" "llAddToLandBanList" "llAddToLandPassList" "llAdjustSoundVolume" "llAllowInventoryDrop" "llAngleBetween" "llApplyImpulse" "llApplyRotationalImpulse" "llAsin" "llAtan2" "llAttachToAvatar" "llAttachToAvatarTemp" "llAvatarOnLinkSitTarget" "llAvatarOnSitTarget" "llAxes2Rot" "llAxisAngle2Rot" "llBase64ToInteger" "llBase64ToString" "llBreakAllLinks" "llBreakLink" "llCastRay" "llCeil" "llClearCameraParams" "llClearLinkMedia" "llClearPrimMedia" "llCloseRemoteDataChannel" "llCloud" "llCollisionFilter" "llCollisionSound" "llCollisionSprite" "llCos" "llCreateCharacter" "llCreateLink" "llCSV2List" "llDeleteCharacter" "llDeleteSubList" "llDeleteSubString" "llDetachFromAvatar" "llDetectedGrab" "llDetectedGroup" "llDetectedKey" "llDetectedLinkNumber" "llDetectedName" "llDetectedOwner" "llDetectedPos" "llDetectedRot" "llDetectedTouchBinormal" "llDetectedTouchFace" "llDetectedTouchNormal" "llDetectedTouchPos" "llDetectedTouchST" "llDetectedTouchUV" "llDetectedType" "llDetectedVel" "llDialog" "llDie" "llDumpList2String" "llEdgeOfWorld" "llEjectFromLand" "llEmail" "llEscapeURL" "llEuler2Rot" "llEvade" "llExecCharacterCmd" "llFabs" "llFleeFrom" "llFloor" "llForceMouselook" "llFrand" "llGenerateKey" "llGetAccel" "llGetAgentInfo" "llGetAgentLanguage" "llGetAgentList" "llGetAgentSize" "llGetAlpha" "llGetAndResetTime" "llGetAnimation" "llGetAnimationList" "llGetAnimationOverride" "llGetAttached" "llGetBoundingBox" "llGetCameraPos" "llGetCameraRot" "llGetCenterOfMass" "llGetClosestNavPoint" "llGetColor" "llGetCreator" "llGetDate" "llGetDisplayName" "llGetEnergy" "llGetEnv" "llGetForce" "llGetFreeMemory" "llGetFreeURLs" "llGetGeometricCenter" "llGetGMTclock" "llGetHTTPHeader" "llGetInventoryCreator" "llGetInventoryKey" "llGetInventoryName" "llGetInventoryNumber" "llGetInventoryPermMask" "llGetInventoryType" "llGetKey" "llGetLandOwnerAt" "llGetLinkKey" "llGetLinkMedia" "llGetLinkName" "llGetLinkNumber" "llGetLinkNumberOfSides" "llGetLinkPrimitiveParams" "llGetListEntryType" "llGetListLength" "llGetLocalPos" "llGetLocalRot" "llGetMass" "llGetMassMKS" "llGetMaxScaleFactor" "llGetMemoryLimit" "llGetMinScaleFactor" "llGetNextEmail" "llGetNotecardLine" "llGetNumberOfNotecardLines" "llGetNumberOfPrims" "llGetNumberOfSides" "llGetObjectDesc" "llGetObjectDetails" "llGetObjectMass" "llGetObjectName" "llGetObjectPermMask" "llGetObjectPrimCount" "llGetOmega" "llGetOwner" "llGetOwnerKey" "llGetParcelDetails" "llGetParcelFlags" "llGetParcelMaxPrims" "llGetParcelMusicURL" "llGetParcelPrimCount" "llGetParcelPrimOwners" "llGetPermissions" "llGetPermissionsKey" "llGetPhysicsMaterial" "llGetPos" "llGetPrimitiveParams" "llGetPrimMediaParams" "llGetRegionAgentCount" "llGetRegionCorner" "llGetRegionFlags" "llGetRegionFPS" "llGetRegionName" "llGetRegionTimeDilation" "llGetRootPosition" "llGetRootRotation" "llGetRot" "llGetScale" "llGetScriptName" "llGetScriptState" "llGetSimStats" "llGetSimulatorHostname" "llGetSPMaxMemory" "llGetStartParameter" "llGetStaticPath" "llGetStatus" "llGetSubString" "llGetSunDirection" "llGetTexture" "llGetTextureOffset" "llGetTextureRot" "llGetTextureScale" "llGetTime" "llGetTimeOfDay" "llGetTimestamp" "llGetTorque" "llGetUnixTime" "llGetUsedMemory" "llGetUsername" "llGetVel" "llGetWallclock" "llGiveInventory" "llGiveInventoryList" "llGiveMoney" "llGodLikeRezObject" "llGround" "llGroundContour" "llGroundNormal" "llGroundRepel" "llGroundSlope" "llHTTPRequest" "llHTTPResponse" "llInsertString" "llInstantMessage" "llIntegerToBase64" "llJson2List" "llJsonGetValue" "llJsonSetValue" "llJsonValueType" "llKey2Name" "llLinkParticleSystem" "llLinkSitTarget" "llList2CSV" "llList2Float" "llList2Integer" "llList2Json" "llList2Key" "llList2List" "llList2ListStrided" "llList2Rot" "llList2String" "llList2Vector" "llListen" "llListenControl" "llListenRemove" "llListFindList" "llListInsertList" "llListRandomize" "llListReplaceList" "llListSort" "llListStatistics" "llLoadURL" "llLog" "llLog10" "llLookAt" "llLoopSound" "llLoopSoundMaster" "llLoopSoundSlave" "llMakeExplosion" "llMakeFire" "llMakeFountain" "llMakeSmoke" "llManageEstateAccess" "llMapDestination" "llMD5String" "llMessageLinked" "llMinEventDelay" "llModifyLand" "llModPow" "llMoveToTarget" "llNavigateTo" "llOffsetTexture" "llOpenRemoteDataChannel" "llOverMyLand" "llOwnerSay" "llParcelMediaCommandList" "llParcelMediaQuery" "llParseString2List" "llParseStringKeepNulls" "llParticleSystem" "llPassCollisions" "llPassTouches" "llPatrolPoints" "llPlaySound" "llPlaySoundSlave" "llPow" "llPreloadSound" "llPursue" "llPushObject" "llRefreshPrimURL" "llRegionSay" "llRegionSayTo" "llReleaseCamera" "llReleaseControls" "llReleaseURL" "llRemoteDataReply" "llRemoteDataSetRegion" "llRemoteLoadScriptPin" "llRemoveFromLandBanList" "llRemoveFromLandPassList" "llRemoveInventory" "llRemoveVehicleFlags" "llRequestAgentData" "llRequestDisplayName" "llRequestInventoryData" "llRequestPermissions" "llRequestSecureURL" "llRequestSimulatorData" "llRequestURL" "llRequestUsername" "llResetAnimationOverride" "llResetLandBanList" "llResetLandPassList" "llResetOtherScript" "llResetScript" "llResetTime" "llReturnObjectsByID" "llReturnObjectsByOwner" "llRezAtRoot" "llRezObject" "llRot2Angle" "llRot2Axis" "llRot2Euler" "llRot2Fwd" "llRot2Left" "llRot2Up" "llRotateTexture" "llRotBetween" "llRotLookAt" "llRotTarget" "llRotTargetRemove" "llRound" "llSameGroup" "llSay" "llScaleByFactor" "llScaleTexture" "llScriptDanger" "llScriptProfiler" "llSendRemoteData" "llSensor" "llSensorRemove" "llSensorRepeat" "llSetAlpha" "llSetAngularVelocity" "llSetAnimationOverride" "llSetBuoyancy" "llSetCameraAtOffset" "llSetCameraEyeOffset" "llSetCameraParams" "llSetClickAction" "llSetColor" "llSetContentType" "llSetDamage" "llSetForce" "llSetForceAndTorque" "llSetHoverHeight" "llSetInventoryPermMask" "llSetKeyframedMotion" "llSetLinkAlpha" "llSetLinkCamera" "llSetLinkColor" "llSetLinkMedia" "llSetLinkPrimitiveParams" "llSetLinkPrimitiveParamsFast" "llSetLinkTexture" "llSetLinkTextureAnim" "llSetLocalRot" "llSetMemoryLimit" "llSetObjectDesc" "llSetObjectName" "llSetObjectPermMask" "llSetParcelMusicURL" "llSetPayPrice" "llSetPhysicsMaterial" "llSetPos" "llSetPrimitiveParams" "llSetPrimMediaParams" "llSetPrimURL" "llSetRegionPos" "llSetRemoteScriptAccessPin" "llSetRot" "llSetScale" "llSetScriptState" "llSetSitText" "llSetSoundQueueing" "llSetSoundRadius" "llSetStatus" "llSetText" "llSetTexture" "llSetTextureAnim" "llSetTimerEvent" "llSetTorque" "llSetTouchText" "llSetVehicleFlags" "llSetVehicleFloatParam" "llSetVehicleRotationParam" "llSetVehicleType" "llSetVehicleVectorParam" "llSetVelocity" "llSHA1String" "llShout" "llSin" "llSitTarget" "llSleep" "llSound" "llSoundPreload" "llSqrt" "llStartAnimation" "llStopAnimation" "llStopHover" "llStopLookAt" "llStopMoveToTarget" "llStopSound" "llStringLength" "llStringToBase64" "llStringTrim" "llSubStringIndex" "llTakeCamera" "llTakeControls" "llTan" "llTarget" "llTargetOmega" "llTargetRemove" "llTeleportAgent" "llTeleportAgentGlobalCoords" "llTeleportAgentHome" "llTextBox" "llToLower" "llToUpper" "llTransferLindenDollars" "llTriggerSound" "llTriggerSoundLimited" "llUnescapeURL" "llUnSit" "llUpdateCharacter" "llVecDist" "llVecMag" "llVecNorm" "llVolumeDetect" "llWanderWithin" "llWater" "llWhisper" "llWind" "llXorBase64" "llXorBase64Strings" "llXorBase64StringsCorrect" "llGetMinScaleFactor" "llGetMaxScaleFactor" "llScaleByFactor")
  "LSL functions."
  :type 'list
  :group 'lsl-font-lock)

(defcustom lsl-events
  '("at_rot_target" "at_target" "attach" "changed" "collision" "collision_end" "collision_start" "control" "dataserver" "email" "http_request" "http_response" "land_collision" "land_collision_end" "land_collision_start" "link_message" "listen" "money" "moving_end" "moving_start" "no_sensor" "not_at_rot_target" "not_at_target" "object_rez" "on_rez" "path_update" "remote_data" "run_time_permissions" "sensor" "state_entry" "state_exit" "timer" "touch" "touch_end" "touch_start" "transaction_result")
  "LSL events."
  :type 'list
  :group 'lsl-font-lock)


;; <modifications for lslstddef.h>
;;
(defcustom lsl-cpp
  '("define" "endif" "include" "undef" "ifdef" "ifndef")
  "cpp keywords"
  :type 'list
  :group 'lsl-font-lock)

(defcustom lsl-macros
  '("afootell" "aftell" "afwis" "AgentIsHere" "apf" "arst" "BbxCenterPos" "BbxCornerPos" "BbxCorners" "BbxFrontBotLeftCorner" "BbxFrontBotRightCorner" "BbxFrontTopLeftCorner" "BbxFrontTopRightCorner" "BbxRearBotLeftCorner" "BbxRearBotRightCorner" "BbxRearTopLeftCorner" "BbxRearTopRightCorner" "BbxScale" "Begstr" "ClrStatus" "CompStatus" "concat" "concats" "continue" "CSVStrX" "DEBUGmsg" "DEBUGmsg0" "DEBUGmsg1" "DEBUGmsg2" "DEBUGmsg3" "DEBUGmsgLIB" "Dist2Line" "Endstr" "Enlist" "ERRORmsg" "event" "foreach" "fprintl" "fprintlt" "GetMenuChannel" "GetPrimText" "GLPP" "guard" "HasInventory" "HasStatus" "IfAttached" "IfEnlist" "IfElseEnlist" "IfMessage" "IfNotEnlist" "IfNStatus" "IfNStatusDo" "IfStatus" "IfStatusDo" "imp" "impa" "InFrontX" "InFrontXThisRoot" "InFrontXVecR" "InFrontXVecRr" "InFrontY" "InFrontYVecRr" "InFrontZ" "Instr" "InvIsCopy4Owner" "LoopDown" "LoopUp" "MultVec" "MultVec2" "NoPin" "NotInventory" "NotOnlst" "NotStatus" "ObjIsCopy4Owner" "ObjectMaybeNotAround" "Onlst" "opf" "parst" "PrimPercentUsed" "PrimsFree" "PrimsFree" "ProtocolData" "ProtocolID" "ProtocolSimpleData" "RemoteDesc" "RemoteGroup" "RemoteIsAttached" "RemoteName" "RemoteOwner" "RemoteOwnerName" "RemotePhantom" "RemotePhysCost" "RemotePos" "RemotePrimEqv" "RemoteRScriptCount" "RemoteRoot" "RemoteRootPos" "RemoteRootRot" "RemoteRot" "RemoteScriptTime" "RemoteServerCost" "RemoteStreamCost" "RemoteTScriptCount" "RemoteVelocity" "retif" "RPos2Slurl" "SameOwner" "sSlurlyfy" "boolSameOwnerOrGroup" "SameParcel" "SetStatus" "SLPPF" "SoundAlert" "SoundDelete" "SoundInvop" "SoundPing" "SoundRezzing" "SoundTyping" "sprintl" "sprintlt" "starst" "Stridx" "stringify" "stringifylt" "Strlen" "Strtrunc" "StrX" "Substr" "tarst" "TruncateDialogButton" "TruncateDialogList" "LstIdx" "Len" "FMin" "FMax" "FVecMax" "FVecMin" "MFree" "Max" "Min" "PosOffset" "Signof" "tif" "unless" "UnStatus" "until" "VecBetween" "VecFabsSum" "VecMult" "VecRound" "VecSubFAbs" "VecSum" "VecWithin" "Velocity" "VSameParcel" "VVSameParcel" "when" "wpf"
    )
  "cpp macros"
  :type 'list
  :group 'lsl-font-lock)


(defcustom lsl-faketypes
  '("bool" "int" "void" "const"
    )
  "cpp faked data types"
  :type 'list
  :group 'lsl-font-lock)
;;
;; </modifications for lslstddef.h>


;; (defvar lsl-builtin-regexp "\\<ll[A-Z][a-zA-Z]+\\>")
(defvar lsl-builtin-regexp (regexp-opt lsl-functions 'words))
(defvar lsl-constant-regexp (regexp-opt lsl-constants 'words))
(defvar lsl-cpp-regexp (regexp-opt lsl-cpp 'words))
(defvar lsl-event-regexp (regexp-opt lsl-events 'words))
(defvar lsl-faketypes-regexp (regexp-opt lsl-faketypes 'words))
(defvar lsl-keywords-regexp (regexp-opt lsl-keywords 'words))
(defvar lsl-macros-regexp (regexp-opt lsl-macros 'words))
(defvar lsl-type-regexp (regexp-opt lsl-types 'words))

(defvar lsl-font-lock-keywords
  `(,lsl-keywords-regexp
    (,lsl-type-regexp . font-lock-type-face)
    (,lsl-faketypes-regexp . font-lock-type-face)
    (,lsl-cpp-regexp . font-lock-preprocessor-face)
    (,lsl-macros-regexp . font-lock-warning-face)
;;    ("\\(\\<\\S +\\>\\)\\s *(" 1 font-lock-function-name-face t)
    (,lsl-event-regexp 0 font-lock-builtin-face t)
    (,lsl-builtin-regexp 0 font-lock-builtin-face t)
    (,(concat lsl-type-regexp "\\W+\\(\\<\\w+\\>\\)") 2 font-lock-variable-name-face)
    (,(concat lsl-faketypes-regexp "\\W+\\(\\<\\w+\\>\\)") 2 font-lock-variable-name-face)
    (,lsl-constant-regexp . font-lock-constant-face)
    )
  "Syntax-highlighting regexps for lsl-mode.")


(setq lsl-builtin-regexp nil)
(setq lsl-constant-regexp nil)
(setq lsl-cpp-regexp nil)
(setq lsl-event-regexp nil)
(setq lsl-faketypes-regexp nil)
(setq lsl-keywords-regexp nil)
(setq lsl-macros-regexp nil)
(setq lsl-type-regexp nil)


;;
;; very much based on the GPL version of xlsl-mode:
;;
;; functions to look up stuff on the wiki
;;
;;


(defcustom lsl-reference-url "http://lslwiki.net/lslwiki/wakka.php?wakka="
  "URL for LSL reference website.
The value is used by `lsl-lookup-lsl-ref'.
The value should be one of:
“http://lslwiki.net/lslwiki/wakka.php?wakka=”
“http://wiki.secondlife.com/wiki/”"
  :type '(string)
  :group 'lsl-mode)


(defcustom lsl-reference-url2 "http://wiki.secondlife.com/wiki/"
  "URL for LSL reference website.
The value is used by `lsl-lookup-lsl-ref2'.
The value can be any of:
“http://wiki.secondlife.com/wiki/”
“http://en.wikipedia.org/wiki/”
“http://www.google.com/search?q=”"
  :type '(string)
  :group 'lsl-mode)


(defun lsl-browse-url (url)
  "This is a wrapper function for lsl-lookup-lsl-site.  It uses
the built-in web browser for the lookups to display the web page
in a different window."
  (pop-to-buffer "*eww*")
  (eww url))


(defun lsl-lookup-lsl-site (site-url)
  "Attempt to look up the word at point on a web page.
site-url is an url string.
This is a internal function.
This function is called by lsl-lookup-lsl-ref and lsl-lookup-lsl-ref2.
It uses the lsl-browse-url function to start the web browser."
  (let (pos1 pos2 bds meat myurl)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))

    (setq meat
          (if (and pos1 pos2 )
              (buffer-substring-no-properties pos1 pos2)
            nil ))

    (if meat
        (progn
          (setq meat (replace-regexp-in-string " " "%20" meat))
          (setq myurl (concat site-url meat))
          (lsl-browse-url myurl))
      (progn (ding) (message "No word under cursor to lookup.")))))


(defun lsl-lookup-lsl-ref ()
  "Look up current word in LSL ref site in a browser.
If there is a text selection (a phrase), lookup that phrase.
Set variable lsl-reference-url if you want to change the url used.

See also `lsl-lookup-lsl-ref2'."
  (interactive)
  (lsl-lookup-lsl-site lsl-reference-url))


(defun lsl-lookup-lsl-ref2 ()
  "Look up current word in LSL ref site in a browser.
If there is a text selection (a phrase), lookup that phrase.
Set variable lsl-reference-url2 if you want to change the url used.

See also `lsl-lookup-lsl-ref'."
  (interactive)
  (lsl-lookup-lsl-site lsl-reference-url2))


;;
;; above are functions to look up stuff on the wiki
;;


;;
;; lsl-mode specifc functions to use hi-lock-mode
;;

(defun lsl-hi-lock-message (unused)
  "Return non-nil to allow setting hi-lock-mode without asking."
  t)

;;
;; functions for when reloading lsl-mode
;;


(defun lsl-modeset-all-buffers ()
  "For all buffers visiting files with names ending in .lsl, enable lsl-mode.

When changes are made to lsl-mode, it can be advisable to reload
the mode.  When lsl-mode is unloaded, it´s also disabled for all
buffers and needs to be re-enabled once the mode has been
reloaded.

This function goes through all buffers and enables lsl-mode for
each buffer, depending on the name of the file the buffer is
visiting.  When lsl-mode is already enabled for such a buffer,
the buffers´ mode remains unchanged."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((filenameofbuffer (buffer-file-name buffer)))
      (unless (eq nil filenameofbuffer)
	(if (string-match ".*\.lsl$" filenameofbuffer)
	    (with-current-buffer buffer (set-auto-mode-0 'lsl-mode t)))))))


(defun lsl-mode-auto-enable ()
  "automatically enable lsl-mode for some files when visiting,
unless already enabled"
  (unless (assoc-default "\\.lsl$" auto-mode-alist)
    (add-to-list 'auto-mode-alist '("\\.lsl$" . lsl-mode))))


;;
;; astyle buffer ...
;;
(defun lsl-astyle-buffer (mcl)
  "Ask for code length and pipe all buffer contents through
  astyle and replace with the output from astyle, then do
  whitespace-cleanup."
  (interactive "nMax code length: ")
  (shell-command-on-region (point-min) (point-max) (format "astyle --max-code-length=%d" mcl) nil t 'shell-command-default-error-buffer t)
  (whitespace-cleanup))


;; simplify indenting
;;
(defun lsl-indent-defun ()
  "Indent the function point is currently within."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (point) (mark))))


;;
;; keymap for lsl-mode
;;

(defvar lsl-mode-map nil "keymap for lsl-mode")

(setq lsl-mode-map (make-sparse-keymap))
(define-key lsl-mode-map (kbd "C-c h") 'lsl-lookup-lsl-ref2)
(define-key lsl-mode-map (kbd "C-c a") 'lsl-astyle-buffer)
(define-key lsl-mode-map (kbd "<C-f1>") 'recompile)
(define-key lsl-mode-map (kbd "<C-S-f1>") 'compile)

;; put indenting on F6
;;
(define-key lsl-mode-map (kbd "<f6>") 'lsl-indent-defun)


;; c-mode and c++-mode apparently use basically the same code.  The
;; most noticable difference is that M-x comment-region uses //
;; comments intead of block comments.
;;
;; (define-derived-mode lsl-mode c-mode "LSL"
(define-derived-mode lsl-mode c++-mode "LSL"
  "Major mode for editing LSL.
\\{lsl-mode-map}"
  (setq font-lock-defaults '((lsl-font-lock-keywords) nil nil))
  (lsl-mode-auto-enable)
  (auto-complete-mode)
  (hi-lock-mode 1)
  (setq hi-lock-file-patterns-policy 'lsl-hi-lock-message))


(provide 'lsl-mode)
