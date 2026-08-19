extends KinematicBody2D

const red_texture = preload("res://Player/Red.png")
const blue_texture = preload("res://Player/Blue.png")

var _sprite
var _hurtbox
var _hitbox

const BASE_FRAME = 0
const ALT_FRAME = 1
const HIT_FRAME = 2
const PUNCH_FRAME = 4
const BATTERED_FRAME = 8
const BROKEN_FRAME = 16

const DEATH_FRAME_1 = 15
const DEATH_FRAME_2 = 16
const DEATH_FRAME_3 = 17
var _death_at = 0
var _is_death = false

const ALT_FRAME_DURATION = 100
const HIT_FRAME_DURATION = 100
const PUNCH_FRAME_DURATION = 100
const DEATH_FRAME_DURATION = 150

var _is_alt = false
var _alt_at = 0
var _is_hit = false
var _hit_at = 0
var _is_punch = false
var _punch_at = 0
var _is_battered = false
var _is_broken = false

var _red_frame_lookup = [
	 0,  # Normal
	 1,  # NormalAlt
	 2,  # NormalHit
	 2,  # NormalHitAlt
	 3,  # NormalPunch
	 3,  # NormalPunchAlt,
	 4,  # NormalPunchHit,
	 4,  # NormalPunchHitAlt
	 5,  # Battered
	 6,  # BatteredAlt
	 7,  # BatteredHit
	 7,  # BatteredHitAlt
	 8,  # BatteredPunch
	 8,  # BatteredPunchAlt
	 9,  # BatteredPunchHit,
	 9,  # BatteredPunchHitAlt
	10,  # Broken
	11,  # BrokenAlt
	12,  # BrokenHit
	12,  # BrokenHitAlt
	13,  # BrokenPunch
	13,  # BrokenPunchAlt
	14,  # BrokenPunchHit
	14,  # BrokenPunchHitAlt
]

var _blue_frame_lookup = [
	 0,  # Normal
	 1,  # NormalAlt
	 2,  # NormalHit
	 2,  # NormalHitAlt
	 9,  # NormalPunch
	 9,  # NormalPunchAlt,
	10,  # NormalPunchHit,
	10,  # NormalPunchHitAlt
	 3,  # Battered
	 4,  # BatteredAlt
	 5,  # BatteredHit
	 5,  # BatteredHitAlt
	11,  # BatteredPunch
	11,  # BatteredPunchAlt
	12,  # BatteredPunchHit,
	12,  # BatteredPunchHitAlt
	 6,  # Broken
	 7,  # BrokenAlt
	 8,  # BrokenHit
	 8,  # BrokenHitAlt
	13,  # BrokenPunch
	13,  # BrokenPunchAlt
	14,  # BrokenPunchHit
	14,  # BrokenPunchHitAlt
]

func update_frame():
	var frame = BASE_FRAME
	if _is_alt: frame += ALT_FRAME
	if _is_hit: frame += HIT_FRAME
	if _is_punch: frame += PUNCH_FRAME
	if _is_broken: frame += BROKEN_FRAME
	elif _is_battered: frame += BATTERED_FRAME
	var table = _red_frame_lookup if player_id == 1 else _blue_frame_lookup
	_sprite.frame = table[frame]

func reset_frame():
	_is_alt = false
	_is_hit = false
	_is_punch = false
	_is_battered = false
	_is_broken = false
	_is_death = false
	update_frame()

func check_frame_updates():
	var ticks = OS.get_ticks_msec()
	if _is_death:
		if ticks >= _death_at + (DEATH_FRAME_DURATION * 3):
			_sprite.frame = DEATH_FRAME_3
		elif ticks >= _death_at + (DEATH_FRAME_DURATION * 2):
			_sprite.frame = DEATH_FRAME_2
		elif ticks >= _death_at + DEATH_FRAME_DURATION:
			_sprite.frame = DEATH_FRAME_1
	else:
		if _is_hit && ticks >= (_hit_at + HIT_FRAME_DURATION):
			_is_hit = false
		if ticks >= (_alt_at + ALT_FRAME_DURATION):
			_is_alt = !_is_alt
			_alt_at = ticks
		if _is_punch && ticks >= (_punch_at + PUNCH_FRAME_DURATION):
			_is_punch = false
		update_frame()


func _ready():
	_sprite = get_node("Sprite")
	_hurtbox = get_node("Hurtbox")
	_hitbox = get_node("Hitbox")

var player_id = 1

func reset(keep_health = false):
	if !keep_health:
		HEALTH = 100
		reset_frame()
	_hurtbox.hurtbox_secondary_id = player_id
	_hitbox.hitbox_secondary_id = player_id
	_hitbox.hitbox_damage = ATTACK
	end_punch()
	_can_attack = true
	POWER = MAX_POWER


var LEFT_BTN
var RIGHT_BTN
var THRUST_BTN
var ATTACK_BTN

func set_control_scheme(control_mappings):
	LEFT_BTN = control_mappings["LEFT"]
	RIGHT_BTN = control_mappings["RIGHT"]
	THRUST_BTN = control_mappings["THRUST"]
	ATTACK_BTN = control_mappings["ATTACK"]

func set_colour(colour):
	_sprite.set_texture(red_texture if colour == "RED" else blue_texture)


var HEALTH = 100
var DEFENCE = 1
var ATTACK = 2
var POWER = 0
const MAX_POWER = 100
var POWER_RECHARGE = 1

func set_stats(stats):
	DEFENCE = stats["DEFENCE"]
	ATTACK = stats["ATTACK"]
	POWER_RECHARGE = stats["POWER"]
	POWER = MAX_POWER
	_hitbox.hitbox_damage = ATTACK


func take_hit(damage):
	HEALTH -= (damage - (damage * ((DEFENCE + 2) / 10)))
	if HEALTH < 67: _is_battered = true
	if HEALTH < 34: _is_broken = true
	_is_hit = true
	_hit_at = OS.get_ticks_msec()
	if HEALTH <= 0:
		_death_at = OS.get_ticks_msec()
		_is_death = true
		set_movement(false)

func _on_Hurtbox_area_entered(area):
	if area.get_id() == "HITBOX" && area.get_secondary_id() != player_id:
		take_hit(area.hitbox_damage)

const ATTACK_COOLDOWN = 400
const ATTACK_ANIMATION_DURATION = 100
var _can_attack = true
var _attack_cooldown_at
var _clear_attack_animation_at = 0
var _attack_animation_active = false

func punch():
	_attack_animation_active = true
	_hitbox.enable()
	var ticks = OS.get_ticks_msec()
	_attack_cooldown_at = ticks + ATTACK_COOLDOWN
	_is_punch = true
	_punch_at = ticks
	_can_attack = false

func end_punch():
	_hitbox.disable()
	_attack_animation_active = false

func handle_attack():
	var ticks = OS.get_ticks_msec()
	if !_is_punch:
		end_punch()

	if _can_attack:
		if Input.is_physical_key_pressed(ATTACK_BTN):
			punch()
	if !_can_attack && ticks > _attack_cooldown_at:
		_can_attack = true


const Direction = preload("res://Direction.gd")

var direction = Direction.Left

func _get_x_input():
	var left = -1 if Input.is_physical_key_pressed(LEFT_BTN) else 0
	var right = 1 if Input.is_physical_key_pressed(RIGHT_BTN) else 0
	return left + right


func _next_direction(current_direction):
	var x = _get_x_input()
	if x < 0:   return Direction.Left
	elif x > 0: return Direction.Right
	else:       return current_direction

func set_direction(new_direction):
	if direction != new_direction:
		scale.x = -1
		direction = new_direction


const MAX_SPEED = 150
const ACCELERATION = 400
const GRAVITY = 150
const THRUST = 400

var _velocity = Vector2.ZERO

func _next_input_velocity(current_velocity, delta):
	var vector = Vector2.ZERO
	vector.x = _get_x_input() * MAX_SPEED
	vector.y = GRAVITY - (THRUST if Input.is_physical_key_pressed(THRUST_BTN) && POWER > 2 else 0)
	if Input.is_physical_key_pressed(THRUST_BTN) && POWER > 2:
		POWER = max(0, POWER - (60 * delta))
	return current_velocity.move_toward(vector, ACCELERATION * delta)

func _set_velocity(new_velocity):
	_velocity = move_and_slide(new_velocity)


var _movementAllowed = true

func set_movement(enabled):
	_movementAllowed = enabled


func second_passed():
	if _velocity.y >= 0 && POWER < MAX_POWER && !Input.is_physical_key_pressed(THRUST_BTN):
		POWER = min(MAX_POWER, POWER + (2 * ((10 * (POWER_RECHARGE / 10.0)) + 3)))


func _process(delta):
	if _movementAllowed:
		_set_velocity(_next_input_velocity(_velocity, delta))
		set_direction(_next_direction(direction))
		handle_attack()
	check_frame_updates()
