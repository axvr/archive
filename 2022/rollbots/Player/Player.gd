extends KinematicBody2D

const red_texture = preload("res://Player/Red.png")
const blue_texture = preload("res://Player/Blue.png")

var _sprite
var _hurtbox
var _hitbox

func _ready():
	_sprite = get_node("Sprite")
	_hurtbox = get_node("Hurtbox")
	_hitbox = get_node("Hitbox")

var player_id = 1

func reset():
	HEALTH = 100
	DEFENCE = 0
	ATTACK = 2
	_hurtbox.hurtbox_secondary_id = player_id
	_hitbox.hitbox_secondary_id = player_id
	_hitbox.hitbox_damage = ATTACK
	end_punch()
	_can_attack = true


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


var HEALTH
var DEFENCE
var ATTACK
var FUEL

func set_stats(stats):
	DEFENCE = stats["DEFENCE"]
	ATTACK = stats["ATTACK"]
	FUEL = stats["FUEL"]
	_hitbox.hitbox_damage = ATTACK

func take_hit(damage):
	HEALTH -= (damage + 1) - DEFENCE
	print(">>>", HEALTH)
	if HEALTH <= 0:
		print("Dead!")
		emit_signal("dead", player_id)

func _on_Hurtbox_area_entered(area):
	if area.get_id() == "HITBOX" && area.get_secondary_id() != player_id:
		take_hit(area.hitbox_damage)

const ATTACK_COOLDOWN = 400
const CLEAR_ATTACK_ANIMATION_AFTER = 100
var _can_attack = true
var _attack_cooldown_at
var _clear_attack_animation_at = 0
var _attack_animation_active = false

func punch():
	_sprite.frame = 1
	_attack_animation_active = true
	_hitbox.enable()
	var ticks = OS.get_ticks_msec()
	_attack_cooldown_at = ticks + ATTACK_COOLDOWN
	_clear_attack_animation_at = ticks + CLEAR_ATTACK_ANIMATION_AFTER
	_can_attack = false

func end_punch():
	_sprite.frame = 0
	_hitbox.disable()
	_attack_animation_active = false

func handle_attack():
	var ticks = OS.get_ticks_msec()
	if _attack_animation_active:
		if ticks > _clear_attack_animation_at:
			end_punch()
	else:
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

# FIXME: this code is slightly glitchy and not perfect.
func _next_input_velocity(current_velocity, delta):
	var vector = Vector2.ZERO
	vector.x = _get_x_input() * MAX_SPEED
	vector.y = GRAVITY - (THRUST if Input.is_physical_key_pressed(THRUST_BTN) else 0)
	return current_velocity.move_toward(vector, ACCELERATION * delta)

func _set_velocity(new_velocity):
	move_and_slide(new_velocity)
	_velocity = new_velocity


func _process(delta):
	_set_velocity(_next_input_velocity(_velocity, delta))
	set_direction(_next_direction(direction))
	handle_attack()
