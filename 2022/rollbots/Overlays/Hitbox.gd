extends Area2D

var hitbox_id = "HITBOX"
var hitbox_secondary_id = ""
var hitbox_damage = 0

var _collision_obj

func _ready():
	_collision_obj = get_node("CollisionShape2D")

func get_id():
	return hitbox_id

func get_secondary_id():
	return hitbox_secondary_id

func get_damage():
	return hitbox_damage

func enable():
	_collision_obj.disabled = false

func disable():
	_collision_obj.disabled = true

func toggle():
	_collision_obj.disabled = !_collision_obj.disabled
