extends Area2D

var hurtbox_id = "HURTBOX"
var hurtbox_secondary_id = ""

var _collision_obj

func _ready():
	_collision_obj = get_node("CollisionShape2D")

func get_id():
	return hurtbox_id

func get_secondary_id():
	return hurtbox_secondary_id

func enable():
	_collision_obj.disabled = false

func disable():
	_collision_obj.disabled = true

func toggle():
	_collision_obj.disabled = !_collision_obj.disabled
