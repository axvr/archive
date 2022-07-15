extends KinematicBody2D

# Called when the node enters the scene tree for the first time.
func _ready():
	pass

# TODO: different controls needed for each player.

const MAX_SPEED = 150
const ACCELERATION = 400
const FRICTION = 500
const GRAVITY = 30  # TODO

var velocity = Vector2.ZERO

# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	var input_vector = Vector2.ZERO
	
	var left = -1 if Input.is_physical_key_pressed(KEY_A) else 0
	var right = 1 if Input.is_physical_key_pressed(KEY_D) else 0
	input_vector.x  = left + right
	
	if input_vector == Vector2.ZERO:
		input_vector = velocity.move_toward(Vector2.ZERO, FRICTION * delta)
	else:
		input_vector = velocity.move_toward(input_vector * MAX_SPEED, ACCELERATION * delta)
	
	velocity = move_and_slide(input_vector)
