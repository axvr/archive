extends Node2D

const Direction = preload("res://Direction.gd")

var _player1
var _player2

var player1_health = 0
var player2_health = 0

var player1_power = 0
var player2_power = 0

func _ready():
	_player1 = get_node("Player1")
	_player1.set_colour("RED")
	_player2 = get_node("Player2")
	_player2.set_colour("BLUE")
	_player1.player_id = 1
	_player2.player_id = 2
	_player1.set_control_scheme({
		"LEFT": KEY_A,
		"RIGHT": KEY_D,
		"THRUST": KEY_W,
		"ATTACK": KEY_SPACE
	})
	_player2.set_control_scheme({
		"LEFT": KEY_LEFT,
		"RIGHT": KEY_RIGHT,
		"THRUST": KEY_UP,
		"ATTACK": KEY_ENTER
	})
	reset()

func _reset_player1():
	_player1.reset()
	_player1.set_direction(Direction.Right)
	_player1.set_position(Vector2(96, 176))

func _reset_player2():
	_player2.reset()
	_player2.set_direction(Direction.Left)
	_player2.set_position(Vector2(416, 176))

func reset():
	_reset_player1()
	_reset_player2()

func _process(_delta):
	player1_health = _player1.HEALTH
	player2_health = _player2.HEALTH
	player1_power = _player1.POWER
	player2_power = _player2.POWER
