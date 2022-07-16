extends Node2D

var _timer
var _level
var _die1
var _die2
var _health1
var _health2

func _ready():
	_timer = get_node("Timer")
	_level = get_node("Level")
	_die1 = get_node("Player1Die1")
	_die2 = get_node("Player2Die2")
	_health1 = get_node("HealthBar1")
	_health2 = get_node("HealthBar2")
	start()

const NUM_ROUNDS = 3
const ROUND_DURATION = 60

var _secondsPassed
var _currentRound

func start():
	_timer.stop()
	_timer.wait_time = 1
	_secondsPassed = 0
	_currentRound = 1
	_die1.set_colour("RED")
	_health1.set_colour("RED")
	_timer.start()

func _on_Timer_timeout():
	_secondsPassed += 1
	_die1.randomise()
	_die2.randomise()
	if _secondsPassed >= ROUND_DURATION:
		_secondsPassed = 0
		_level.reset()

func _process(delta):
	_health1.set_health(_level.player1_health)
	_health2.set_health(_level.player2_health)
