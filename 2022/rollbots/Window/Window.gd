extends Node2D

var _timer
var _countdown

var _level

var _player1attackdie
var _player1defencedie
var _player1powerdie

var _player2attackdie
var _player2defencedie
var _player2powerdie

var _player1health
var _player2health

var _player1power
var _player2power


func _set_colour(colour, objs):
	for obj in objs:
		obj.set_colour(colour)


func _ready():
	_timer = get_node("GameScreen/Timer")
	_countdown = get_node("GameScreen/Countdown")

	_level = get_node("GameScreen/Level")

	_player1attackdie = get_node("GameScreen/Player1Stats/AttackDie")
	_player1defencedie = get_node("GameScreen/Player1Stats/DefenceDie")
	_player1powerdie = get_node("GameScreen/Player1Stats/PowerDie")
	_set_colour("RED", [_player1attackdie, _player1defencedie, _player1powerdie])

	_player2attackdie = get_node("GameScreen/Player2Stats/AttackDie")
	_player2defencedie = get_node("GameScreen/Player2Stats/DefenceDie")
	_player2powerdie = get_node("GameScreen/Player2Stats/PowerDie")
	_set_colour("BLUE", [_player2attackdie, _player2defencedie, _player2powerdie])

	_player1health = get_node("GameScreen/Player1Vitals/HealthBar")
	_player2health = get_node("GameScreen/Player2Vitals/HealthBar")
	_set_colour("RED", [_player1health, _player2health])

	_player1power = get_node("GameScreen/Player1Vitals/PowerBar")
	_player2power = get_node("GameScreen/Player2Vitals/PowerBar")
	_set_colour("BLUE", [_player1power, _player2power])

	start_round()


const NUM_ROUNDS = 3
const ROUND_DURATION = 60

var _secondsPassed
var _currentRound


func _update_counter():
	var string = String(ROUND_DURATION - _secondsPassed)
	_countdown.text = "0" + string if string.length() == 1 else string

func start_round():
	_timer.stop()
	_timer.wait_time = 1
	_secondsPassed = 0
	_currentRound = 1
	_update_counter()
	_timer.start()

func _on_Timer_timeout():
	_secondsPassed += 1
	_update_counter()

	_player1attackdie.randomise()
	_player1defencedie.randomise()
	_player1powerdie.randomise()
	_player2attackdie.randomise()
	_player2defencedie.randomise()
	_player2powerdie.randomise()

	if _secondsPassed >= ROUND_DURATION:
		_secondsPassed = 0
		_level.reset()


func _process(_delta):
	_player1health.set_progress(100 - _level.player1_health)
	_player2health.set_progress(100 - _level.player2_health)
	_player1power.set_progress(100 - _level.player1_power)
	_player2power.set_progress(100 - _level.player2_power)
	
	if _level.player1_health <= 0 || _level.player2_health <= 0:
		print("Game over")
