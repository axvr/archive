extends Node2D

var _timer
var _countdown

var _level
var _gameScreen
var _roundScreen
var _gameOverScreen
var _pauseScreen
var _creditsScreen
var _homeScreen

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
	randomize()
	_timer = get_node("GameScreen/Timer")
	_countdown = get_node("GameScreen/Countdown")

	_homeScreen = get_node("HomeScreen")
	_homeScreen.visible = true
	_level = get_node("GameScreen/Level")
	_gameScreen = get_node("GameScreen")
	_gameScreen.visible = false
	_roundScreen = get_node("RoundScreen")
	_roundScreen.visible = false
	_gameOverScreen = get_node("GameOverScreen")
	_gameOverScreen.visible = false
	_pauseScreen = get_node("PauseScreen")
	_pauseScreen.visible = false
	_creditsScreen = get_node("CreditsScreen")
	_creditsScreen.visible = false

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


const NUM_ROUNDS = 3
const ROUND_DURATION = 30

var _secondsPassed = 0
var _currentRound = 1


func _update_counter():
	var string = String(ROUND_DURATION - _secondsPassed)
	_countdown.text = "0" + string if string.length() == 1 else string

func _randomise_player_stats(keep_health = false):
	# TODO: stat "equaliser"
	_level.reset_player1({
		"ATTACK": _player1attackdie.randomise(),
		"DEFENCE": _player1defencedie.randomise(),
		"POWER": _player1powerdie.randomise()
	}, keep_health)
	_level.reset_player2({
		"ATTACK": _player2attackdie.randomise(),
		"DEFENCE": _player2defencedie.randomise(),
		"POWER": _player2powerdie.randomise()
	}, keep_health)

func start_game():
	_gameScreen.visible = true
	_currentRound = 0
	_gameOverScreen.visible = false
	_gameScreen.visible = true
	_randomise_player_stats(false)
	start_round()

const ROUND_SCREEN_DURATION = 2

func start_round():
	_level.set_player_movement(false)
	_timer.stop()
	_currentRound += 1
	_secondsPassed = 0
	_roundScreen.set_round(_currentRound)
	_roundScreen.visible = true
	_update_counter()
	_randomise_player_stats(true)
	_timer.start()

func back_home():
	_timer.stop()
	_level.set_player_movement(false)
	_gameScreen.visible = false
	_gameOverScreen.visible = false
	_roundScreen.visible = false
	_pauseScreen.visible = false
	_creditsScreen.visible = false
	_homeScreen.visible = true

func _on_Timer_timeout():
	_secondsPassed += 1
	if _roundScreen.visible:
		if _secondsPassed >= ROUND_SCREEN_DURATION:
			_roundScreen.visible = false
			_secondsPassed = 0
			_level.set_player_movement(true)
	elif !_gameOverScreen.visible:
		_update_counter()
		_level.second_passed()
		if _level.player1_health <= 0 || _level.player2_health <= 0:
			_gameOverScreen.set_winner("Red" if _level.player2_health <= 0 else "Blue")
			_gameOverScreen.visible = true
			_secondsPassed = 0
		if _secondsPassed >= ROUND_DURATION:
			start_round()

func pause_game():
	_level.set_player_movement(false)
	_timer.stop()
	_pauseScreen.visible = true

func resume_game():
	_pauseScreen.visible = false
	if !_roundScreen.visible:
		_level.set_player_movement(true)
	_timer.start()

func _process(_delta):
	_player1health.set_progress(100 - _level.player1_health)
	_player2health.set_progress(100 - _level.player2_health)
	_player1power.set_progress(100 - _level.player1_power)
	_player2power.set_progress(100 - _level.player2_power)
	if _gameScreen.visible && !_gameOverScreen.visible && Input.is_physical_key_pressed(KEY_ESCAPE):
		pause_game()

func _on_StartButton_pressed():
	start_game()

func _on_QuitButton_pressed():
	get_tree().notification(MainLoop.NOTIFICATION_WM_QUIT_REQUEST)

func _on_ResumeButton_pressed():
	resume_game()

func _on_BackToMenuButton_pressed():
	back_home()

func _on_RematchButton_pressed():
	start_game()

func _on_CreditsButton_pressed():
	_homeScreen.visible = false
	_creditsScreen.visible = true
