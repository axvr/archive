extends Node2D

var _roundLabel

func _ready():
	_roundLabel = get_node("RoundLabel")

func set_round(num):
	var num2 = String(num)
	if num < 10: num2 = "0" + num2
	_roundLabel.text = "Round " + num2
