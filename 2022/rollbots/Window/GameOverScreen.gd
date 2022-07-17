extends Node2D

var _winnerLabel

func _ready():
	_winnerLabel = get_node("WinnerLabel")

func set_winner(winner):
	_winnerLabel.text = winner + " Wins"
