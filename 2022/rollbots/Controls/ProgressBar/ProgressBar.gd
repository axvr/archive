extends StaticBody2D

const red_texture = preload("res://Controls/ProgressBar/Red.png")
const blue_texture = preload("res://Controls/ProgressBar/Blue.png")

var _sprite

func _ready():
	_sprite = get_node("Sprite")

func set_colour(colour):
	_sprite.set_texture(red_texture if colour == "RED" else blue_texture)

func set_progress(progress):
	if progress < 0: progress = 0
	elif progress > 100: progress = 100
	_sprite.frame = round(progress * 0.32)
