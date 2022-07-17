extends StaticBody2D

const red_texture = preload("res://Controls/Die/Red.png")
const blue_texture = preload("res://Controls/Die/Blue.png")

var _sprite

func _ready():
	_sprite = get_node("Sprite")

func set_colour(colour):
	_sprite.set_texture(red_texture if colour == "RED" else blue_texture)

func set_number(num):
	if num < 1: num = 1
	elif num > 6: num = 6
	_sprite.frame = num - 1

func random_number():
	var rolls = range(0, 6)
	rolls.shuffle()
	return rolls[0] + 1

func randomise():
	var num = random_number()
	set_number(num)
	return num
