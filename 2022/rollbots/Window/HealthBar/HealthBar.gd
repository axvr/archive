extends StaticBody2D

const red_texture = preload("res://Window/HealthBar/Red.png")
const blue_texture = preload("res://Window/HealthBar/Blue.png")

var _sprite

func _ready():
	_sprite = get_node("Sprite")

func set_colour(colour):
	_sprite.set_texture(red_texture if colour == "RED" else blue_texture)

func set_health(health):
	print(health)
	if health < 0: health = 0
	elif health > 100: health = 100
	_sprite.frame = round((100 - health) * 0.32)
