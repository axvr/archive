all: markdown blog site clean

site:
	./scripts/static-gen.sh

blog:
	./scripts/blog-feed.sh

markdown:
	./scripts/md-to-content.sh

clean:
	find . -name '*.content.html' -type f -exec rm -rv {} +

serve:
	python3 -m http.server 8000

.PHONY:	blog site markdown clean serve
