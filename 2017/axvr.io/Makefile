all: markdown blog site clean

site:
	./scripts/static-gen.sh

blog:
	./scripts/blog-feed.sh

markdown:
	./scripts/md-to-html.sh

clean:
	find . -name '*.html.tmp' -type f -exec rm -rv {} +

serve:
	python3 -m http.server 8000

.PHONY:	blog site markdown clean serve
