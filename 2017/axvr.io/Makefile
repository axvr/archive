all: markdown blog site clean

site:
	./scripts/static-gen.sh

blog:
	./scripts/blog-feed.sh

markdown:
	./scripts/md-to-content.sh

clean:
	find . -name '*.content.html' -type f -exec rm -rv {} +

.PHONY:	blog site markdown clean
