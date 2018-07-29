all: markdown blog site

site:
	./scripts/static-gen.sh

blog:
	./scripts/blog-feed.sh

markdown:
	./scripts/md-to-content.sh

.PHONY:	blog site markdown
