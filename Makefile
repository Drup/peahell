NAME=peahell
DOCDIR=.gh-pages

.PHONY: all test clean doc gh-pages

all:
	dune build @install

test:
	dune runtest

clean:
	dune clean

doc:
	dune build @doc

$(DOCDIR)/.git:
	mkdir -p $(DOCDIR)
	cd $(DOCDIR) && (\
		git clone -b gh-pages git@github.com:Drup/$(NAME).git . \
	)

gh-pages: $(DOCDIR)/.git
	git -C $(DOCDIR) pull
	git -C $(DOCDIR) add --all 
	git -C $(DOCDIR) commit -a -m "gh-page updates"
	git -C $(DOCDIR) push origin gh-pages
