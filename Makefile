
.PHONY: hup-pack hup-doc

# can call w/ e.g.
# $ HACKAGE_USER='MY_HACKAGE_USERID' PASSWORD='MY_PASSWORD' make hup-pack

hup-pack:
	if [ -z "$$PASSWORD" -o -z "$$HACKAGE_USER" ]; then \
		echo need to set hackage userid and password!; \
		exit 1; \
	fi
	hup packboth --candidate --user="$$HACKAGE_USER"

hup-doc:
	if [ -z "$$PASSWORD" -o -z "$$HACKAGE_USER" ]; then \
		echo need to set hackage userid and password!; \
		exit 1; \
	fi
	hup docboth --candidate --user="$$HACKAGE_USER"
