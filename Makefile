

res/%.json: res/%.yaml
	@python -c \
		'import sys, yaml, json; json.dump(yaml.load(sys.stdin), sys.stdout, indent=4)' \
		< $< > $@

run:
	@stack exec monopoly-server

build-res: res/locations.json res/team.json

clean-res:
	@rm -f res/*.json

