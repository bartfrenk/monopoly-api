.PHONY: build-res build-docs help run build-schemas clean-stack \
		clean-target clean-all run purge-db build-samples

APP_NAME=Monopoly Server

DB_PORT_HOST=8001
SERVER_ADDRESS=localhost:8000

.DEFAULT_GOAL:=help

help: ## Show this help.
	@echo "---"
	@echo "Makefile for ${APP_NAME}"
	@echo "---"
	@fgrep -h "##" $(MAKEFILE_LIST) | \
	fgrep -v fgrep | sed -e 's/## */##/' | column -t -s##

build-all: ## Build everything
build-all: build-images build-docs

build-images: ## Build all docker images
	@stack image container
	@docker-compose build store

build-docs: ## Generate html documentation
build-docs: target/docs/monopoly.html

build-res: ## Generate target files from resource files
build-res: build-schemas build-samples build-docs

build-docker-pg: ## Build the PostgreSQL docker image.
	docker build -t bartfrenk/monopoly-pg -f Dockerfile.pg .

up-docker-pg: ## Run monopoly-pg in the background. Create it if it doesn't exist.
	@docker create -p ${DB_PORT_HOST}:5432 --name monopoly-pg \
			bartfrenk/monopoly-pg 2> /dev/null; \
	docker start monopoly-pg

restart-and-deploy: ## Stops Docker Compose, rebuild images and launches Docker again.
                    ## Then loads the database.
	docker-compose down -v
	git pull
	make build-images
	docker-compose up -d
	sleep 5
	make load-all

# TODO: autogenerate targets
build-samples: target/samples/locations.json \
			   target/samples/team-1.json \
			   target/samples/questions.json

clean-all: ## Remove all generated files
clean-all: clean-target clean-stack

clean-stack: ## Removes files generated by Haskell Stack
	@stack clean

clean-target: ## Remove all files generated from resources
	@rm -rf target

load-all: ## Loads all resources into the server
load-all: load-locations load-questions load-teams

load-locations: ## Load locations in YAML to the server
load-locations: target/samples/locations.json
	@curl ${SERVER_ADDRESS}/locations -H 'Content-Type: application/json' \
		 -d @$<

load-questions: ## Loads questions in YAML to the server
load-questions: target/samples/questions.json
	@curl ${SERVER_ADDRESS}/questions -H 'Content-Type: application/json' \
		 -d @$<

load-teams: ## Loads teams into the server
load-teams: target/samples/team-1.json target/samples/team-2.json
	@curl ${SERVER_ADDRESS}/teams -H 'Content-Type: application/json' \
		 -d @target/samples/team-1.json
	@curl ${SERVER_ADDRESS}/teams -H 'Content-Type: application/json' \
		 -d @target/samples/team-2.json

target/docs/monopoly.html: res/docs/monopoly.raml target/docs \
						   build-samples build-schemas
	@raml2html $< > $@

target/samples/%.json: res/samples/%.yaml target/samples
	@python -c \
		'import sys, yaml, json; json.dump(yaml.load(sys.stdin), sys.stdout, indent=4)' \
		< $< > $@

target/docs:
	@mkdir -p target/docs

target/samples:
	@mkdir -p target/samples

##

api-cors: ## Test whether server responds to CORS request
	@curl -H "Origin: http://example.com" \
		 -H "Accept: */*" \
		 -H "Accept-Encoding: gzip, deflate, sdch" \
		 -H "Access-Control-Request-Headers: content-type" \
		 -H "Access-Control-Request-Method: POST" \
		 =H "Origin: http://localhost:4000" \
		 -X OPTIONS --verbose \
		 "${SERVER_ADDRESS}/locations"

api-visit: ## Simulate a visit
	@curl -X 'POST' ${SERVER_ADDRESS}/locations/${site}/visit/${team} | jq

api-sync-team: ## Simulate synching the team status
	@curl -d '{"latitude": 21.0, "longitude": 21.0}' ${SERVER_ADDRESS}/teams/${team}/sync \
		  -H 'Content-Type: application/json' | jq

api-sync-with-site: ## Sync team that is visiting a specific site
	@curl -d '{"latitude": 21.0, "longitude": 21.0}' \
		  ${SERVER_ADDRESS}/teams/${team}/sync?siteToken=${site} \
		  -H 'Content-Type: application/json'

api-buy-no-question: ## Simulate buying a location without having to answer a question
	@curl  ${SERVER_ADDRESS}/locations/${site}/buy/${team} -d "\"NoQuestionToken 10\"" \
         -H 'Content-Type: application/json' | jq

api-buy-question: ## Simulate buying a location with a question chance card
	@curl  ${SERVER_ADDRESS}/locations/${site}/buy/${team} \
		 -d "\"QuestionToken ${question} ${answer}\"" \
         -H 'Content-Type: application/json' | jq

api-to-jail: ## Simulate drawing a ToJail chance card
	@curl -X 'POST' -v ${SERVER_ADDRESS}/teams/${team}/to-jail

api-to-start: ## Simulate drawing a ToStart chance card
	@curl -X 'POST' -v ${SERVER_ADDRESS}/teams/${team}/to-start?amount=${amount}

api-recover-team: ## Recover team details
	@curl -v -d "{\"token\": ${team}}" ${SERVER_ADDRESS}/teams \
		  -H 'Content-Type: application/json'

api-list-locations: ## Return loaded sites
	@curl ${SERVER_ADDRESS}/locations | jq

api-game-overview: ## Returns game status
	@curl ${SERVER_ADDRESS}/teams/status

api-owned: ## Returns tokens of sites owned by team
	@curl -v ${SERVER_ADDRESS}/teams/${team}/owned

##
