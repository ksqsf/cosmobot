.PHONY: echo-plugin clean-echo-plugin

PLUGIN_DIR ?= plugins
ECHO_BUNDLE := $(PLUGIN_DIR)/echo

# Build a self-contained Python zipapp so the sandbox never depends on host
# site-packages, virtualenvs, or user directories.
echo-plugin:
	mkdir -p "$(ECHO_BUNDLE)"
	cp examples/plugins/echo/config.toml "$(ECHO_BUNDLE)/config.toml"
	tmp_dir="$$(mktemp -d)"; \
	trap 'rm -rf "$$tmp_dir"' EXIT; \
	cp examples/plugins/echo/echo "$$tmp_dir/__main__.py"; \
	cp -R cosmobot-plugin-python/src/cosmobot_plugin "$$tmp_dir/cosmobot_plugin"; \
	python3 -m zipapp "$$tmp_dir" -p '/usr/bin/python3' -o "$(ECHO_BUNDLE)/echo"; \
	chmod 0755 "$(ECHO_BUNDLE)/echo"

clean-echo-plugin:
	rm -rf "$(ECHO_BUNDLE)"
