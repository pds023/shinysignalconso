# Prepare the source bundle manifest for Posit Connect (no publication).
# Run from the project root after installing app dependencies and rsconnect.
source("dev/write_manifest.R", encoding = "UTF-8")

# Recommended: commit manifest.json and publish with Connect's Import from Git.
# Set title, description and thumbnail in Settings > General; see README.md.
# Configure SIGNALCONSO_* and AWS environment variables on the server.

# Alternative: direct deployment to an already configured Connect account.
# Uncomment and supply your own server/account; do not use this to update a
# content item already managed through Git-backed deployment.
# rsconnect::deployApp(
#   appDir = ".",
#   manifestPath = "manifest.json",
#   appName = "signalconso-observatoire",
#   appTitle = config::get("app_title", file = "inst/golem-config.yml"),
#   server = "YOUR_CONNECT_SERVER",
#   account = "YOUR_CONNECT_ACCOUNT"
# )
