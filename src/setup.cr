# Cardano Faucet
# - Setup and config
#

ANONYMOUS_ACCESS         = ENV.fetch("ANONYMOUS_ACCESS", "TRUE")
FAUCET_API_KEY_PATH      = ENV.fetch("FAUCET_API_KEY_PATH", "/var/lib/cardano-faucet/faucet.apikey")
FAUCET_LOG_LEVEL         = ENV.fetch("CRYSTAL_LOG_LEVEL", "INFO")
FAUCET_LOG_SOURCES       = ENV.fetch("CRYSTAL_LOG_SOURCES", "*")
FAUCET_LISTEN_PORT       = ENV.fetch("FAUCET_LISTEN_PORT", "8091").to_i
FAUCET_PASSPHRASE_PATH   = ENV.fetch("FAUCET_SECRET_PASSPHRASE_PATH", "/var/lib/cardano-faucet/faucet.passphrase")
FAUCET_WALLET_ID_PATH    = ENV.fetch("FAUCET_WALLET_ID_PATH", "/var/lib/cardano-faucet/faucet.id")
LOVELACES_TO_GIVE_ANON   = ENV.fetch("LOVELACES_TO_GIVE_ANON", "1000000000").to_u64
LOVELACES_TO_GIVE_APIKEY = ENV.fetch("LOVELACES_TO_GIVE_APIKEY", "1000000000").to_u64
SECONDS_BETWEEN_REQUESTS = ENV.fetch("SECONDS_BETWEEN_REQUESTS", "86400").to_i
USE_BYRON_WALLET         = ENV.fetch("USE_BYRON_WALLET", "TRUE");
WALLET_LISTEN_PORT       = ENV.fetch("WALLET_LISTEN_PORT", "8090").to_i
WALLET_API               = ENV.fetch("WALLET_API", "http://localhost:#{WALLET_LISTEN_PORT}/v2")

FAUCET_WALLET_ID  = readFile(FAUCET_WALLET_ID_PATH)
SECRET_PASSPHRASE = readFile(FAUCET_PASSPHRASE_PATH)
API_KEYS          = readKeys(FAUCET_API_KEY_PATH)

API_KEY_LEN             = 32
API_KEY_COMMENT_MAX_LEN = 64

API_URI = URI.parse("#{WALLET_API}")
HEADERS = HTTP::Headers{"Content-Type" => "application/json; charset=utf-8"}

MIN_METRICS_PERIOD = 10

STDOUT.sync = true
Log.setup_from_env
