require "http/server"
require "http/client"
require "http/params"
require "uri"
require "socket"
require "json"
require "file_utils"
require "db"
require "sqlite3"
require "logger"

FAUCET_LOG_LEVEL              = ENV.fetch("FAUCET_LOG_LEVEL", "INFO")
FAUCET_LISTEN_PORT            = ENV.fetch("FAUCET_LISTEN_PORT", "8091").to_i
FAUCET_WALLET_ID_PATH         = ENV.fetch("FAUCET_WALLET_ID_PATH", "/var/lib/cardano-faucet/faucet.id")
FAUCET_PASSPHRASE_PATH        = ENV.fetch("FAUCET_SECRET_PASSPHRASE_PATH", "/var/lib/cardano-faucet/faucet.passphrase")
FAUCET_API_KEY_PATH           = ENV.fetch("FAUCET_API_KEY_PATH", "/var/lib/cardano-faucet/faucet.apikey")
WALLET_LISTEN_PORT            = ENV.fetch("WALLET_LISTEN_PORT", "8090").to_i
WALLET_API                    = ENV.fetch("WALLET_API", "http://localhost:#{WALLET_LISTEN_PORT}/v2")
LOVELACES_TO_GIVE             = ENV.fetch("LOVELACES_TO_GIVE", "1000000000").to_u64
SECONDS_BETWEEN_REQUESTS      = ENV.fetch("SECONDS_BETWEEN_REQUESTS", "86400").to_i

STDOUT.sync = true
LOG = Logger.new(STDOUT)
case FAUCET_LOG_LEVEL
  when "FATAL"
    LOG.level = Logger::FATAL
  when "ERROR"
    LOG.level = Logger::ERROR
  when "WARN"
    LOG.level = Logger::WARN
  when "INFO"
    LOG.level = Logger::INFO
  when "DEBUG"
    LOG.level = Logger::DEBUG
  when "UNKNOWN"
    LOG.level = Logger::UNKNOWN
  else
    raise "Unknown log level: #{FAUCET_LOG_LEVEL}"
end

FAUCET_WALLET_ID              = readFile(FAUCET_WALLET_ID_PATH)
SECRET_PASSPHRASE             = readFile(FAUCET_PASSPHRASE_PATH)
API_KEYS                      = readKeys(FAUCET_API_KEY_PATH)

API_KEY_LEN                   = 32
API_KEY_COMMENT_MAX_LEN       = 64

API_URI                       = URI.parse("#{WALLET_API}")
HEADERS                       = HTTP::Headers{ "Content-Type" => "application/json; charset=utf-8" }

MIN_METRICS_PERIOD            = 10


def readFile(file)
  if (File.exists?(file) && !File.empty?(file))
    return File.read(file).strip
  else
    return ""
  end
end

def readKeys(file)
  apiKeys = Hash(String, String).new
  f = readFile(file)
  if f != ""
    f.split("\n").each.with_index do |i, c|
      if i.lstrip =~ /^#/ || i =~ /^\s*$/
        next
      end
      keyFields = i.split(" ")
      if !(keyFields[0] =~ /^[A-Za-z0-9]{#{API_KEY_LEN}}$/)
        msg = "API Key file \"#{file}\", line \"#{c + 1}\", key \"#{keyFields[0]}\" " \
              "is not a #{API_KEY_LEN} char alphanumeric"
        LOG.error(msg)
        raise(msg)
      end
      if keyFields.size == 1
        apiKeys[keyFields[0]] = "Uncommented"
      else
        apiKeys[keyFields[0]] = keyFields[1..-1].join(" ")[0..API_KEY_COMMENT_MAX_LEN]
      end
    end
  end
  apiKeys
end

module Cardano

  class Wallet
    def self.apiPost(path, body)
      client = HTTP::Client.new(API_URI)
      client.post(path, HEADERS, body) do |response|
        result = response.body_io.gets
        statusCode = response.status_code
        statusMessage = response.status_message
        LOG.debug("response: #{response}")
        if response.success?
          LOG.debug("statusCode: #{statusCode}")
          LOG.debug("statusMessage: #{statusMessage}")
          LOG.debug("Result: #{result}")
        else
          LOG.error("statusCode: #{statusCode}")
          LOG.error("statusMessage: #{statusMessage}")
          LOG.error("Result: #{result}")
        end
        return { response, result }
      end
    end

    def self.apiGet(path)
      client = HTTP::Client.new(API_URI)
      client.get(path) do |response|
        result = response.body_io.gets
        statusCode = response.status_code
        statusMessage = response.status_message
        LOG.debug("response: #{response}")
        if response.success?
          LOG.debug("statusCode: #{statusCode}")
          LOG.debug("statusMessage: #{statusMessage}")
          LOG.debug("Result: #{result}")
        else
          LOG.error("statusCode: #{statusCode}")
          LOG.error("statusMessage: #{statusMessage}")
          LOG.error("Result: #{result}")
        end
        return { response, result }
      end
    end
  end

  class Account
    def self.for_wallet(wallet)
      #value = JSON.parse(`cardano-wallet-byron wallet get #{wallet}`)["balance"]["available"]["quantity"].as_i64

      path = "#{WALLET_API}/byron-wallets/#{wallet}"
      LOG.debug("Fetching available wallet value; curl equivalent:")
      LOG.debug("curl -v #{path}")
      response = Wallet.apiGet(path)
      value = 0
      if response[0].success?
        value = JSON.parse(response[1].not_nil!)["balance"]["available"]["quantity"].as_i64
      end
      return value.not_nil!
    end
  end

  class Txs
    def self.for_wallet(wallet)
      #counter = JSON.parse(`cardano-wallet-byron transaction list #{wallet}`)

      path = "#{WALLET_API}/byron-wallets/#{wallet}/transactions"
      LOG.debug("Fetching wallet transaction count; curl equivalent:")
      LOG.debug("curl -v #{path} | jq '. | length'")
      response = Wallet.apiGet(path)
      if response[0].success?
        counter = JSON.parse(response[1].not_nil!)
      end
      return counter.not_nil!.size
    end
  end

  class Fees
    def self.for_tx(wallet, dest_addr)
      # fees = JSON.parse(`cardano-wallet-byron transaction fees #{wallet} --payment #{LOVELACES_TO_GIVE}@#{dest_addr}`)["amount"]["quantity"].as_i

      path = "#{WALLET_API}/byron-wallets/#{wallet}/payment-fees"
      body = %({"payments":[{"address":"#{dest_addr}","amount":{"quantity":#{LOVELACES_TO_GIVE},"unit":"lovelace"}}]})
      LOG.debug("Fetching transaction fee estimate; curl equivalent:")
      LOG.debug("curl -vX POST #{path} -H 'Content-Type: application/json; charset=utf-8' -d '#{body}' --http1.1")
      response = Wallet.apiPost(path, body)
      fees = 0
      if response[0].success?
        fees = JSON.parse(response[1].not_nil!)["amount"]["quantity"].as_i
      end
      return fees.not_nil!
    end
  end

  class Settings
    JSON.mapping(
      genesis_block_hash: String
    )

    def self.get
      #from_json(`cardano-wallet-byron network parameters latest`)

      path = "#{WALLET_API}/network/parameters/latest"
      LOG.debug("Fetching network parameters; curl equivalent:")
      LOG.debug("curl -v #{path}")
      response = Wallet.apiGet(path)
      from_json(response[1].not_nil!)
    end
  end

  class Faucet
    TIME_BETWEEN_REQUESTS = SECONDS_BETWEEN_REQUESTS.seconds

    alias Allow = Bool
    alias Response = {status: HTTP::Status, body: SendFundsResult | NotFoundResult | RateLimitResult | String}
    alias SendFundsResult = {success: Bool, amount: UInt64, fee: Int32, txid: String}
    alias RateLimitResult = {statusCode: Int32, error: String, message: String, retryAfter: Time}
    alias NotFoundResult = {statusCode: Int32, error: String, message: String}

    @amount : UInt64
    @settings : Settings
    @db : DB::Database
    @lastMetricsTime : Time
    @lastMetrics : String

    def initialize(@amount, @db)
      @settings = Settings.get
      @db.scalar "PRAGMA journal_mode=WAL"
      @lastMetricsTime = Time.utc
      @lastMetrics = ""
      migrations
    end

    # recursively increment version until no migrations are left
    # Note that we cannot `DROP COLUMN`
    # see https://www.sqlite.org/faq.html#q11
    def migrations
      version = @db.scalar("PRAGMA main.user_version").as(Int64)

      case version
      when 0
        migrate <<-SQL
          CREATE TABLE IF NOT EXISTS requests (
            host VARCHAR UNIQUE PRIMARY KEY NOT NULL,
            seen TIME NOT NULL DEFAULT CURRENT_TIMESTAMP
          )
        SQL
      when 1
        migrate <<-SQL
          ALTER TABLE requests ADD COLUMN hash VARCHAR NOT NULL DEFAULT ''
        SQL
      else
        return
      end

      @db.exec "PRAGMA main.user_version = #{version + 1}"
      migrations
    end

    def migrate(statement : String)
      @db.exec statement
    end

    def on_request(context : HTTP::Server::Context) : Response
      case context.request.method
      when "POST"
        on_post(context)
      when "GET"
        on_get(context)
      else
        on_not_found
      end
    rescue error
      on_error(error)
    end

    def on_error(error)
      msg = { statusCode: 500,
              error:      HTTP::Status::INTERNAL_SERVER_ERROR.to_s,
              message:    error.to_s,
            }
      LOG.debug(msg.to_json)
      {
        status: HTTP::Status::INTERNAL_SERVER_ERROR,
        body:   msg
      }
    end

    def on_not_found
      msg = { statusCode: 404,
              error:      "Not Found",
              message:    "No URL found"
            }
      LOG.debug(msg.to_json)
      {
        status: HTTP::Status::NOT_FOUND,
        body:   msg
      }
    end

    def on_post(context : HTTP::Server::Context) : Response
      rateExempted = false

      match = context.request.path.match(%r(/send-money/([^/]+)))
      return on_not_found unless match

      if context.request.query_params.has_key?("apiKey")
        if API_KEYS.has_key? context.request.query_params["apiKey"]
          rateExempted = true
          LOG.debug("Rate exempted by API key with comment: " \
                    "\"#{API_KEYS[context.request.query_params["apiKey"]]}\"")
        end
      end

      rate_limiter = limit_rate(
        real_ip(context.request.headers["X-Real-IP"]?) ||
        context.request.remote_address)

      if rate_limiter[:allow] || rateExempted
        on_send_money(match[1])
      else
        on_too_many_requests(rate_limiter[:try_again])
      end
    end

    def on_get(context : HTTP::Server::Context) : Response
      match = context.request.path.match(%r(/metrics/?$))
      return on_not_found unless match

      on_metrics
    end

    def on_metrics : Response
      now = Time.utc
      metricsDelta = now - @lastMetricsTime

      if metricsDelta.seconds > MIN_METRICS_PERIOD || @lastMetrics == ""
        result = Account.for_wallet(FAUCET_WALLET_ID)
        @lastMetricsTime = now
        @lastMetrics = "cardano_faucet_metrics_value_available #{result}"
      else
        LOG.debug("Metrics were fetched #{@lastMetricsTime} with a refresh period \
                   of #{MIN_METRICS_PERIOD}s; serving previous result...")
      end
      {
        status:  HTTP::Status::OK,
        body:    @lastMetrics,
      }
    end

    def on_send_money(to_address : String) : Response
      result = send_funds(to_address)
      {
        status: HTTP::Status::OK,
        body:   result,
      }
    end

    def on_too_many_requests(try_again : Time) : Response
      delta = (try_again - Time.utc).total_seconds.to_i
      msg = { statusCode: 429,
              error:      "Too Many Requests",
              message:    "Try again in #{delta} seconds",
              retryAfter: try_again.to_utc
            }
      LOG.debug(msg.to_json)
      {
        status: HTTP::Status::TOO_MANY_REQUESTS,
        body:   msg
      }
    end

    def real_ip(header : String) : String
      "#{header}:443"
    end

    def real_ip(header : Nil) : Nil
    end

    def limit_rate(ip : Nil) : NamedTuple(time: Time, allow: Bool, try_again: Time)
      {
        time:      Time.utc,
        allow:     false,
        try_again: Time.utc + TIME_BETWEEN_REQUESTS,
      }
    end

    def limit_rate(remote : String) : NamedTuple(time: Time, allow: Bool, try_again: Time)
      ip = Socket::IPAddress.parse("tcp://#{remote}").address
      allow_after = Time.utc - TIME_BETWEEN_REQUESTS

      found = nil

      select_seen(ip, allow_after) do |rs|
        rs.each do
          seen = rs.read(Time)
          found = {
            time:      seen,
            allow:     false,
            try_again: seen + TIME_BETWEEN_REQUESTS,
          }
        end
      end

      if found
        return found.not_nil!
      end

      @db.exec(<<-SQL, ip, Time.utc, @settings.genesis_block_hash)
        INSERT OR REPLACE INTO requests VALUES (?, ?, ?)
      SQL

      {
        time:      Time.utc,
        allow:     true,
        try_again: allow_after,
      }
    end

    def select_seen(ip, allow_after, &block : DB::ResultSet -> Nil)
      @db.query(<<-SQL, ip, allow_after, @settings.genesis_block_hash, &block)
        SELECT seen FROM requests
          WHERE host = ?
          AND seen > ?
          AND hash = ?
          LIMIT 1
      SQL
    end

    def sh(cmd, args)
      raise "Failed to execute #{cmd} #{args.join(" ")}" unless system(cmd, args)
    end

    def sh!(cmd)
      result = `#{cmd}`
      raise "Failed to execute #{cmd}" unless $?.success?
      result.strip
    end

    def send_funds(address : String) : SendFundsResult
      digest = OpenSSL::Digest.new("SHA256")
      digest.update([
        address,
        Process.pid,
        Time.utc,
        @settings.genesis_block_hash,
      ].join)

      tx_fees = Fees.for_tx(FAUCET_WALLET_ID, address)
      amount_with_fees = @amount + tx_fees

      source_account_value = Account.for_wallet(FAUCET_WALLET_ID)
      source_tx_counter = Txs.for_wallet(FAUCET_WALLET_ID)

      LOG.info("The transaction will be posted to the blockchain with genesis hash: #{@settings.genesis_block_hash}")

      if source_account_value < amount_with_fees
          LOG.error("Not enough funds in faucet account, only #{source_account_value} left")
          raise "Not enough funds in faucet account, only #{source_account_value} left"
      end

      path = "#{WALLET_API}/byron-wallets/#{FAUCET_WALLET_ID}/transactions"
      body = %({"payments":[{"address":"#{address}","amount":{"quantity":#{@amount},"unit":"lovelace"}}],"passphrase":"#{SECRET_PASSPHRASE}"})
      LOG.debug("Performing send; curl equivalent:")
      LOG.debug("curl -vX POST #{path} -H 'Content-Type: application/json; charset=utf-8' -d '#{body}' --http1.1")
      response = Wallet.apiPost(path, body)
      id = "ERROR"
      if response[0].success?
        result = response[1]
        id = JSON.parse(result.not_nil!)["id"].as_s
      end

      LOG.info("The id for this funds transfer transaction is: #{id}")
      msg = {
        success: response[0].success?,
        amount:  @amount,
        fee:     tx_fees,
        txid:    id,
      }
      LOG.info(msg.to_json)
      msg
    end
  end
end

DB.open "sqlite3://last-seen.sqlite" do |db|
  faucet = Cardano::Faucet.new(LOVELACES_TO_GIVE, db)
  middleware = [HTTP::ErrorHandler.new, HTTP::LogHandler.new]
  server = HTTP::Server.new(middleware) do |context|
    context.response.content_type = "application/json"
    status_and_body = faucet.on_request(context)
    context.response.status = status_and_body[:status]
    if context.request.method == "GET" \
       && context.request.path.match(%r(/metrics/?$)) \
       && context.response.status == HTTP::Status::OK
      context.response.print(status_and_body[:body])
    else
      context.response.print(status_and_body[:body].to_json)
    end
  end

  address = server.bind_tcp(FAUCET_LISTEN_PORT)

  LOG.info("Listening on http://#{address}")

  LOG.debug("FAUCET_LOG_LEVEL: #{FAUCET_LOG_LEVEL}")
  LOG.debug("FAUCET_LISTEN_PORT: #{FAUCET_LISTEN_PORT}")
  LOG.debug("FAUCET_WALLET_ID_PATH: #{FAUCET_WALLET_ID_PATH}")
  LOG.debug("FAUCET_WALLET_ID: #{FAUCET_WALLET_ID}")
  LOG.debug("FAUCET_PASSPHRASE_PATH: #{FAUCET_PASSPHRASE_PATH}")
  LOG.debug("FAUCET_API_KEY_PATH: #{FAUCET_API_KEY_PATH}")
  LOG.debug("WALLET_LISTEN_PORT: #{WALLET_LISTEN_PORT}")
  LOG.debug("WALLET_API: #{WALLET_API}")
  LOG.debug("LOVELACES_TO_GIVE: #{LOVELACES_TO_GIVE}")
  LOG.debug("SECONDS_BETWEEN_REQUESTS: #{SECONDS_BETWEEN_REQUESTS}")

  server.listen
end
