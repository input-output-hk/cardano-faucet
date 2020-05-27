# Cardano Faucet
# - Cardano module
#

module Cardano
  class Wallet
    def self.apiPost(path, body)
      client = HTTP::Client.new(API_URI)
      client.post(path, HEADERS, body) do |response|
        result = response.body_io.gets
        statusCode = response.status_code
        statusMessage = response.status_message
        Log.debug { "response: #{response}" }
        if response.success?
          Log.debug { "statusCode: #{statusCode}" }
          Log.debug { "statusMessage: #{statusMessage}" }
          Log.debug { "Result: #{result}" }
        else
          Log.error { "statusCode: #{statusCode}" }
          Log.error { "statusMessage: #{statusMessage}" }
          Log.error { "Result: #{result}" }
        end
        return {response, result}
      end
    end

    def self.apiGet(path)
      client = HTTP::Client.new(API_URI)
      client.get(path) do |response|
        result = response.body_io.gets
        statusCode = response.status_code
        statusMessage = response.status_message
        Log.debug { "response: #{response}" }
        if response.success?
          Log.debug { "statusCode: #{statusCode}" }
          Log.debug { "statusMessage: #{statusMessage}" }
          Log.debug { "Result: #{result}" }
        else
          Log.error { "statusCode: #{statusCode}" }
          Log.error { "statusMessage: #{statusMessage}" }
          Log.error { "Result: #{result}" }
        end
        return {response, result}
      end
    end
  end

  class Account
    def self.for_wallet(walletId)
      # value = JSON.parse(`cardano-wallet-{byron|shelley} wallet get #{walletId}`)["balance"]["available"]["quantity"].as_i64

      path = USE_BYRON_WALLET ? "#{WALLET_API}/byron-wallets/#{walletId}"
                              : "#{WALLET_API}/wallets/#{walletId}"
      Log.debug { "Fetching available wallet value; curl equivalent:" }
      Log.debug { "curl -v #{path}" }
      response = Wallet.apiGet(path)
      value = 0
      if response[0].success?
        value = JSON.parse(response[1].not_nil!)["balance"]["available"]["quantity"].as_i64
      end
      return value.not_nil!
    end
  end

  class Txs
    def self.for_wallet(walletId)
      # counter = JSON.parse(`cardano-wallet-{byron|shelley} transaction list #{walletId}`)

      path = USE_BYRON_WALLET ? "#{WALLET_API}/byron-wallets/#{walletId}/transactions"
                              : "#{WALLET_API}/wallets/#{walletId}/transactions"
      Log.debug { "Fetching wallet transaction count; curl equivalent:" }
      Log.debug { "curl -v #{path} | jq '. | length'" }
      response = Wallet.apiGet(path)
      if response[0].success?
        counter = JSON.parse(response[1].not_nil!)
      end
      return counter.not_nil!.size
    end
  end

  class Fees
    def self.for_tx(walletId, dest_addr, amount)
      # fees = JSON.parse(`cardano-wallet-{byron|shelley} transaction fees #{walletId} --payment #{amount}@#{dest_addr}`)["amount"]["quantity"].as_i

      path = USE_BYRON_WALLET ? "#{WALLET_API}/byron-wallets/#{walletId}/payment-fees"
                              : "#{WALLET_API}/wallets/#{walletId}/payment-fees"
      body = %({"payments":[{"address":"#{dest_addr}","amount":{"quantity":#{amount},"unit":"lovelace"}}]})
      Log.debug { "Fetching transaction fee estimate; curl equivalent:" }
      Log.debug { "curl -vX POST #{path} -H 'Content-Type: application/json; charset=utf-8' -d '#{body}' --http1.1" }
      response = Wallet.apiPost(path, body)
      fees = 0_i64
      if response[0].success?
        fees = JSON.parse(response[1].not_nil!)["estimated_min"]["quantity"].as_i64
      end
      return fees.not_nil!
    end
  end

  class Settings
    JSON.mapping(
      genesis_block_hash: String
    )

    def self.get
      # from_json(`cardano-wallet-{byron|shelley} network parameters latest`)

      path = "#{WALLET_API}/network/parameters/latest"
      Log.debug { "Fetching network parameters; curl equivalent:" }
      Log.debug { "curl -v #{path}" }
      response = Wallet.apiGet(path)
      from_json(response[1].not_nil!)
    end
  end

  class Faucet
    TIME_BETWEEN_REQUESTS = SECONDS_BETWEEN_REQUESTS.seconds

    alias Allow = Bool
    alias Response = {status: HTTP::Status, body: SendFundsResult | NotFoundResult | RateLimitResult | String}
    alias SendFundsResult = {success: Bool, amount: UInt64, fee: Int64, txid: String}
    alias RateLimitResult = {statusCode: Int32, error: String, message: String, retryAfter: Time}
    alias NotFoundResult = {statusCode: Int32, error: String, message: String}

    @settings : Settings
    @db : DB::Database
    @lastMetricsTime : Time
    @lastMetrics : String

    def initialize(@db)
      @settings = Settings.get
      @db.scalar "PRAGMA journal_mode=WAL"
      @lastMetricsTime = Time.utc
      @lastMetrics = ""
      migrations
    end

    # Recursively increment version until no migrations are left
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
      msg = {statusCode: 500,
             error:      HTTP::Status::INTERNAL_SERVER_ERROR.to_s,
             message:    error.to_s,
      }
      Log.debug { msg.to_json }
      {
        status: HTTP::Status::INTERNAL_SERVER_ERROR,
        body:   msg,
      }
    end

    def on_not_found
      msg = {statusCode: 404,
             error:      "Not Found",
             message:    "No URL found",
      }
      Log.debug { msg.to_json }
      {
        status: HTTP::Status::NOT_FOUND,
        body:   msg,
      }
    end

    def on_forbidden
      msg = {statusCode: 403,
             error:      "Forbidden",
             message:    "Anonymous Access Not Allowed: please authenticate by apiKey",
      }
      Log.debug { msg.to_json }
      {
        status: HTTP::Status::FORBIDDEN,
        body:   msg,
      }
    end

    def on_post(context : HTTP::Server::Context) : Response
      rateExempted = false

      match = context.request.path.match(%r(/send-money/([^/]+)))
      return on_not_found unless match

      if context.request.query_params.has_key?("apiKey")
        if API_KEYS.has_key? context.request.query_params["apiKey"]
          rateExempted = true
          Log.debug { "Rate exempted by API key with comment: " \
                      "\"#{API_KEYS[context.request.query_params["apiKey"]]}\"" }
        end
      end

      if !ANONYMOUS_ACCESS && !rateExempted
        return on_forbidden
      end

      rate_limiter = limit_rate(
        real_ip(context.request.headers["X-Real-IP"]?) ||
        context.request.remote_address)

      if rate_limiter[:allow] || rateExempted
        if rateExempted
          on_send_money(match[1], LOVELACES_TO_GIVE_APIKEY)
        else
          on_send_money(match[1], LOVELACES_TO_GIVE_ANON)
        end
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
        Log.debug { "Metrics were fetched #{@lastMetricsTime} with a refresh period \
                   of #{MIN_METRICS_PERIOD}s; serving previous result..." }
      end
      {
        status: HTTP::Status::OK,
        body:   @lastMetrics,
      }
    end

    def on_send_money(to_address : String, amount : UInt64) : Response
      result = send_funds(to_address, amount)
      {
        status: HTTP::Status::OK,
        body:   result,
      }
    end

    def on_too_many_requests(try_again : Time) : Response
      delta = (try_again - Time.utc).total_seconds.to_i
      msg = {statusCode: 429,
             error:      "Too Many Requests",
             message:    "Try again in #{delta} seconds",
             retryAfter: try_again.to_utc,
      }
      Log.debug { msg.to_json }
      {
        status: HTTP::Status::TOO_MANY_REQUESTS,
        body:   msg,
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

    def send_funds(address : String, amount : UInt64) : SendFundsResult
      tx_fees = Fees.for_tx(FAUCET_WALLET_ID, address, amount)
      amount_with_fees = amount + tx_fees

      source_account_value = Account.for_wallet(FAUCET_WALLET_ID)

      # If we want to add a faucet Tx count metric
      # source_tx_counter = Txs.for_wallet(FAUCET_WALLET_ID)

      Log.info { "The transaction will be posted to the blockchain with genesis hash: #{@settings.genesis_block_hash}" }

      if source_account_value < amount_with_fees
        Log.error { "Not enough funds in faucet account, only #{source_account_value} left" }
        raise "Not enough funds in faucet account, only #{source_account_value} left"
      end

      path = USE_BYRON_WALLET ? "#{WALLET_API}/byron-wallets/#{FAUCET_WALLET_ID}/transactions"
                              : "#{WALLET_API}/wallets/#{FAUCET_WALLET_ID}/transactions"
      body = %({"payments":[{"address":"#{address}","amount":{"quantity":#{amount},"unit":"lovelace"}}],"passphrase":"#{SECRET_PASSPHRASE}"})
      Log.debug { "Performing send; curl equivalent:" }
      Log.debug { "curl -vX POST #{path} -H 'Content-Type: application/json; charset=utf-8' -d '#{body}' --http1.1" }
      response = Wallet.apiPost(path, body)
      id = "ERROR"
      if response[0].success?
        result = response[1]
        id = JSON.parse(result.not_nil!)["id"].as_s
      end

      Log.info { "The id for this funds transfer transaction is: #{id}" }
      msg = {
        success: response[0].success?,
        amount:  amount,
        fee:     tx_fees,
        txid:    id,
      }
      Log.info { msg.to_json }
      msg
    end
  end
end
