# Cardano Faucet
# - Cardano module
#

module Cardano
  class ApiException < Exception
    getter response

    @response : HTTP::Client::Response

    def initialize(@response)
    end
  end

  class Wallet
    def self.recaptchaVerify(recaptchaResponse, ip)
      params = HTTP::Params.encode({"secret" => RECAPTCHA_SECRET,
                                    "response" => recaptchaResponse,
                                    "remoteip" => ip })
      response = HTTP::Client.post(RECAPTCHA_URI + "?" + params, RECAPTCHA_HEADER)
      result = response.body
      statusCode = response.status_code
      statusMessage = response.status_message
      Log.debug { "reCaptcha response: #{response}" }
      Log.debug { "reCaptcha submitted ip: #{ip}" }
      if response.success?
        Log.debug { "statusCode: #{statusCode}" }
        Log.debug { "statusMessage: #{statusMessage}" }
        Log.debug { "Result: #{result.to_s.delete('\n')}" }
      else
        Log.error { "statusCode: #{statusCode}" }
        Log.error { "statusMessage: #{statusMessage}" }
        Log.error { "Result: #{result.to_s.delete('\n')}" }
        apiRaise response
      end
      return response
    end

    def self.apiPost(path, body)
      client = HTTP::Client.new(API_URI)
      response = client.post(path, HEADERS, body)
      result = response.body
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
        apiRaise response
      end
      return response
    end

    def self.apiGet(path)
      client = HTTP::Client.new(API_URI)
      response = client.get(path)
      result = response.body
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
        apiRaise response
      end
      return response
    end

    def self.apiRaise(response)
      raise ApiException.new(response)
    end

    def self.restartWallet
      IO_CMD_OUT.clear
      IO_CMD_ERR.clear
      Log.debug { "Found a cardano-wallet code event requiring cardano-wallet systemd service restart -- restarting..." }
      cmd = "/run/wrappers/bin/sudo /run/current-system/sw/bin/systemctl restart cardano-wallet.service"
      result = Process.run(cmd, output: IO_CMD_OUT, error: IO_CMD_ERR, shell: true)
      Log.debug { "Cardano-wallet restart result:\nRestart success: #{result.success?}\nSTDOUT: #{IO_CMD_OUT}\nSTDERR: #{IO_CMD_ERR}" }
    end
  end

  class Account
    def self.for_wallet(walletId)
      # value = JSON.parse(`cardano-wallet wallet get #{walletId}`)["balance"]["available"]["quantity"].as_i64

      path = USE_BYRON_WALLET ? "#{WALLET_API}/byron-wallets/#{walletId}" : "#{WALLET_API}/wallets/#{walletId}"
      Log.debug { "Fetching available wallet value; curl equivalent:" }
      Log.debug { "curl -v #{path}" }
      response = Wallet.apiGet(path)
      value = JSON.parse(response.body)["balance"]["available"]["quantity"].as_i64
      return value.not_nil!
    end
  end

  class Txs
    def self.for_wallet(walletId)
      # counter = JSON.parse(`cardano-wallet transaction list #{walletId}`)

      path = USE_BYRON_WALLET ? "#{WALLET_API}/byron-wallets/#{walletId}/transactions" : "#{WALLET_API}/wallets/#{walletId}/transactions"
      Log.debug { "Fetching wallet transaction count; curl equivalent:" }
      Log.debug { "curl -v #{path} | jq '. | length'" }
      response = Wallet.apiGet(path)
      counter = JSON.parse(response.body)
      return counter.not_nil!.size
    end
  end

  class Fees
    def self.for_tx(walletId, dest_addr, amount)
      # fees = JSON.parse(`cardano-wallet transaction fees #{walletId} --payment #{amount}@#{dest_addr}`)["amount"]["quantity"].as_i

      path = USE_BYRON_WALLET ? "#{WALLET_API}/byron-wallets/#{walletId}/payment-fees" : "#{WALLET_API}/wallets/#{walletId}/payment-fees"
      body = %({"payments":[{"address":"#{dest_addr}","amount":{"quantity":#{amount},"unit":"lovelace"}}]})
      Log.debug { "Fetching transaction fee estimate; curl equivalent:" }
      Log.debug { "curl -vX POST #{path} -H 'Content-Type: application/json; charset=utf-8' -d '#{body}' --http1.1" }
      response = Wallet.apiPost(path, body)
      fees = JSON.parse(response.body)["estimated_min"]["quantity"].as_i64
      return fees.not_nil!
    end
  end

  class Settings
    JSON.mapping(
      genesis_block_hash: String
    )

    def self.get
      # from_json(`cardano-wallet network parameters`)

      path = "#{WALLET_API}/network/parameters"
      Log.debug { "Fetching network parameters; curl equivalent:" }
      Log.debug { "curl -v #{path}" }
      response = Wallet.apiGet(path)
      from_json(response.body)
    end
  end

  class Faucet
    getter settings

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
      @lastRequestTime = Time.utc
      migrations
    end

    # Recursively increment version until no migrations are left
    # Note that we cannot `DROP COLUMN`
    # see https://www.sqlite.org/faq.html#q11
    def migrations
      version = @db.scalar("PRAGMA main.user_version").as(Int64)

      case version
      when 0
        Log.info { "Performing db migration 0: request table creation" }
        migrate <<-SQL
          CREATE TABLE IF NOT EXISTS requests (
            host VARCHAR UNIQUE PRIMARY KEY NOT NULL,
            seen TIME NOT NULL DEFAULT CURRENT_TIMESTAMP
          )
        SQL
      when 1
        Log.info { "Performing db migration 1: adding a genesis hash column" }
        migrate <<-SQL
          ALTER TABLE requests ADD COLUMN hash VARCHAR NOT NULL DEFAULT ''
        SQL
      when 2
        # Add support for per API key rate limiting
        Log.info { "Performing db migration 2: adding API key support" }
        txCmds = Array(String).new
        txCmds << "ALTER TABLE requests RENAME TO old_requests"
        txCmds << <<-SQL
          CREATE TABLE requests (
            host VARCHAR NOT NULL,
            apikey VARCHAR NOT NULL DEFAULT '',
            seen TIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
            hash VARCHAR NOT NULL DEFAULT '',
            CONSTRAINT requests_pk PRIMARY KEY (host, apikey)
          )
        SQL
        txCmds << "INSERT INTO requests SELECT host, '', seen, hash FROM old_requests"
        txCmds << "DROP TABLE old_requests"
        migrate_tx txCmds
      when 3
        Log.info { "Performing db migration 3: adding an apikeycomment column" }
        migrate <<-SQL
          ALTER TABLE requests ADD COLUMN apikeycomment VARCHAR NOT NULL DEFAULT ''
        SQL
      when 4
        Log.info { "Performing db migration 4: adding an address column" }
        migrate <<-SQL
          ALTER TABLE requests ADD COLUMN address VARCHAR NOT NULL DEFAULT ''
        SQL
      when 5
        Log.info { "Performing db migration 5: adding a txid column" }
        migrate <<-SQL
          ALTER TABLE requests ADD COLUMN txid VARCHAR NOT NULL DEFAULT ''
        SQL
      when 6
        Log.info { "Performing db migration 6: adding an amount column" }
        migrate <<-SQL
          ALTER TABLE requests ADD COLUMN amount VARCHAR NOT NULL DEFAULT ''
        SQL
      else
        return
      end

      @db.exec "PRAGMA main.user_version = #{version + 1}"
      migrations
    end

    def migrate(statement : String)
      Log.info { "DB statement execution summary: #{@db.exec statement}" }
    end

    def migrate_tx(txCmds : Array(String))
      begin
        tx = @db.transaction do |tx|
          txCmds.each do |cmd|
            @db.exec cmd
          end
        end
        Log.info { "DB transaction success: #{tx}" }
      rescue ex
        Log.error { "ERROR: DB transaction exception due to: #{ex}" }
      end
    end

    def on_request(context : HTTP::Server::Context) : Response
      @lastRequestTime = Time.utc
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

    def on_error(error : Exception)
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

    def on_error(error : ApiException)
      body = error.response.body
      statusCode = error.response.status_code

      begin
        if blob = JSON.parse(body)
          code = blob["code"]? || "NA"
          message = blob["message"]? || body
          Log.debug { "ApiException code: #{code}, message: #{message}" }

          # Check for errors requiring wallet restart
          if code.to_s =~ /^wallet_not_responding$/
            Wallet.restartWallet
          end

          # Check for errors requiring status code intercept
          if message.to_s =~ /WrongNetwork/
            statusCode = 400
          end
        else
          message = body
        end
      rescue
        message = body
      end

      msg = {statusCode: statusCode,
             error:      HTTP::Status.from_value?(statusCode).to_s,
             message:    message.to_s,
      }

      Log.debug { msg.to_json }
      {
        status: HTTP::Status.from_value?(statusCode).as(HTTP::Status),
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

    def on_recaptcha_required
      if FAUCET_FRONTEND_URL == ""
        message = "Anonymous Access Requires Recaptcha: please request funds via the frontend and complete the recaptcha"
      else
        message = "Anonymous Access Requires Recaptcha: please request funds via the frontend and complete the recaptcha; #{FAUCET_FRONTEND_URL}"
      end
      msg = {statusCode: 403,
             error:      "Forbidden",
             message:    message,
      }

      Log.debug { msg.to_json }
      {
        status: HTTP::Status::FORBIDDEN,
        body:   msg,
      }
    end

    def on_recaptcha_failed
      msg = {statusCode: 403,
             error:      "Forbidden",
             message:    "Recaptcha verification failed",
      }

      Log.debug { msg.to_json }
      {
        status: HTTP::Status::FORBIDDEN,
        body:   msg,
      }
    end

    def on_post(context : HTTP::Server::Context) : Response
      amount = LOVELACES_TO_GIVE_ANON
      apiKey = ""
      apiKeyComment = ""
      authenticated = false

      match = context.request.path.match(%r(/send-money/([^/]+)))
      return on_not_found unless match

      address = match[1]
      if context.request.query_params.has_key?("apiKey")
        apiKey = context.request.query_params["apiKey"]
        if API_KEYS.has_key?(apiKey)
          amount = API_KEYS[apiKey][:lovelacesPerTx].as(UInt64)
          authenticated = true
          apiKeyComment = API_KEYS[apiKey][:comment].as(String)
          timeBetweenRequests = API_KEYS[apiKey][:periodPerTx].as(UInt32).seconds
        else
          timeBetweenRequests = SECS_BETWEEN_REQS_ANON.seconds
          apiKey = ""
        end
      else
        timeBetweenRequests = SECS_BETWEEN_REQS_ANON.seconds
      end

      ipPort = context.request.remote_address
      xRealIp = context.request.headers["X-Real-IP"]?
      ip = Socket::IPAddress.parse("tcp://#{real_ip_port(xRealIp) || ipPort}").address

      if authenticated
        Log.info { "Auth Request:  #{apiKey}  \"#{API_KEYS[apiKey][:comment]}\"  " \
                   "LOVELACES_PER_TX: #{amount}  " \
                   "PERIOD_PER_TX: #{API_KEYS[apiKey][:periodPerTx]}  " \
                   "IP-PORT: #{ipPort || "NA"}  " \
                   "X-Real-IP: #{xRealIp || "NA"}" }
      else
        Log.info { "Anon Request:  LOVELACES_PER_TX: #{amount}  " \
                   "PERIOD_PER_TX: #{SECS_BETWEEN_REQS_ANON}  " \
                   "IP-PORT: #{ipPort || "NA"}  " \
                   "X-Real-IP: #{xRealIp || "NA"}" }
      end

      if !ANONYMOUS_ACCESS && !authenticated
        return on_forbidden
      end

      if USE_RECAPTCHA_ON_ANON && !authenticated
        if context.request.query_params.has_key?("g-recaptcha-response")
          gRecaptchaResponse = context.request.query_params["g-recaptcha-response"]
          response = Wallet.recaptchaVerify(gRecaptchaResponse, ip)
          if JSON.parse(response.body)["success"]? && JSON.parse(response.body)["success"].to_s == "true"
            Log.info { "Recaptcha Verified: true" }
          else
            Log.info { "Recaptcha Verified: false" }
            return on_recaptcha_failed
          end
        else
          Log.info { "Recaptcha Verified: not provided" }
          return on_recaptcha_required
        end
      end

      rate_limiter, ip = limit_rate(
        ip,
        timeBetweenRequests.as(Time::Span),
        apiKey,
        apiKeyComment,
        address,
        amount,
        "rateLimitOnRequest"
        )

      if rate_limiter[:allow]
        on_send_money(address,
                      amount,
                      ip,
                      apiKey,
                      apiKeyComment)
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

    def on_send_money(to_address : String,
                      amount : UInt64,
                      ip : String,
                      apiKey : String,
                      apiKeyComment : String,
                     ) : Response

      result = send_funds(to_address, amount, ip, apiKey, apiKeyComment)
      {
        status: HTTP::Status::OK,
        body:   result,
      }
    end

    def on_too_many_requests(try_again : Time) : Response
      delta = (try_again - @lastRequestTime).total_seconds.to_i
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

    def real_ip_port(header : String) : String
      "#{header}:443"
    end

    def real_ip_port(header : Nil) : Nil
    end

    def limit_rate(ip : Nil,
                   timeBetweenRequests : Time::Span,
                   apiKey : String,
                   apiKeyComment : String,
                   address : String,
                   amount : UInt64,
                   txId : String
                  ) : Tuple(NamedTuple(time: Time, allow: Bool, try_again: Time), String)

      return({time:      @lastRequestTime,
              allow:     false,
              try_again: @lastRequestTime + timeBetweenRequests,
             },
             "")
    end

    def limit_rate(ip : String,
                   timeBetweenRequests : Time::Span,
                   apiKey : String,
                   apiKeyComment : String,
                   address : String,
                   amount : UInt64,
                   txId : String
                  ) : Tuple(NamedTuple(time: Time, allow: Bool, try_again: Time), String)

      allow_after = @lastRequestTime - timeBetweenRequests

      found = nil

      select_seen(ip, address, apiKey, allow_after) do |rs|
        rs.each do
          seen = rs.read(Time)
          found = {
            time:      seen,
            allow:     false,
            try_again: seen + timeBetweenRequests,
          }
        end
      end

      if found
        return(found.not_nil!, ip)
      end

      unless RATE_LIMIT_ON_SUCCESS
        @db.exec(<<-SQL, ip, apiKey, @lastRequestTime, @settings.genesis_block_hash, apiKeyComment, address, txId, amount.to_s)
          INSERT OR REPLACE INTO requests VALUES (?, ?, ?, ?, ?, ?, ?, ?)
        SQL
      end

      return({time:      @lastRequestTime,
              allow:     true,
              try_again: allow_after,
             },
             ip)
    end

    def select_seen(ip, address, apiKey, allow_after, &block : DB::ResultSet -> Nil)
      @db.query(<<-SQL, ip, address, apiKey, allow_after, @settings.genesis_block_hash, &block)
        SELECT seen FROM requests
          WHERE (host = ? OR address = ?)
          AND apikey = ?
          AND seen > ?
          AND hash = ?
          ORDER BY seen DESC
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

    def send_funds(address : String,
                   amount : UInt64,
                   ip : String,
                   apiKey : String,
                   apiKeyComment : String,
                  ) : SendFundsResult

      tx_fees = Fees.for_tx(FAUCET_WALLET_ID, address, amount)
      amount_with_fees = amount + tx_fees

      source_account_value = Account.for_wallet(FAUCET_WALLET_ID)

      # If we want to add a faucet Tx count metric
      # source_tx_counter = Txs.for_wallet(FAUCET_WALLET_ID)

      if source_account_value < amount_with_fees
        Log.error { "Not enough funds in faucet account, only #{source_account_value} left" }
        raise "Not enough funds in faucet account, only #{source_account_value} left"
      else
        Log.info { "Faucet funds: { \"pre-tx\": \"#{source_account_value}\", " \
                   "\"post-tx\": \"#{source_account_value - amount_with_fees}\" }" }
      end

      path = USE_BYRON_WALLET ? "#{WALLET_API}/byron-wallets/#{FAUCET_WALLET_ID}/transactions" : "#{WALLET_API}/wallets/#{FAUCET_WALLET_ID}/transactions"
      body = %({"payments":[{"address":"#{address}","amount":{"quantity":#{amount},"unit":"lovelace"}}],"passphrase":"#{SECRET_PASSPHRASE}"})
      Log.debug { "Performing send; curl equivalent:" }
      Log.debug { "curl -vX POST #{path} -H 'Content-Type: application/json; charset=utf-8' -d '#{body}' --http1.1" }
      response = Wallet.apiPost(path, body)
      id = JSON.parse(response.body)["id"].as_s

      msg = {
        success: response.success?,
        amount:  amount,
        fee:     tx_fees,
        txid:    id,
      }
      Log.info { msg.to_json }

      if RATE_LIMIT_ON_SUCCESS
        @db.exec(<<-SQL, ip, apiKey, @lastRequestTime, @settings.genesis_block_hash, apiKeyComment, address, id, amount.to_s)
          INSERT OR REPLACE INTO requests VALUES (?, ?, ?, ?, ?, ?, ?, ?)
        SQL
      end

      msg
    end
  end
end
