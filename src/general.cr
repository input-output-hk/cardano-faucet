# Cardano Faucet
# - General functions
#

def readFile(file)
  if (File.exists?(file) && !File.empty?(file))
    return File.read(file).strip
  else
    return ""
  end
end

def readKeys(file)
  apiKeys = Hash(String, Hash(Symbol, String | UInt32 | UInt64)).new
  f = readFile(file)
  if f != ""
    f.split("\n").each.with_index do |i, c|
      msgPrefix = "API Key file \"#{file}\", line \"#{c + 1}\""

      # Remove comment lines and whitespace lines
      if i.lstrip =~ /^#/ || i =~ /^\s*$/
        next
      end

      # Ensure all required fields are included in each key record processed
      raise "#{msgPrefix} does not contain the 3 required fields" if i.split.size < 3

      keyFields = i.split

      # Ensure the key field API key is alphanumeric of API_KEY_LEN
      if keyFields[0] =~ /^[A-Za-z0-9]{#{API_KEY_LEN}}$/
        apiKey = keyFields[0]
      else
        raise "#{msgPrefix}, key \"#{keyFields[0]}\" is not a #{API_KEY_LEN} char alphanumeric"
      end

      # Ensure all declared API keys are unique
      raise "#{msgPrefix} contains an API key that has already been declared" if apiKeys.has_key?(apiKey)

      # Ensure the key field LOVELACES_PER_TX is > 0 or "default" and parse
      if (keyFields[1].to_u64? && keyFields[1].to_u64 > 0) || keyFields[1].to_s == "default"
        lovelacesPerTx = keyFields[1].to_u64? ? keyFields[1].to_u64 : LOVELACES_TO_GIVE_APIKEY
      else
        raise "#{msgPrefix}, LOVELACES_PER_TX field is not > 0 or \"default\" (without quotes)"
      end

      # Ensure the key field PERIOD_PER_TX is >= 0 or "default" and parse
      if keyFields[2].to_u32? || keyFields[2].to_s == "default"
        periodPerTx = keyFields[2].to_u32? ? keyFields[2].to_u32 : SECS_BETWEEN_REQS_APIKEY
      else
        raise "#{msgPrefix}, PERIOD_PER_TX field is not >= 0 or \"default\" (without quotes)"
      end

      # Ensure the comment field, if provided is set properly
      if keyFields.size == 3
        comment = "Uncommented"
      else
        comment = keyFields[3..-1].join(" ")[0..API_KEY_COMMENT_MAX_LEN]
      end

      apiKeys[apiKey] = {:lovelacesPerTx => lovelacesPerTx,
                         :periodPerTx    => periodPerTx,
                         :comment        => comment}
    end
  end
  apiKeys
end
