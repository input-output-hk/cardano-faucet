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
        Log.error { msg }
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
