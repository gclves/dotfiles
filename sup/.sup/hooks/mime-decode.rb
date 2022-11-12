require 'shellwords'

unless sibling_types.member? "text/plain"
  safe_name = Shellwords.escape filename
  case content_type
  when "text/html"
    `/usr/bin/w3m -dump -T #{content_type} #{safe_name}`
  end
end

