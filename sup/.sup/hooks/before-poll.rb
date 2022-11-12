def unless_offline
  system "ping -c2 www.yahoo.com &> /dev/null"
  yield if $? == 0
end

unless_offline do
  if (@last_fetch_time || Time.at(0)) < Time.now - 60
    say "Polling for new mail..."
    system "mbsync -a 2>&1 > /dev/null"
    say "Done polling."
    @last_fetch_time = Time.now
  end
end

