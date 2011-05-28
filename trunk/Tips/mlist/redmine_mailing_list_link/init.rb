require 'redmine'

Redmine::Plugin.register :redmine_mailing_list_link do
  name 'Redmine Mailing List Link plugin'
  author 'M. Yoshida'
  description 'This is a plugin for linking to Mailing list'
  url 'http://www.r-labs.org/projects/r-labs/wiki/%E3%83%97%E3%83%A9%E3%82%B0%E3%82%A4%E3%83%B3_Tips'

  settings :default => {
    'mlist_view_string' => 'ruby-list#',
    'mlist_address' => 'http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/'
  }, :partial => 'settings/mlist_settings'

end


Redmine::WikiFormatting::Macros.register do
  desc "Mailing list link macro"
  macro :mlist do |obj, args|
    if (args.size != 1)
      raise "this macro is given only one argument."
    else
      word = args.first.strip
      addr = Setting.plugin_redmine_mailing_list_link['mlist_address'] + CGI.escape(word)
      viewstr = Setting.plugin_redmine_mailing_list_link['mlist_view_string'] + word
      return link_to(viewstr, addr, :class => "external")
    end
  end
end

