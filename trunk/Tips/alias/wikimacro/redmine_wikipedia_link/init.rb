require 'redmine'
require 'cgi'

Redmine::Plugin.register :redmine_wikipedia_link do
  name 'Redmine Wikipedia Link plugin'
  author 'M. Yoshida'
  description 'This is a plugin for linking to Wikipedia'
  url 'http://www.r-labs.org/projects/r-labs/wiki/%E3%83%97%E3%83%A9%E3%82%B0%E3%82%A4%E3%83%B3_Tips'
  version '0.0.1'
end

Redmine::WikiFormatting::Macros.register do
  desc "Wikipedia link macro"
  macro :wikipedia do |obj, args|
    if (args.size != 1)
      raise "Wikipedia macro is given only one argument."
    else
      word = args.first.strip
      addr = "http://ja.wikipedia.org/wiki/" + CGI.escape(word)
      return link_to(word, addr, :class => "external")
    end
  end
end
