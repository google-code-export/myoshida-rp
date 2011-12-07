require 'redmine'

# Plugin 
Redmine::Plugin.register :redmine_sample_macros do
  name 'Redmine Sample Macros plugin'
  author 'yohshiy'
  description 'This is a sample plugin for wiki macro'
  version '0.0.1'
  url 'http://yohshiy.blog.fc2.com/blog-entry-77.html'
  author_url 'http://yohshiy.blog.fc2.com/'
end


# Wikipedia
Redmine::WikiFormatting::Macros.register do
  macro :wikipedia do |obj, args|
    word = args.first
    link_to(word, "http://ja.wikipedia.org/wiki/" + CGI.escape(word))
  end
end

# Google Search
Redmine::WikiFormatting::Macros.register do
  macro :google do |obj, args|
    word = args.first
    link_to(word + "?", "http://www.google.co.jp/search?ie=UTF-8&amp;q=" + CGI.escape(word))
  end
end

# Plugin Link (redmine.org)
Redmine::WikiFormatting::Macros.register do
  macro :plugin do |obj, args|
    word = args.first
    link_to("Plugin:"+word, "http://www.redmine.org/plugins/" + word)
  end
end


# Hatena Bookmark
Redmine::WikiFormatting::Macros.register do
  macro :b_hatena do |obj, args|
    word = args.first
    link_to("Bookmark:"+word, "http://b.hatena.ne.jp/" + word)
  end
end


# Amazon
Redmine::WikiFormatting::Macros.register do
  macro :bsdn do |obj, args|
    word = args.first
    link_to("BSDN-"+word, "http://www.amazon.co.jp/exec/obidos/ASIN/" + word)
  end
end


# Rubi
Redmine::WikiFormatting::Macros.register do
  macro :rubi do |obj, args|
    content_tag(:ruby) {
      out = content_tag(:rb, args[0])
      out += content_tag(:rp, '(')
      out += content_tag(:rt, args[1])
      out += content_tag(:rp, ')')
    }
  end
end

