require 'redmine'

Redmine::Plugin.register :redmine_original_image do
  name 'Redmine original image plugin'
  author 'M. Yoshida'
  description 'Sample use plugin original image'
  url 'http://www.r-labs.org/projects/r-labs/wiki/%E3%83%97%E3%83%A9%E3%82%B0%E3%82%A4%E3%83%B3_Tips'
  version '0.0.1'

  menu :application_menu, :original_image, {:controller => 'foo', :action => 'show' }

end
