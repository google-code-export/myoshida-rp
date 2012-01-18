require 'redmine'

Redmine::Plugin.register :redmine_standard do
  name 'Redmine Standard plugin'
  author 'M. Yoshida'
  description 'This is a sample plugin for Redmine'
  version '0.0.2'
  url 'http://www.r-labs.org/projects/r-labs/wiki'

  project_module :standard do
    permission :view_foos, :foos => [:index, :show, :preview]
    permission :manage_foos, {:foos => [:new, :edit, :destroy]},
               :require => :member
  end

  menu :project_menu, :standard, { :controller => 'foos', :action => 'index'}, :param => :project_id

end
