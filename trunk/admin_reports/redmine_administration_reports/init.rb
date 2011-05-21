require 'redmine'

Redmine::Plugin.register :redmine_administration_reports do
  name 'Redmine administration Reports plugin'
  author 'M. Yoshida'
  description 'This is a plugin for reports of administration'
  version '0.0.1'
  url 'http://example.com/path/to/plugin'


  menu :application_menu, :admin_reports, { :controller => 'admin_reports', :action => 'info' },
                                               :if => Proc.new { User.current.logged? }

end


Redmine::MenuManager.map :admin_reports_menu do |menu|
  menu.push :permissions, {:controller => 'admin_reports', :action => 'permissions'},
                                             :caption => :label_permissions_report,
                                             :html => {:class => 'roles'}

  menu.push :workflows, {:controller => 'admin_reports', :action => 'workflows'},
                                             :caption => :label_workflow
  menu.push :plugins, {:controller => 'admin_reports', :action => 'plugins'}
  menu.push :info, {:controller => 'admin_reports', :action => 'info'},
                                             :caption => :label_information_plural, :last => true
end
