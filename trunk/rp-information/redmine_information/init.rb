require 'redmine'

Redmine::Plugin.register :redmine_information do
  name 'Redmine Information Plugin'
  author 'M. Yoshida'
  description 'This is a plugin for information of Redmine'
  version '0.1.0'
  url 'http://www.r-labs.org/projects/rp-admin-reports/wiki/RedmineInformationEn'

  settings(:default => InfoCategory.hide_map(),
           :partial => 'settings/reports_settings')
  menu(:top_menu, :redmine_info,
       { :controller => 'info', :action => 'show', :id => :version },
       :if => Proc.new { User.current.logged? })

end


Redmine::MenuManager.map :redmine_info_menu do |menu|
  ReportItem.push_menu(menu, :permissions, :label_permissions_report, 
                       :html => {:class => 'roles'})
  ReportItem.push_menu(menu, :workflows, :label_workflow)
                                    
  ReportItem.push_menu(menu, :settings)
  ReportItem.push_menu(menu, :plugins)
  ReportItem.push_menu(menu, :version, :label_information_plural, :last => true)
end
