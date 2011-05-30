require 'redmine'

Redmine::Plugin.register :redmine_administration_reports do
  name 'Redmine administration Reports plugin'
  author 'M. Yoshida'
  description 'This is a plugin for reports of administration'
  version '0.1.0'
  url 'http://www.r-labs.org/projects/rp-admin-reports/wiki/AdministrationReportsEn'

  settings(:default => ReportItem.hide_map(),
           :partial => 'settings/reports_settings')
  menu :top_menu, :admin_reports, { :controller => 'admin_reports', :action => 'info' },
                                                         :if => Proc.new { User.current.logged? }

end


Redmine::MenuManager.map :admin_reports_menu do |menu|
  ReportItem.push_menu(menu, :permissions, :label_permissions_report, 
                       :html => {:class => 'roles'})
  ReportItem.push_menu(menu, :workflows, :label_workflow)
                                    
  ReportItem.push_menu(menu, :settings)
  ReportItem.push_menu(menu, :plugins)
  ReportItem.push_menu(menu, :info, :label_information_plural, :last => true)
end
