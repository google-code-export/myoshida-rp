require 'redmine'

Redmine::Plugin.register :redmine_information do
  name 'Redmine Information Plugin'
  author 'M. Yoshida'
  description 'This is a plugin for information of Redmine'
  version '0.1.0'
  url 'http://www.r-labs.org/projects/rp-admin-reports/wiki/RedmineInformationEn'

  settings(:default => InfoCategory.hide_map(),
           :partial => 'settings/info_settings')
  menu(:top_menu, :redmine_info,
       { :controller => 'info', :action => 'show', :id => :version },
       :if => Proc.new { User.current.logged? })

end


Redmine::MenuManager.map :redmine_info_menu do |menu|
  InfoCategory.push_menu(menu, :permissions, :label_permissions_report, 
                       :html => {:class => 'roles'})
  InfoCategory.push_menu(menu, :workflows, :label_workflow)
                                    
  InfoCategory.push_menu(menu, :settings)
  InfoCategory.push_menu(menu, :plugins)
  InfoCategory.push_menu(menu, :wiki_macros)
  InfoCategory.push_menu(menu, :rails_info)
  InfoCategory.push_menu(menu, :version, :label_information_plural,
                         {:last => true, :html=>{:class => 'info'}})
end
