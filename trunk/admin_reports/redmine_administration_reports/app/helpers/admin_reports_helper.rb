module AdminReportsHelper

  def is_shown?(item)
    return !Settings.plugin_redmine_administration_reports['hide_' + item];
  end
  
end
