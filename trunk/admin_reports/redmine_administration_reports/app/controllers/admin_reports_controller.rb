class AdminReportsController < ApplicationController
  unloadable

  layout 'admin_reports'
  
  before_filter :require_login


  def permissions
    @roles = Role.find(:all, :order => 'builtin, position')
    @permissions = Redmine::AccessControl.permissions.select { |p| !p.public? }
  end

  def plugins
    @plugins = Redmine::Plugin.all
  end

  def info
    @db_adapter_name = ActiveRecord::Base.connection.adapter_name
  end

end
