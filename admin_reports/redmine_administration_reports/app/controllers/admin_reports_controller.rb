class AdminReportsController < ApplicationController
  unloadable

  layout 'admin_reports'
  
  before_filter :require_login


  def permissions
    @roles = Role.find(:all, :order => 'builtin, position')
    @permissions = Redmine::AccessControl.permissions.select { |p| !p.public? }
  end

  def workflows
    @roles = Role.find(:all, :order => 'builtin, position')
    @role = Role.find_by_id(params[:role_id])

    @trackers = Tracker.find(:all, :order => 'position')
    @tracker = Tracker.find_by_id(params[:tracker_id])    
    
    if @tracker && @tracker.issue_statuses.any?
      @statuses = @tracker.issue_statuses
    end
    @statuses ||= IssueStatus.find(:all, :order => 'position')
  end

  
  def plugins
    @plugins = Redmine::Plugin.all
  end

  def info
    @db_adapter_name = ActiveRecord::Base.connection.adapter_name
  end

end
