class AdminReportsController < ApplicationController
  unloadable

  layout 'redmine_info'
  
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


  def settings
    @commit_fix_status = IssueStatus.find_by_id(Setting[:commit_fix_status_id])
    unless (@commit_fix_status)
      @commit_fix_status = l(:label_no_change_option)
    end
    
    @commit_fix_done_ratio = Setting[:commit_fix_done_ratio]
    if (!@commit_fix_done_ratio or @commit_fix_done_ratio.empty?)
      @commit_fix_done_ratio = l(:label_no_change_option)
    end
  end
  

  def plugins
    @plugins = Redmine::Plugin.all
  end


  def show
    @icat = params[:id]
    case @icat
    when 'permissions'; permissions;
    when 'workflows'; workflows;
    when 'settings'; settings;
    when 'plugins'; plugins;
    else
      @db_adapter_name = ActiveRecord::Base.connection.adapter_name
    end
  end

end
