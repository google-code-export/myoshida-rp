class InfoController < ApplicationController
  unloadable
  
  before_filter :require_login

  helper :info
  include InfoHelper


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

    if ((1 <= Redmine::VERSION::MAJOR && 2 <= Redmine::VERSION::MINOR) &&
        @tracker && @role && @statuses.any?)
      workflows = Workflow.all(:conditions => {:role_id => @role.id, :tracker_id => @tracker.id})
      @workflows = {}
      @workflows['always'] = workflows.select {|w| !w.author && !w.assignee}
      @workflows['author'] = workflows.select {|w| w.author}
      @workflows['assignee'] = workflows.select {|w| w.assignee}
    end

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
    when 'version'
      @db_adapter_name = ActiveRecord::Base.connection.adapter_name 
    end
  end

end
