class FoosController < ApplicationController
  unloadable
  menu_item :standard
  before_filter :find_project, :authorize
  before_filter :find_foo, :except => [:index, :new, :preview]

  helper :attachments

  layout 'standard'

  def index
    @foos = Foo.find(:all, :conditions => ["project_id = #{@project.id} "])
  end


  def new
    @foo = Foo.new(params[:foo])
    @foo.project_id = @project.id

    if request.post? and @foo.save
      Attachment.attach_files(@foo, params[:attachments])
      render_attachment_warning_if_needed(@foo)
      flash[:notice] = l(:notice_successful_create)
      redirect_to :action => 'show', :project_id => @project, :id => @foo.id
    end
  end


  def show
  end


  def edit
    if request.post?
      @foo.attributes = params[:foo]
      if @foo.save
        Attachment.attach_files(@foo, params[:attachments])
        render_attachment_warning_if_needed(@foo)
        flash[:notice] = l(:notice_successful_update)
        redirect_to :action => 'show', :project_id => @project, :id => @foo.id
      end
    end
  rescue ActiveRecord::StaleObjectError
    flash.now[:error] = l(:notice_locking_conflict)
  end


  def destroy
    @foo.destroy
    redirect_to :action => 'index', :project_id => @project
  end


  def preview
    @text = params[:foo][:description]
    render :partial => 'common/preview'
  end


private
  def find_project
    @project = Project.find(params[:project_id])
  rescue ActiveRecord::RecordNotFound
    render_404
  end

  def find_foo
    @foo = Foo.find_by_id(params[:id])
    render_404	unless @foo
  end
end
