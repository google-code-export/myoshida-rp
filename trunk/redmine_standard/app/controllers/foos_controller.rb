class FoosController < ApplicationController
  unloadable
  menu_item :standard
  before_filter :find_project, :authorize
  before_filter :find_foo, :except => [:index, :new]

  layout 'standard'

  def index
    @foos = Foo.find(:all, :conditions => ["project_id = #{@project.id} "])
  end


  def new
    @foo = Foo.new(params[:foo])
    @foo.project_id = @project.id

    if request.post? and @foo.save	
      flash[:notice] = l(:notice_successful_create)
      redirect_to :action => 'show', :id => @project, :foo_id => @foo.id
    end
  end


  def show
  end


  def edit
    if request.post?
      @foo.attributes = params[:foo]
      if @foo.save
        flash[:notice] = l(:notice_successful_update)
        redirect_to :action => 'show', :id => @project, :foo_id => @foo
      end
    end
  rescue ActiveRecord::StaleObjectError
    flash.now[:error] = l(:notice_locking_conflict)
  end


  def destroy
    @foo.destroy
    redirect_to :action => 'index', :id => @project
  end


private
  def find_project
    @project = Project.find(params[:id])
  rescue ActiveRecord::RecordNotFound
    render_404
  end

  def find_foo
    @foo = Foo.find_by_id(params[:foo_id])
    render_404	unless @foo
  end
end
