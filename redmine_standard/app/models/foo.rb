class Foo < ActiveRecord::Base
  unloadable

  belongs_to :project

  acts_as_attachable :delete_permission => :manage_foos
  
  validates_presence_of :subject
  validates_length_of :subject, :maximum => 255

  def project
    Project.find(:first, :conditions => "projects.id = #{project_id}")
  end
  
end
