class Foo < ActiveRecord::Base
  unloadable

  validates_presence_of :subject
  validates_length_of :subject, :maximum => 255

end
